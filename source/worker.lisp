(in-package #:clifff)

;;;; -- Worker Protocol --

(defconstant +worker-protocol-version+ 1
  "The private protocol shared by clifff worker processes.")

(defun worker--write-form (stream form)
  "Write one portable protocol FORM to STREAM and flush it."
  (let ((*print-circle* nil)
        (*print-length* nil)
        (*print-level* nil)
        (*print-pretty* nil)
        (*print-readably* t))
    (write form :stream stream)
    (terpri stream)
    (finish-output stream))
  nil)

(defun worker--read-form (stream timeout-seconds)
  "Read one portable form from STREAM within TIMEOUT-SECONDS."
  (let ((*read-eval* nil)
        (end-marker (gensym "CLIFFF-WORKER-END-")))
    (let ((form (with-timeout (timeout-seconds)
                  (read stream nil end-marker))))
      (when (eq form end-marker)
        (error "The clifff helper closed its response stream."))
      form)))

(defun worker--response-p (value request-id)
  "Return true when VALUE is a complete response for REQUEST-ID."
  (and (listp value)
       (eq (first value) ':clifff-response)
       (= (or (getf (rest value) :version) 0)
          +worker-protocol-version+)
       (= (or (getf (rest value) :id) 0) request-id)
       (member (getf (rest value) :status) '(:ok :error))
       (stringp (getf (rest value) :content))
       t))


;;;; -- Parent-Side Lifecycle --

(defclass worker ()
  ((command
    :initarg :command
    :reader worker--command
    :type list
    :documentation "The argv list starting a process that calls WORKER-MAIN.")
   (log-pathname
    :initarg :log-pathname
    :initform nil
    :accessor worker--log-pathname
    :type (or null pathname)
    :documentation "The private file receiving helper diagnostics, or NIL before configuration.")
   (start-timeout-seconds
    :initarg :start-timeout-seconds
    :reader worker--start-timeout-seconds
    :type (integer 1)
    :documentation "The maximum helper startup duration.")
   (request-timeout-seconds
    :initarg :request-timeout-seconds
    :reader worker--request-timeout-seconds
    :type (integer 1)
    :documentation "The maximum duration of one complete request.")
   (process
    :initform nil
    :accessor worker-process
    :type t
    :documentation "The live helper process, or NIL.")
   (input
    :initform nil
    :accessor worker--input
    :type t
    :documentation "The request stream connected to the helper.")
   (output
    :initform nil
    :accessor worker--output
    :type t
    :documentation "The response stream connected to the helper.")
   (next-request-id
    :initform 1
    :accessor worker--next-request-id
    :type (integer 1)
    :documentation "The next request identifier sent to the helper.")
   (lock
    :initform (make-lock "clifff worker")
    :reader worker--lock
    :type t
    :documentation "Serialize helper lifecycle and request exchange."))
  (:documentation "A restartable process containing a native fff index."))

(defun make-worker
    (&key command log-pathname
          (start-timeout-seconds 30)
          (request-timeout-seconds 45))
  "Create a lazy supervised fff worker started with COMMAND."
  (unless (and (listp command)
               command
               (every #'stringp command))
    (clifff--fail ':worker "COMMAND must be a non-empty list of strings."))
  (unless (or (null log-pathname) (pathnamep log-pathname))
    (clifff--fail ':worker "LOG-PATHNAME must be NIL or a pathname."))
  (unless (typep start-timeout-seconds '(integer 1))
    (clifff--fail ':worker "START-TIMEOUT-SECONDS must be positive."))
  (unless (typep request-timeout-seconds '(integer 1))
    (clifff--fail ':worker "REQUEST-TIMEOUT-SECONDS must be positive."))
  (make-instance 'worker
                 :command command
                 :log-pathname log-pathname
                 :start-timeout-seconds start-timeout-seconds
                 :request-timeout-seconds request-timeout-seconds))

(defun worker--alive-p (worker)
  "Return true when WORKER has a live process and open protocol streams."
  (let ((process (worker-process worker)))
    (and process
         (uiop:process-alive-p process)
         (open-stream-p (worker--input worker))
         (open-stream-p (worker--output worker))
         t)))

(defun worker--detach-unlocked (worker)
  "Close inherited streams and forget WORKER without signaling its process."
  (dolist (stream (list (worker--input worker) (worker--output worker)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  (setf (worker-process worker) nil
        (worker--input worker) nil
        (worker--output worker) nil
        (worker--next-request-id worker) 1)
  nil)

(defun worker--close-unlocked (worker)
  "Stop and forget WORKER while its lifecycle lock is held."
  (let ((process (worker-process worker))
        (input (worker--input worker)))
    (when (and process (uiop:process-alive-p process))
      (when (and input (open-stream-p input))
        (ignore-errors
          (worker--write-form
           input
           (list :clifff-shutdown :version +worker-protocol-version+))))
      (loop repeat 20
            while (uiop:process-alive-p process)
            do (sleep 0.01))
      (when (uiop:process-alive-p process)
        (ignore-errors (uiop:terminate-process process :urgent t))))
    (when process
      (ignore-errors (uiop:wait-process process))))
  (worker--detach-unlocked worker))

(defun worker-close (worker)
  "Stop WORKER and its isolated native fff index."
  (with-lock-held ((worker--lock worker))
    (worker--close-unlocked worker)))

(defun worker-detach (worker)
  "Forget inherited WORKER streams without affecting the parent process."
  (with-lock-held ((worker--lock worker))
    (worker--detach-unlocked worker)))

(defun worker--start-unlocked (worker)
  "Start WORKER and validate its protocol handshake."
  (let ((log-pathname (worker--log-pathname worker)))
    (unless log-pathname
      (clifff--fail ':worker
                    "The worker needs a diagnostic LOG-PATHNAME before startup."))
    (ensure-directories-exist log-pathname)
    (with-open-file (stream log-pathname
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (finish-output stream))
    (worker--close-unlocked worker)
    (let ((process
            (uiop:launch-program
             (worker--command worker)
             :input :stream
             :output :stream
             :error-output log-pathname
             :if-error-output-exists :append
             :wait nil)))
      (setf (worker-process worker) process
            (worker--input worker) (uiop:process-info-input process)
            (worker--output worker) (uiop:process-info-output process)
            (worker--next-request-id worker) 1)
      (handler-case
          (let ((handshake
                  (worker--read-form
                   (worker--output worker)
                   (worker--start-timeout-seconds worker))))
            (unless (equal handshake
                           (list :clifff-worker
                                 :version +worker-protocol-version+))
              (error "Invalid clifff helper handshake: ~S" handshake)))
        (error (cause)
          (worker--close-unlocked worker)
          (clifff--fail
           ':worker
           (format nil "Could not start the clifff helper: ~A" cause)
           :pathname (first (worker--command worker))
           :cause cause)))))
  worker)

(defun worker--ensure-unlocked (worker)
  "Return a live WORKER, starting it when necessary."
  (unless (worker--alive-p worker)
    (worker--start-unlocked worker))
  worker)

(defun worker--request-form
    (request-id library-path base-path cache-directory operation arguments)
  "Return one complete helper request for OPERATION and ARGUMENTS."
  (list :clifff-request
        :version +worker-protocol-version+
        :id request-id
        :library-path (namestring library-path)
        :base-path (namestring base-path)
        :cache-directory (namestring cache-directory)
        :operation operation
        :arguments arguments))

(defun worker--exchange
    (worker library-path base-path cache-directory operation arguments)
  "Exchange one request, returning content and an optional remote error."
  (worker--ensure-unlocked worker)
  (let ((request-id (worker--next-request-id worker)))
    (incf (worker--next-request-id worker))
    (worker--write-form
     (worker--input worker)
     (worker--request-form request-id library-path base-path cache-directory
                           operation arguments))
    (let ((response
            (worker--read-form
             (worker--output worker)
             (worker--request-timeout-seconds worker))))
      (unless (worker--response-p response request-id)
        (error "Invalid clifff helper response: ~S" response))
      (if (eq (getf (rest response) :status) ':ok)
          (values (getf (rest response) :content) nil)
          (values nil (getf (rest response) :content))))))

(defun worker--reset-databases (cache-directory)
  "Discard the rebuildable fff ranking and history databases."
  (dolist (name '("frecency/" "history/"))
    (uiop:delete-directory-tree (merge-pathnames name cache-directory)
                                :validate t
                                :if-does-not-exist :ignore))
  nil)

(defun worker-request
    (worker &key library-path base-path cache-directory log-pathname
      operation arguments)
  "Run one search, resetting fff databases before one transport retry."
  (unless (and (pathnamep library-path) (probe-file library-path))
    (clifff--fail ':worker "LIBRARY-PATH must name the fff C library."
                  :pathname library-path))
  (unless (and (pathnamep base-path) (probe-file base-path))
    (clifff--fail ':worker "BASE-PATH must name an existing directory."
                  :pathname base-path))
  (unless (pathnamep cache-directory)
    (clifff--fail ':worker "CACHE-DIRECTORY must be a pathname."))
  (unless (pathnamep log-pathname)
    (clifff--fail ':worker "LOG-PATHNAME must be a pathname."))
  (unless (keywordp operation)
    (clifff--fail ':worker "OPERATION must be a keyword."))
  (unless (listp arguments)
    (clifff--fail ':worker "ARGUMENTS must be a list."))
  (let ((library-path (truename library-path))
        (base-path (uiop:ensure-directory-pathname (truename base-path)))
        (cache-directory (uiop:ensure-directory-pathname cache-directory))
        (log-pathname log-pathname))
    (with-lock-held ((worker--lock worker))
      (unless (equal log-pathname (worker--log-pathname worker))
        (worker--close-unlocked worker)
        (setf (worker--log-pathname worker) log-pathname))
      (when (and (worker-process worker)
                 (not (worker--alive-p worker)))
        (worker--close-unlocked worker)
        (worker--reset-databases cache-directory))
      (loop for attempt from 1 to 2
            do (handler-case
                   (multiple-value-bind (content remote-error)
                       (worker--exchange worker library-path base-path
                                         cache-directory operation arguments)
                     (when remote-error
                       (clifff--fail operation remote-error
                                     :pathname base-path
                                     :cause ':remote))
                     (return content))
                 (clifff-error (condition)
                   (error condition))
                 (error (cause)
                   (worker--close-unlocked worker)
                   (if (= attempt 1)
                       (worker--reset-databases cache-directory)
                       (clifff--fail
                        operation
                        (format nil
                                "The isolated fff helper failed twice: ~A Diagnostic log: ~A"
                                cause
                                (worker--log-pathname worker))
                        :pathname base-path
                        :cause cause))))))))


(defun worker-file-count
    (worker glob &key library-path base-path cache-directory log-pathname)
  "Return how many indexed files under BASE-PATH match GLOB, through WORKER.

The other arguments are those of WORKER-REQUEST. Hosts use the count to check
that a path filter names existing files before searching under it."
  (parse-integer
   (worker-request worker
                   :library-path library-path
                   :base-path base-path
                   :cache-directory cache-directory
                   :log-pathname log-pathname
                   :operation ':file-count
                   :arguments (list glob))))


;;;; -- Child-Side Dispatch --

(defun worker--request-p (request)
  "Return true when REQUEST has the complete clifff protocol shape."
  (and (listp request)
       (eq (first request) ':clifff-request)
       (= (or (getf (rest request) :version) 0) +worker-protocol-version+)
       (integerp (getf (rest request) :id))
       (stringp (getf (rest request) :library-path))
       (stringp (getf (rest request) :base-path))
       (stringp (getf (rest request) :cache-directory))
       (keywordp (getf (rest request) :operation))
       (listp (getf (rest request) :arguments))
       t))

(defun worker--engine-signature (request)
  "Return the engine configuration identity carried by REQUEST."
  (list (getf (rest request) :library-path)
        (getf (rest request) :base-path)
        (getf (rest request) :cache-directory)))

(defun worker--make-engine (request)
  "Construct an engine from one validated worker REQUEST."
  (make-engine
   :library-path (pathname (getf (rest request) :library-path))
   :base-path (pathname (getf (rest request) :base-path))
   :cache-directory (pathname (getf (rest request) :cache-directory))))

(defun worker--dispatch (engine request)
  "Execute one validated worker REQUEST with ENGINE."
  (let ((arguments (getf (rest request) :arguments)))
    (case (getf (rest request) :operation)
      (:files
       (render-file-result
        (apply #'engine-search-files engine arguments)))
      (:content
       (render-content-result
        (apply #'engine-search-content engine arguments)))
      (:multi-content
       (render-content-result
        (apply #'engine-search-multi-content engine arguments)))
      (:file-count
       (princ-to-string
        (getf (engine-search-files engine (first arguments) :glob-p t :page-size 1)
              :total-matched)))
      (otherwise
       (error "Unknown clifff worker operation ~S."
              (getf (rest request) :operation))))))

(defun worker--error-text (condition)
  "Return bounded printable text for one worker CONDITION."
  (let ((text (princ-to-string condition)))
    (subseq text 0 (min (length text) 2000))))

(defun worker-main (&optional (protocol-output *standard-output*))
  "Serve fff requests on standard input and PROTOCOL-OUTPUT.
Native helpers run with masked floating-point traps as C and Rust expect."
  #+sbcl (sb-ext:disable-debugger)
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (let ((engine nil)
        (signature nil)
        (*read-eval* nil))
    (unwind-protect
         (progn
           (worker--write-form
            protocol-output
            (list :clifff-worker :version +worker-protocol-version+))
           (loop for request = (read *standard-input* nil nil)
                 while request
                 do (when (and (listp request)
                               (eq (first request) ':clifff-shutdown)
                               (= (or (getf (rest request) :version) 0)
                                  +worker-protocol-version+))
                      (return))
                    (let ((request-id (and (listp request)
                                           (getf (rest request) :id))))
                      (handler-case
                          (progn
                            (unless (worker--request-p request)
                              (error "Invalid clifff worker request."))
                            (let ((new-signature
                                    (worker--engine-signature request)))
                              (unless (equal new-signature signature)
                                (when engine
                                  (engine-close engine))
                                (setf engine (worker--make-engine request)
                                      signature new-signature)))
                            (worker--write-form
                             protocol-output
                             (list :clifff-response
                                   :version +worker-protocol-version+
                                   :id request-id
                                   :status ':ok
                                   :content (worker--dispatch engine request))))
                        (serious-condition (condition)
                          (worker--write-form
                           protocol-output
                           (list :clifff-response
                                 :version +worker-protocol-version+
                                 :id request-id
                                 :status ':error
                                 :content
                                 (worker--error-text condition))))))))
      (when engine
        (ignore-errors (engine-close engine)))))
  nil)
