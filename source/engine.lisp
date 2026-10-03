(in-package #:clifff)

;;;; -- Search Engine --

(defclass engine ()
  ((handle
    :initform nil
    :accessor engine--handle
    :type t
    :documentation "The opaque live fff instance pointer, or NIL.")
   (library-path
    :initarg :library-path
    :reader engine--library-path
    :type pathname
    :documentation "The fff C library loaded by this process.")
   (base-path
    :initarg :base-path
    :reader engine-base-path
    :type pathname
    :documentation "The canonical directory indexed by this engine.")
   (cache-directory
    :initarg :cache-directory
    :reader engine--cache-directory
    :type pathname
    :documentation "The private directory containing fff databases.")
   (scan-timeout-milliseconds
    :initarg :scan-timeout-milliseconds
    :reader engine--scan-timeout-milliseconds
    :type (integer 1)
    :documentation "The initial indexing deadline in milliseconds.")
   (lock
    :initform (make-lock "clifff engine")
    :reader engine--lock
    :type t
    :documentation "Serialize lifecycle and query calls."))
  (:documentation "One lazily initialized, watched fff workspace index."))

(defun make-engine
    (&key library-path base-path cache-directory
          (scan-timeout-milliseconds 30000))
  "Create a lazy fff engine for BASE-PATH using private CACHE-DIRECTORY."
  (unless (pathnamep library-path)
    (clifff--fail ':create "LIBRARY-PATH must be a pathname."))
  (unless (and (pathnamep base-path) (probe-file base-path))
    (clifff--fail ':create "BASE-PATH must name an existing directory."
                  :pathname base-path))
  (unless (pathnamep cache-directory)
    (clifff--fail ':create "CACHE-DIRECTORY must be a pathname."))
  (unless (typep scan-timeout-milliseconds '(integer 1))
    (clifff--fail ':create "SCAN-TIMEOUT-MILLISECONDS must be positive."))
  (make-instance 'engine
                 :library-path library-path
                 :base-path (uiop:ensure-directory-pathname
                             (truename base-path))
                 :cache-directory (uiop:ensure-directory-pathname
                                   cache-directory)
                 :scan-timeout-milliseconds scan-timeout-milliseconds))

(defun engine--initialize-create-options
    (options &key base-path frecency-path history-path)
  "Initialize foreign OPTIONS for one watched AI-oriented workspace index."
  (dotimes (index (foreign-type-size '(:struct fff-create-options)))
    (setf (mem-aref options :uint8 index) 0))
  (setf (foreign-slot-value options '(:struct fff-create-options) 'version)
        +create-options-version+
        (foreign-slot-value options '(:struct fff-create-options) 'base-path)
        base-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'frecency-db-path)
        frecency-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'history-db-path)
        history-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'enable-mmap-cache)
        1
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'enable-content-indexing)
        1
        (foreign-slot-value options '(:struct fff-create-options) 'watch)
        1
        (foreign-slot-value options '(:struct fff-create-options) 'ai-mode)
        1
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'log-file-path)
        (null-pointer)
        (foreign-slot-value options '(:struct fff-create-options) 'log-level)
        (null-pointer))
  nil)

(defun engine--close-unlocked (engine)
  "Destroy ENGINE's native instance while its lock is held."
  (let ((handle (engine--handle engine)))
    (when (and handle (not (null-pointer-p handle)))
      (fff--destroy handle)))
  (setf (engine--handle engine) nil)
  nil)

(defun engine-close (engine)
  "Destroy ENGINE's native instance and filesystem watcher."
  (with-lock-held ((engine--lock engine))
    (engine--close-unlocked engine)))

(defun engine-detach (engine)
  "Forget ENGINE's inherited pointer without touching native state after a fork."
  (setf (engine--handle engine) nil)
  nil)

(defun engine--ensure-unlocked (engine)
  "Return ENGINE's initialized native handle while its lock is held."
  (clifff--load-library (engine--library-path engine))
  (unless (engine--handle engine)
    (let* ((cache-directory (engine--cache-directory engine))
           (frecency-path (merge-pathnames "frecency" cache-directory))
           (history-path (merge-pathnames "history" cache-directory))
           (base-path (engine-base-path engine)))
      (ensure-directories-exist cache-directory)
      (with-foreign-string (base-pointer (namestring base-path))
        (with-foreign-string (frecency-pointer (namestring frecency-path))
          (with-foreign-string (history-pointer (namestring history-path))
            (with-foreign-object (options '(:struct fff-create-options))
              (engine--initialize-create-options
               options
               :base-path base-pointer
               :frecency-path frecency-pointer
               :history-path history-pointer)
              (setf (engine--handle engine)
                    (clifff--take-handle-result
                     (fff--create-instance-with options)
                     ':initialize
                     :pathname base-path))))))
      (unless (plusp
               (clifff--take-integer-result
                (fff--wait-for-scan
                 (engine--handle engine)
                 (engine--scan-timeout-milliseconds engine))
                ':scan
                :pathname base-path))
        (engine--close-unlocked engine)
        (clifff--fail
         ':scan
         (format nil "fff did not finish indexing within ~D milliseconds."
                 (engine--scan-timeout-milliseconds engine))
         :pathname base-path))))
  (engine--handle engine))


;;;; -- Result Decoding --

(defun engine--file-item (item)
  "Copy one foreign fff file ITEM into a readable property list."
  (list :path (or (clifff--foreign-string
                   (fff--file-item-relative-path item))
                  "<unknown>")
        :git-status (or (clifff--foreign-string
                         (fff--file-item-git-status item))
                        "")
        :size (fff--file-item-size item)
        :frecency (fff--file-item-frecency item)
        :binary-p (not (zerop (fff--file-item-binary-p item)))))

(defun engine--file-result (result page page-size)
  "Copy foreign file RESULT into readable Common Lisp data."
  (let* ((count (fff--search-result-count result))
         (total-matched (fff--search-result-total-matched result)))
    (list :kind ':files
          :items (loop for index below count
                       collect (engine--file-item
                                (fff--search-result-item result index)))
          :count count
          :total-matched total-matched
          :total-files (fff--search-result-total-files result)
          :page page
          :page-size page-size
          :next-page (and (< (* (1+ page) page-size) total-matched)
                          (1+ page)))))

(defun engine--context-lines (match direction)
  "Copy MATCH context lines in DIRECTION into a list of strings."
  (let ((count (ecase direction
                 (:before (fff--grep-match-context-before-count match))
                 (:after (fff--grep-match-context-after-count match)))))
    (loop for index below count
          collect
          (or (clifff--foreign-string
               (ecase direction
                 (:before (fff--grep-match-context-before match index))
                 (:after (fff--grep-match-context-after match index))))
              ""))))

(defun engine--grep-match (match)
  "Copy one foreign fff grep MATCH into a readable property list."
  (list :path (or (clifff--foreign-string
                   (fff--grep-match-relative-path match))
                  "<unknown>")
        :git-status (or (clifff--foreign-string
                         (fff--grep-match-git-status match))
                        "")
        :line-content (or (clifff--foreign-string
                           (fff--grep-match-line-content match))
                          "")
        :line-number (fff--grep-match-line-number match)
        :column (fff--grep-match-column match)
        :context-before (engine--context-lines match ':before)
        :context-after (engine--context-lines match ':after)
        :fuzzy-score
        (and (not (zerop (fff--grep-match-has-fuzzy-score-p match)))
             (fff--grep-match-fuzzy-score match))
        :definition-p (not (zerop (fff--grep-match-definition-p match)))
        :binary-p (not (zerop (fff--grep-match-binary-p match)))))

(defun engine--grep-result (result)
  "Copy foreign content RESULT into readable Common Lisp data."
  (let ((count (fff--grep-result-count result)))
    (list :kind ':content
          :matches (loop for index below count
                         collect (engine--grep-match
                                  (fff--grep-result-match result index)))
          :count count
          :searched (fff--grep-result-total-files-searched result)
          :eligible (fff--grep-result-filtered-file-count result)
          :total-files (fff--grep-result-total-files result)
          :next-file-offset (fff--grep-result-next-file-offset result)
          :regex-fallback-error
          (clifff--foreign-string
           (fff--grep-result-regex-fallback-error result)))))


;;;; -- Result Presentation --

(defun clifff--annotation
    (git-status frecency binary-p &key definition-p fuzzy-score)
  "Return compact metadata annotations for one copied result."
  (format nil "~@[ [git:~A]~]~:[~; [definition]~]~:[~; [binary]~]~@[ [fuzzy:~D]~]~:[~; [frecency:~D]~]"
          (and (plusp (length git-status)) git-status)
          definition-p
          binary-p
          fuzzy-score
          (not (zerop frecency))
          frecency))

(defun render-file-result (result)
  "Render one copied file or glob RESULT as compact text."
  (with-output-to-string (stream)
    (format stream "~:D result~:P shown, ~:D matched, ~:D indexed; page ~D.~%"
            (getf result :count)
            (getf result :total-matched)
            (getf result :total-files)
            (getf result :page))
    (dolist (item (getf result :items))
      (format stream "~A~A [~:D bytes]~%"
              (getf item :path)
              (clifff--annotation (getf item :git-status)
                                  (getf item :frecency)
                                  (getf item :binary-p))
              (getf item :size)))
    (when (getf result :next-page)
      (format stream "next-page: ~D~%" (getf result :next-page)))))

(defun clifff--render-context-lines (stream match)
  "Render MATCH's context lines to STREAM."
  (let* ((line-number (getf match :line-number))
         (before (getf match :context-before)))
    (loop for line in before
          for number from (- line-number (length before))
          do (format stream "  | ~D  ~A~%" number line))
    (format stream "  > ~D  ~A~%" line-number (getf match :line-content))
    (loop for line in (getf match :context-after)
          for number from (1+ line-number)
          do (format stream "  | ~D  ~A~%" number line))))

(defun render-content-result (result)
  "Render one copied content RESULT as compact text."
  (with-output-to-string (stream)
    (format stream "~:D match~:P; searched ~:D of ~:D eligible files, ~:D indexed.~%"
            (getf result :count)
            (getf result :searched)
            (getf result :eligible)
            (getf result :total-files))
    (when (getf result :regex-fallback-error)
      (format stream "regex fallback: ~A~%"
              (getf result :regex-fallback-error)))
    (dolist (match (getf result :matches))
      (format stream "~A:~D:~D~A~%"
              (getf match :path)
              (getf match :line-number)
              (getf match :column)
              (clifff--annotation (getf match :git-status)
                                  0
                                  (getf match :binary-p)
                                  :definition-p (getf match :definition-p)
                                  :fuzzy-score (getf match :fuzzy-score)))
      (clifff--render-context-lines stream match))
    (when (plusp (getf result :next-file-offset))
      (format stream "next-file-offset: ~D~%"
              (getf result :next-file-offset)))))


;;;; -- Native Search Operations --

(defun engine--bounded-unsigned (value name maximum &key (minimum 0))
  "Return VALUE after validating its unsigned foreign integer range."
  (unless (and (integerp value) (<= minimum value maximum))
    (clifff--fail
     ':arguments
     (format nil "~A must be an integer between ~D and ~D."
             name minimum maximum)))
  value)

(defun engine-search-files
    (engine query &key glob-p (page 0) (page-size 20))
  "Search ENGINE paths for QUERY and return one readable result page."
  (unless (stringp query)
    (clifff--fail ':arguments "QUERY must be a string."))
  (engine--bounded-unsigned page "PAGE" #xffffffff)
  (engine--bounded-unsigned page-size "PAGE-SIZE" #xffffffff :minimum 1)
  (with-lock-held ((engine--lock engine))
    (let ((handle (engine--ensure-unlocked engine)))
      (with-foreign-string (query-pointer query)
        (let* ((result
                 (if glob-p
                     (fff--glob handle query-pointer (null-pointer)
                                0 page page-size)
                     (fff--search handle query-pointer (null-pointer)
                                  0 page page-size 100 3)))
               (payload
                 (clifff--take-handle-result
                  result ':files :pathname (engine-base-path engine))))
          (unwind-protect
               (engine--file-result payload page page-size)
            (fff--free-search-result payload)))))))

(defun engine--grep-mode (mode)
  "Return fff's numeric content-search mode for keyword MODE."
  (ecase mode
    (:plain 0)
    (:regex 1)
    (:fuzzy 2)))

(defun engine-search-content
    (engine query
     &key (mode ':plain) (file-offset 0) (maximum-results 20)
       (maximum-matches-per-file 20) (time-budget-milliseconds 3000)
       (context-lines 0) (maximum-file-size (* 10 1024 1024)))
  "Search ENGINE contents for QUERY and return one readable result page.

TIME-BUDGET-MILLISECONDS bounds every search, including one that finds nothing;
the page's :NEXT-FILE-OFFSET then resumes at the first unsearched file."
  (unless (stringp query)
    (clifff--fail ':arguments "QUERY must be a string."))
  (engine--bounded-unsigned file-offset "FILE-OFFSET" #xffffffff)
  (engine--bounded-unsigned maximum-results "MAXIMUM-RESULTS" #xffffffff
                            :minimum 1)
  (engine--bounded-unsigned maximum-matches-per-file
                            "MAXIMUM-MATCHES-PER-FILE" #xffffffff
                            :minimum 1)
  (engine--bounded-unsigned time-budget-milliseconds
                            "TIME-BUDGET-MILLISECONDS" #xffffffffffffffff
                            :minimum 1)
  (engine--bounded-unsigned context-lines "CONTEXT-LINES" #xffffffff)
  (engine--bounded-unsigned maximum-file-size
                            "MAXIMUM-FILE-SIZE" #xffffffffffffffff
                            :minimum 1)
  (with-lock-held ((engine--lock engine))
    (let ((handle (engine--ensure-unlocked engine)))
      (with-foreign-string (query-pointer query)
        (let* ((result
                 (fff--live-grep handle
                                 query-pointer
                                 (handler-case
                                     (engine--grep-mode mode)
                                   (type-error ()
                                     (clifff--fail
                                      ':arguments
                                      "MODE must be :PLAIN, :REGEX, or :FUZZY.")))
                                 maximum-file-size
                                 maximum-matches-per-file
                                 1
                                 file-offset
                                 maximum-results
                                 time-budget-milliseconds
                                 1
                                 context-lines
                                 context-lines
                                 1))
               (payload
                 (clifff--take-handle-result
                  result ':content :pathname (engine-base-path engine))))
          (unwind-protect
               (engine--grep-result payload)
            (fff--free-grep-result payload)))))))

(defun engine-search-multi-content
    (engine patterns
     &key (constraints "") (file-offset 0) (maximum-results 20)
       (maximum-matches-per-file 20) (time-budget-milliseconds 3000)
       (context-lines 0) (maximum-file-size (* 10 1024 1024)))
  "Search ENGINE for lines matching any literal PATTERNS under CONSTRAINTS.

The time budget is enforced as in ENGINE-SEARCH-CONTENT."
  (unless (and (listp patterns)
               patterns
               (every (lambda (pattern)
                        (and (stringp pattern)
                             (plusp (length pattern))
                             (not (find #\Newline pattern))))
                      patterns))
    (clifff--fail
     ':arguments
     "PATTERNS must contain non-empty strings without newlines."))
  (unless (stringp constraints)
    (clifff--fail ':arguments "CONSTRAINTS must be a string."))
  (engine--bounded-unsigned file-offset "FILE-OFFSET" #xffffffff)
  (engine--bounded-unsigned maximum-results "MAXIMUM-RESULTS" #xffffffff
                            :minimum 1)
  (engine--bounded-unsigned maximum-matches-per-file
                            "MAXIMUM-MATCHES-PER-FILE" #xffffffff
                            :minimum 1)
  (engine--bounded-unsigned time-budget-milliseconds
                            "TIME-BUDGET-MILLISECONDS" #xffffffffffffffff
                            :minimum 1)
  (engine--bounded-unsigned context-lines "CONTEXT-LINES" #xffffffff)
  (engine--bounded-unsigned maximum-file-size
                            "MAXIMUM-FILE-SIZE" #xffffffffffffffff
                            :minimum 1)
  (with-lock-held ((engine--lock engine))
    (let ((handle (engine--ensure-unlocked engine))
          (joined (format nil "~{~A~^~%~}" patterns)))
      (with-foreign-string (patterns-pointer joined)
        (with-foreign-string (constraints-pointer constraints)
          (let* ((result
                   (fff--multi-grep handle
                                    patterns-pointer
                                    constraints-pointer
                                    maximum-file-size
                                    maximum-matches-per-file
                                    1
                                    file-offset
                                    maximum-results
                                    time-budget-milliseconds
                                    1
                                    context-lines
                                    context-lines
                                    1))
                 (payload
                   (clifff--take-handle-result
                    result ':multi-content
                    :pathname (engine-base-path engine))))
            (unwind-protect
                 (engine--grep-result payload)
              (fff--free-grep-result payload))))))))
