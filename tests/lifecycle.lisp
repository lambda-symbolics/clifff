(in-package #:clifff/tests)

(defun tests--call-with-functions (replacements function)
  "Call FUNCTION with temporary global function REPLACEMENTS in this serial suite."
  (let ((saved (mapcar (lambda (entry) (cons (first entry) (fdefinition (first entry))))
                       replacements)))
    (unwind-protect
         (progn
           (dolist (entry replacements) (setf (fdefinition (first entry)) (rest entry)))
           (funcall function))
      (dolist (entry saved) (setf (fdefinition (first entry)) (rest entry))))))

(defun tests--input-validation ()
  "Reject invalid C strings and literal collections before native library loading."
  (let* ((root     (uiop:temporary-directory))
         (engine   (make-engine :library-path #P"/missing/clifff-library"
                                :base-path root :cache-directory root))
         (nul      (string #\Null))
         (circular (list "literal")))
    (setf (rest circular) circular)
    (dolist (patterns (list nil #() '("valid" . "tail") circular '("") '(42)
                            (list (format nil "two~%lines")) (list (string #\Return))
                            (list nul)))
      (test-assert (signals clifff-error (clifff:validate-literal-patterns patterns))
                   "invalid literal collections signal an argument failure"))
    (test-assert (equal (clifff:validate-literal-patterns #("λ" "literal")) '("λ" "literal"))
                 "a vector of Unicode literals is normalized to a proper list")
    (dolist (call (list (lambda () (engine-search-files engine nul))
                        (lambda () (engine-search-files engine nul :glob-p t))
                        (lambda () (engine-search-content engine nul))
                        (lambda () (engine-search-multi-content engine (list nul)))
                        (lambda () (engine-search-multi-content engine '("x") :constraints nul))
                        (lambda () (clifff:engine-search-structured-content
                                    engine nul :constraints "missing.lisp"))
                        (lambda () (clifff:engine-search-structured-content
                                    engine "x" :constraints nul))
                        (lambda () (clifff:engine-search-structured-multi-content
                                    engine (list nul) :constraints "missing.lisp"))))
      (test-assert (handler-case (progn (funcall call) nil)
                     (clifff-error (condition)
                       (eq (clifff:clifff-error-operation condition) :arguments)))
                   "embedded NUL is rejected before a missing library can be loaded"))))

(defun tests--provisional-handle ()
  "Publish completed scans and reclaim failed provisional handles."
  (dolist (outcome '(:pending :failure 1))
    (let* ((root      (merge-pathnames (format nil "clifff-handle-~D/" (random most-positive-fixnum))
                                      (uiop:temporary-directory)))
           (engine    (make-engine :library-path #P"/missing/clifff-library"
                                   :base-path (uiop:temporary-directory) :cache-directory root))
           (handle    (cffi:make-pointer 123))
           (attempts  0)
           (destroyed nil))
      (unwind-protect
           (tests--call-with-functions
            (list (cons 'clifff::clifff--load-library (lambda (path) (declare (ignore path))))
                  (cons 'clifff::fff--create-instance-with (lambda (options) (declare (ignore options)) handle))
                  (cons 'clifff::clifff--take-handle-result
                        (lambda (result operation &key pathname)
                          (declare (ignore operation pathname)) result))
                  (cons 'clifff::fff--wait-for-scan
                        (lambda (pointer timeout)
                          (declare (ignore pointer))
                          (test-assert (zerop timeout) "scan readiness uses a nonblocking poll")
                          (incf attempts)
                          (cond
                            ((eq outcome :failure)
                             (error "Failed wait fixture"))
                            ((and (eq outcome :pending) (< attempts 3))
                             0)
                            (t
                             1))))
                  (cons 'clifff::clifff--take-integer-result
                        (lambda (result operation &key pathname)
                          (declare (ignore operation pathname)) result))
                  (cons 'clifff::fff--destroy (lambda (pointer) (push pointer destroyed))))
            (lambda ()
              (if (not (eq outcome :failure))
                  (progn
                    (test-assert (eq (clifff::engine--ensure-unlocked engine) handle)
                                 "a successful scan publishes the native handle")
                    (test-assert (null destroyed) "a published handle is not destroyed provisionally")
                    (test-assert (= attempts (if (eq outcome :pending) 3 1))
                                 "incomplete scans are polled until completion")
                    (engine-close engine))
                  (progn
                    (test-assert (signals error (clifff::engine--ensure-unlocked engine))
                                 "a failed scan signals its condition")
                    (test-assert (null (clifff::engine--handle engine))
                                 "a failed scan does not publish its provisional handle")))
              (test-assert (equal destroyed (list handle)) "the native handle is destroyed exactly once")))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(defun tests--search-without-deadline ()
  "Exercise both content-search entry points without a native time budget."
  (let* ((root   (uiop:temporary-directory))
         (engine (make-engine :library-path #P"/missing/clifff-library"
                              :base-path root :cache-directory root))
         (calls  0))
    (flet ((search-call (&rest arguments)
             (incf calls)
             (test-assert (zerop (nth 8 arguments)) "content search has no native time budget")
             (test-assert (zerop (nth 9 arguments)) "native budget enforcement is disabled")
             (cffi:make-pointer 123)))
      (tests--call-with-functions
       (list (cons 'clifff::engine--ensure-unlocked (lambda (value) (declare (ignore value)) nil))
             (cons 'clifff::fff--live-grep #'search-call)
             (cons 'clifff::fff--multi-grep #'search-call)
             (cons 'clifff::clifff--take-handle-result
                   (lambda (result operation &key pathname)
                     (declare (ignore operation pathname)) result))
             (cons 'clifff::engine--grep-result
                   (lambda (result maximum) (declare (ignore result maximum)) '(:searched)))
             (cons 'clifff::fff--free-grep-result (lambda (result) (declare (ignore result)))))
       (lambda ()
         (test-assert (equal '(:searched) (engine-search-content engine "needle"))
                      "single-pattern search returns its decoded result")
         (test-assert (equal '(:searched) (engine-search-multi-content engine '("needle" "other")))
                      "multi-pattern search returns its decoded result")
         (test-assert (= calls 2) "both native search entry points execute"))))))

(defun tests--worker-shutdown ()
  "Reap graceful shutdowns and abort failed transports without a cleanup deadline."
  (dolist (mode '(:graceful :broken :abort))
    (let* ((worker (clifff:make-worker :command '("unused")))
           (input  (make-string-output-stream))
           (events nil))
      (setf (clifff:worker-process worker) :fixture
            (clifff::worker--input worker) input)
      (tests--call-with-functions
       (list (cons 'uiop:process-alive-p (lambda (process) (declare (ignore process)) t))
             (cons 'clifff::worker--write-form
                   (lambda (stream form)
                     (declare (ignore form))
                     (push :shutdown events)
                     (when (eq mode :broken)
                       (error 'stream-error :stream stream))))
             (cons 'uiop:terminate-process
                   (lambda (process &key urgent)
                     (declare (ignore process))
                     (test-assert urgent "failed transports terminate immediately")
                     (push :terminate events)))
             (cons 'uiop:wait-process
                   (lambda (process)
                     (declare (ignore process))
                     (push :reap events)
                     0)))
       (lambda ()
         (if (eq mode :abort)
             (clifff::worker--recover-unlocked worker)
             (clifff:worker-close worker))
         (test-assert
          (equal (reverse events)
                 (ecase mode
                   (:graceful
                    '(:shutdown :reap))
                   (:broken
                    '(:shutdown :terminate :reap))
                   (:abort
                    '(:terminate :reap))))
          "graceful shutdown waits for exit and failed transports are terminated before reaping")
         (test-assert (null (clifff:worker-process worker)) "shutdown detaches the reaped process"))))))

(defun tests--ownership-framing ()
  "Distinguish helper death before and after a validated ownership acknowledgement."
  (let* ((old    '("old-library" "old-root/" "old-cache/"))
         (new    '("new-library" "new-root/" "new-cache/"))
         (worker (clifff:make-worker :command '("unused"))))
    (tests--call-with-functions
     (list (cons 'clifff::worker--ensure-unlocked (lambda (value) value)))
     (lambda ()
       (dolist (case '(:before :after :wrong-id :wrong-signature))
         (setf (clifff::worker--owned-signature worker) old
               (clifff::worker--next-request-id worker) 1
               (clifff::worker--input worker) (make-string-output-stream)
               (clifff::worker--output worker)
               (make-string-input-stream
                (if (eq case :before) ""
                    (write-to-string
                     (list :clifff-owned :version 1
                           :id           (if (eq case :wrong-id) 2 1)
                           :signature    (if (eq case :wrong-signature) old new))))))
         (test-assert
          (signals clifff::worker-protocol-error
            (clifff::worker--exchange worker :library-path    #P"new-library"
                                             :base-path       #P"new-root/"
                                             :cache-directory #P"new-cache/"
                                             :operation :files :arguments '("x")))
          "an incomplete or invalid exchange is a transport failure")
         (test-assert (equal (clifff::worker--owned-signature worker)
                             (if (eq case :after) new old))
                      "only a matching acknowledgement transfers database ownership"))))))

(defun tests--wait-until (predicate)
  "Wait at most ten seconds for PREDICATE to succeed."
  (loop repeat 100 when (funcall predicate) return t do (sleep 0.1)))

(defun tests--worker-lifecycle (library)
  "Exercise cache ownership, watching, query errors and real helper replacement."
  (let* ((root      (merge-pathnames (format nil "clifff-worker-~D/" (random most-positive-fixnum))
                                     (uiop:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (cache-a   (merge-pathnames "cache-a/" root))
         (cache-b   (merge-pathnames "cache-b/" root))
         (log-a     (merge-pathnames "first.log" root))
         (log-b     (merge-pathnames "second.log" root))
         (log-c     (merge-pathnames "third.log" root))
         (script    (merge-pathnames "tests/worker.lisp" (asdf:system-source-directory "clifff")))
         (lisp      (or (uiop:argv0) #+sbcl (first sb-ext:*posix-argv*) "sbcl"))
         (worker    (clifff:make-worker :command (list lisp "--noinform" "--disable-debugger"
                                                      "--script" (namestring script)))))
    (labels ((request (cache log &key (query "CLIFFF_WATCHED") (native library))
               "Search QUERY through WORKER using CACHE, LOG and NATIVE."
               (clifff:worker-request worker :library-path native :base-path workspace
                                      :cache-directory cache :log-pathname log
                                      :operation :content :arguments (list query)))

             (pid ()
               "Return the current native helper PID."
               (uiop:process-info-pid (clifff:worker-process worker))))
      (unwind-protect
           (progn
             (tests--write-file (merge-pathnames "sample.txt" workspace) "CLIFFF_INITIAL")
             (request cache-a log-a :query "CLIFFF_INITIAL")
             (let ((initial (pid)))
               (request cache-b log-a :query "CLIFFF_INITIAL")
               (test-assert (= initial (pid)) "two caches with one log reuse the helper process")
               (test-assert (signals clifff-error (request cache-b log-a :query (string #\Null)))
                            "a native argument error returns as a query error")
               (test-assert (= initial (pid)) "query errors do not replace the helper")
               (tests--write-file (merge-pathnames "sample.txt" workspace) "CLIFFF_WATCHED")
               (test-assert (tests--wait-until
                             (lambda () (search "sample.txt" (request cache-b log-a))))
                            "the watched index observes file updates")
               (test-assert (= initial (pid)) "watch updates preserve the live helper PID"))
             (tests--write-file (merge-pathnames "frecency/marker" cache-a) "untouched")
             (tests--write-file (merge-pathnames "frecency/marker" cache-b) "dead owner")
             (let* ((failed (clifff:worker-process worker)) (failed-pid (pid)))
               (uiop:terminate-process failed :urgent t)
               (uiop:wait-process failed)
               (request cache-a log-b)
               (test-assert (/= failed-pid (pid)) "a dead helper is replaced with a new PID")
               (test-assert (probe-file (merge-pathnames "frecency/marker" cache-a))
                            "death before a new request does not reset the incoming cache")
               (test-assert (not (probe-file (merge-pathnames "frecency/marker" cache-b)))
                            "death plus log switch resets only the acknowledged dead owner")
               (test-assert (probe-file (merge-pathnames "frecency/lock.mdb" cache-b))
                            "native lockfile identity survives recovery"))
             (let ((old (pid)))
               (request cache-b log-c)
               (test-assert (/= old (pid)) "a live log switch replaces the helper"))
             (let ((copy (merge-pathnames (clifff:fff-library-file-name) root)) (old (pid)))
               (uiop:copy-file library copy)
               (request cache-b log-c :query "CLIFFF_WATCHED" :native copy)
               (test-assert (/= old (pid)) "a native library switch replaces the helper process")))
        (clifff:worker-close worker)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))
