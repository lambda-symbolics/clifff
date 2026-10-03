(in-package #:clifff)

;;;; -- Conditions --

(define-condition clifff-error (error)
  ((message
    :initarg :message
    :reader clifff-error-message
    :type string
    :documentation "The human-readable description of the failure.")
   (operation
    :initarg :operation
    :reader clifff-error-operation
    :type keyword
    :documentation "The fff operation that failed.")
   (pathname
    :initarg :pathname
    :initform nil
    :reader clifff-error-pathname
    :type (or null pathname)
    :documentation "The related library or workspace pathname, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader clifff-error-cause
    :type t
    :documentation "The underlying condition or remote failure, when known."))
  (:report
   (lambda (condition stream)
     (format stream "~A~@[ (~A)~]"
             (clifff-error-message condition)
             (clifff-error-pathname condition))))
  (:documentation "A native fff operation or supervised worker request failed."))

(defun clifff--fail (operation message &key pathname cause)
  "Signal a structured CLIFFF-ERROR for OPERATION."
  (error 'clifff-error
         :message message
         :operation operation
         :pathname pathname
         :cause cause))


;;;; -- fff C ABI --

(defconstant +create-options-version+ 2
  "The fff create-options ABI version bound by this release.")

(defcstruct fff-result
  (success :uint8)
  (error :pointer)
  (handle :pointer)
  (int-value :int64))

(defcstruct fff-create-options
  (version :uint32)
  (base-path :pointer)
  (frecency-db-path :pointer)
  (history-db-path :pointer)
  (enable-mmap-cache :uint8)
  (enable-content-indexing :uint8)
  (watch :uint8)
  (ai-mode :uint8)
  (log-file-path :pointer)
  (log-level :pointer)
  (cache-budget-max-files :uint64)
  (cache-budget-max-bytes :uint64)
  (cache-budget-max-file-size :uint64)
  (enable-fs-root-scanning :uint8)
  (enable-home-dir-scanning :uint8)
  (follow-symlinks :uint8))

(defcfun ("fff_create_instance_with" fff--create-instance-with) :pointer
  (options :pointer))

(defcfun ("fff_destroy" fff--destroy) :void
  (handle :pointer))

(defcfun ("fff_wait_for_scan" fff--wait-for-scan) :pointer
  (handle :pointer)
  (timeout-milliseconds :uint64))

(defcfun ("fff_search" fff--search) :pointer
  (handle :pointer)
  (query :pointer)
  (current-file :pointer)
  (max-threads :uint32)
  (page-index :uint32)
  (page-size :uint32)
  (combo-boost-multiplier :int32)
  (minimum-combo-count :uint32))

(defcfun ("fff_glob" fff--glob) :pointer
  (handle :pointer)
  (pattern :pointer)
  (current-file :pointer)
  (max-threads :uint32)
  (page-index :uint32)
  (page-size :uint32))

(defcfun ("fff_live_grep_ex" fff--live-grep) :pointer
  (handle :pointer)
  (query :pointer)
  (mode :uint8)
  (maximum-file-size :uint64)
  (maximum-matches-per-file :uint32)
  (smart-case :uint8)
  (file-offset :uint32)
  (page-limit :uint32)
  (time-budget-milliseconds :uint64)
  (enforce-time-budget :uint8)
  (before-context :uint32)
  (after-context :uint32)
  (classify-definitions :uint8))

(defcfun ("fff_multi_grep_ex" fff--multi-grep) :pointer
  (handle :pointer)
  (patterns :pointer)
  (constraints :pointer)
  (maximum-file-size :uint64)
  (maximum-matches-per-file :uint32)
  (smart-case :uint8)
  (file-offset :uint32)
  (page-limit :uint32)
  (time-budget-milliseconds :uint64)
  (enforce-time-budget :uint8)
  (before-context :uint32)
  (after-context :uint32)
  (classify-definitions :uint8))

(defcfun ("fff_free_result" fff--free-result) :void
  (result :pointer))

(defcfun ("fff_free_search_result" fff--free-search-result) :void
  (result :pointer))

(defcfun ("fff_free_grep_result" fff--free-grep-result) :void
  (result :pointer))

(defcfun ("fff_search_result_get_count" fff--search-result-count) :uint32
  (result :pointer))

(defcfun ("fff_search_result_get_total_matched"
          fff--search-result-total-matched) :uint32
  (result :pointer))

(defcfun ("fff_search_result_get_total_files"
          fff--search-result-total-files) :uint32
  (result :pointer))

(defcfun ("fff_search_result_get_item" fff--search-result-item) :pointer
  (result :pointer)
  (index :uint32))

(defcfun ("fff_file_item_get_relative_path" fff--file-item-relative-path) :pointer
  (item :pointer))

(defcfun ("fff_file_item_get_git_status" fff--file-item-git-status) :pointer
  (item :pointer))

(defcfun ("fff_file_item_get_size" fff--file-item-size) :uint64
  (item :pointer))

(defcfun ("fff_file_item_get_total_frecency_score"
          fff--file-item-frecency) :int64
  (item :pointer))

(defcfun ("fff_file_item_get_is_binary" fff--file-item-binary-p) :uint8
  (item :pointer))

(defcfun ("fff_grep_result_get_count" fff--grep-result-count) :uint32
  (result :pointer))

(defcfun ("fff_grep_result_get_total_files_searched"
          fff--grep-result-total-files-searched) :uint32
  (result :pointer))

(defcfun ("fff_grep_result_get_total_files"
          fff--grep-result-total-files) :uint32
  (result :pointer))

(defcfun ("fff_grep_result_get_filtered_file_count"
          fff--grep-result-filtered-file-count) :uint32
  (result :pointer))

(defcfun ("fff_grep_result_get_next_file_offset"
          fff--grep-result-next-file-offset) :uint32
  (result :pointer))

(defcfun ("fff_grep_result_get_regex_fallback_error"
          fff--grep-result-regex-fallback-error) :pointer
  (result :pointer))

(defcfun ("fff_grep_result_get_match" fff--grep-result-match) :pointer
  (result :pointer)
  (index :uint32))

(defcfun ("fff_grep_match_get_relative_path"
          fff--grep-match-relative-path) :pointer
  (match :pointer))

(defcfun ("fff_grep_match_get_git_status" fff--grep-match-git-status) :pointer
  (match :pointer))

(defcfun ("fff_grep_match_get_line_content"
          fff--grep-match-line-content) :pointer
  (match :pointer))

(defcfun ("fff_grep_match_get_line_number"
          fff--grep-match-line-number) :uint64
  (match :pointer))

(defcfun ("fff_grep_match_get_col" fff--grep-match-column) :uint32
  (match :pointer))

(defcfun ("fff_grep_match_get_context_before_count"
          fff--grep-match-context-before-count) :uint32
  (match :pointer))

(defcfun ("fff_grep_match_get_context_before"
          fff--grep-match-context-before) :pointer
  (match :pointer)
  (index :uint32))

(defcfun ("fff_grep_match_get_context_after_count"
          fff--grep-match-context-after-count) :uint32
  (match :pointer))

(defcfun ("fff_grep_match_get_context_after"
          fff--grep-match-context-after) :pointer
  (match :pointer)
  (index :uint32))

(defcfun ("fff_grep_match_get_fuzzy_score"
          fff--grep-match-fuzzy-score) :uint16
  (match :pointer))

(defcfun ("fff_grep_match_get_has_fuzzy_score"
          fff--grep-match-has-fuzzy-score-p) :uint8
  (match :pointer))

(defcfun ("fff_grep_match_get_is_definition"
          fff--grep-match-definition-p) :uint8
  (match :pointer))

(defcfun ("fff_grep_match_get_is_binary" fff--grep-match-binary-p) :uint8
  (match :pointer))


;;;; -- Foreign Result Helpers --

(defvar *foreign-library* nil
  "The process-global CFFI handle for the loaded fff library.")

(defvar *foreign-library-path* nil
  "The canonical pathname from which *FOREIGN-LIBRARY* was loaded.")

(defvar *foreign-library-lock* (make-lock "clifff foreign library")
  "Serialize process-global loading of fff.")

(defun clifff--load-library (pathname)
  "Load fff from PATHNAME once and return its CFFI handle."
  (let ((pathname (or (probe-file pathname) pathname)))
    (unless (probe-file pathname)
      (clifff--fail ':load "The fff C library does not exist."
                    :pathname pathname))
    (setf pathname (truename pathname))
    (with-lock-held (*foreign-library-lock*)
      (cond
        ((and *foreign-library* (equal pathname *foreign-library-path*))
         *foreign-library*)
        (*foreign-library*
         (clifff--fail
          ':load
          (format nil "fff is already loaded from ~A instead of ~A."
                  *foreign-library-path* pathname)
          :pathname pathname))
        (t
         (handler-case
             (setf *foreign-library* (load-foreign-library pathname)
                   *foreign-library-path* pathname)
           (error (cause)
             (clifff--fail
              ':load
              (format nil "Could not load the fff C library: ~A" cause)
              :pathname pathname
              :cause cause))))))))

(defun clifff--foreign-string (pointer)
  "Copy POINTER's UTF-8 C string, returning NIL for a null pointer."
  (unless (or (null pointer) (null-pointer-p pointer))
    (foreign-string-to-lisp pointer :encoding :utf-8)))

(defun clifff--take-handle-result (result operation &key pathname)
  "Free RESULT's envelope and return its successful non-null handle."
  (when (or (null result) (null-pointer-p result))
    (clifff--fail operation "fff returned a null result." :pathname pathname))
  (let ((success-p nil)
        (message nil)
        (handle nil))
    (unwind-protect
         (setf success-p
               (not (zerop (foreign-slot-value result
                                               '(:struct fff-result)
                                               'success)))
               message
               (clifff--foreign-string
                (foreign-slot-value result '(:struct fff-result) 'error))
               handle
               (foreign-slot-value result '(:struct fff-result) 'handle))
      (fff--free-result result))
    (unless success-p
      (clifff--fail operation
                    (or message "fff reported an unknown failure.")
                    :pathname pathname))
    (when (null-pointer-p handle)
      (clifff--fail operation
                    "fff returned a successful result without a payload."
                    :pathname pathname))
    handle))

(defun clifff--take-integer-result (result operation &key pathname)
  "Free RESULT's envelope and return its successful integer payload."
  (when (or (null result) (null-pointer-p result))
    (clifff--fail operation "fff returned a null result." :pathname pathname))
  (let ((success-p nil)
        (message nil)
        (value 0))
    (unwind-protect
         (setf success-p
               (not (zerop (foreign-slot-value result
                                               '(:struct fff-result)
                                               'success)))
               message
               (clifff--foreign-string
                (foreign-slot-value result '(:struct fff-result) 'error))
               value
               (foreign-slot-value result '(:struct fff-result) 'int-value))
      (fff--free-result result))
    (unless success-p
      (clifff--fail operation
                    (or message "fff reported an unknown failure.")
                    :pathname pathname))
    value))
