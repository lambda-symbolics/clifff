(defpackage #:clifff
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-timeout
                #:with-lock-held)
  (:import-from #:cffi
                #:defcstruct
                #:defcfun
                #:foreign-slot-value
                #:foreign-string-to-lisp
                #:foreign-type-size
                #:load-foreign-library
                #:mem-aref
                #:null-pointer
                #:null-pointer-p
                #:with-foreign-object
                #:with-foreign-string)
  (:export #:+create-options-version+
           #:clifff-error
           #:clifff-error-cause
           #:clifff-error-operation
           #:clifff-error-pathname
           #:engine
           #:engine-base-path
           #:engine-close
           #:engine-detach
           #:engine-search-content
           #:engine-search-files
           #:engine-search-multi-content
           #:make-engine
           #:render-content-result
           #:render-file-result
           #:worker
           #:worker-close
           #:worker-detach
           #:worker-main
           #:worker-process
           #:worker-request
           #:worker-file-count
           #:make-worker))

(defpackage #:clifff/tests
  (:use #:cl)
  (:import-from #:clifff
                #:+create-options-version+
                #:clifff-error
                #:engine-close
                #:engine-search-content
                #:engine-search-files
                #:engine-search-multi-content
                #:make-engine
                #:render-content-result
                #:render-file-result)
  (:export #:run-tests))
