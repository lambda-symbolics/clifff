(asdf:defsystem #:clifff
  :description "Common Lisp bindings and a supervised worker for fff"
  :author      "Lukáš Hozda"
  :license     "ISC"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:bordeaux-threads
                #:cffi)
  :components  ((:module     "source"
                 :serial     t
                 :components ((:file "package")
                              (:file "ffi")
                              (:file "artifact")
                              (:file "library")
                              (:file "engine")
                              (:file "structured")
                              (:file "worker"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:clifff/tests))))

(asdf:defsystem #:clifff/tests
  :description "Tests for clifff"
  :depends-on  (#:clifff)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "package")
                              (:file "tests")
                              (:file "lifecycle"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:clifff/tests '#:run-tests)))
