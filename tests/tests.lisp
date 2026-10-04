(in-package #:clifff/tests)

(defun tests--write-file (pathname content)
  "Write CONTENT to PATHNAME for a native integration fixture."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  nil)

(defun tests--unit-tests ()
  "Exercise ABI declarations, validation, and deterministic presentation."
  (test-assert (= +create-options-version+ 2)
               "the binding declares create-options ABI version 2")
  (test-assert (= (cffi:foreign-type-size '(:struct clifff::fff-result)) 32)
               "the result envelope matches the x86-64 C ABI")
  (test-assert
   (= (cffi:foreign-type-size '(:struct clifff::fff-create-options)) 88)
   "the create options match version 2 of the x86-64 C ABI")
  (test-assert
   (signals clifff-error
     (make-engine :library-path #P"/missing/libfff_c.so"
                  :base-path #P"/missing/workspace/"
                  :cache-directory #P"/tmp/clifff/"))
   "engine construction rejects a missing workspace")
  (let ((rendered
          (render-file-result
           (list :kind ':files
                 :items (list (list :path "src/example.lisp"
                                    :git-status "clean"
                                    :size 42
                                    :frecency 0
                                    :binary-p nil))
                 :count 1
                 :total-matched 2
                 :total-files 7
                 :page 0
                 :page-size 1
                 :next-page 1))))
    (test-assert (and (search "src/example.lisp" rendered)
                      (search "next-page: 1" rendered))
                 "file results render paths and pagination"))
  (let ((rendered
          (render-content-result
           (list :kind ':content
                 :matches
                 (list (list :path "src/example.lisp"
                             :git-status "clean"
                             :line-content "needle"
                             :line-number 2
                             :column 1
                             :context-before '("before")
                             :context-after '("after")
                             :fuzzy-score nil
                             :definition-p t
                             :binary-p nil))
                 :count 1
                 :searched 1
                 :eligible 1
                 :total-files 1
                 :next-file-offset 0
                 :regex-fallback-error nil))))
    (test-assert
     (and (search "src/example.lisp:2:1" rendered)
          (search "before" rendered)
          (search "needle" rendered)
          (search "after" rendered)
          (search "1 match;" rendered))
     "content results render location and context")
    (test-assert
     (search "3 matches;"
             (render-content-result
              (list :kind ':content :matches nil :count 3 :searched 1 :eligible 1
                    :total-files 1 :next-file-offset 0 :regex-fallback-error nil)))
     "several content matches are counted in plural"))
  nil)

(defun tests--native-tests (library)
  "Exercise native search operations through LIBRARY."
  (let ((root
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "clifff-tests-~D-~D/"
                    (get-universal-time)
                    (random most-positive-fixnum))
            (uiop:temporary-directory))))
        (engine nil))
    (unwind-protect
         (progn
           (tests--write-file
            (merge-pathnames "src/example.lisp" root)
            (format nil "before~%CLIFFF_PRIMARY~%after~%"))
           (tests--write-file
            (merge-pathnames "docs/example.org" root)
            (format nil "CLIFFF_SECONDARY~%"))
           (setf engine
                 (make-engine :library-path library
                              :base-path root
                              :cache-directory
                              (merge-pathnames "cache/" root)))
           (let ((files (engine-search-files engine "example" :page-size 20)))
             (test-assert
              (find "src/example.lisp" (getf files :items)
                    :key (lambda (item) (getf item :path))
                    :test #'string=)
              "native file search returns relative paths"))
           (let ((content
                   (engine-search-content engine "CLIFFF_PRIMARY"
                                          :context-lines 1)))
             (test-assert
              (and (= (getf content :count) 1)
                   (equal (getf (first (getf content :matches)) :context-before)
                          '("before"))
                   (equal (getf (first (getf content :matches)) :context-after)
                          '("after")))
              "native content search copies matching context"))
           (let ((content
                   (engine-search-multi-content
                    engine
                    '("CLIFFF_PRIMARY" "CLIFFF_SECONDARY")
                    :constraints "*.lisp")))
             (test-assert
              (and (= (getf content :count) 1)
                   (string= (getf (first (getf content :matches)) :path)
                            "src/example.lisp"))
              "native multi-search applies file constraints")))
      (when engine
        (engine-close engine))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(defun run-tests ()
  "Run clifff tests, including native integration when configured."
  (setf *test-count* 0)
  (tests--unit-tests)
  (let ((library (uiop:getenv "CLIFFF_LIBRARY")))
    (when (and library (plusp (length library)))
      (tests--native-tests (pathname library))))
  (format t "~&~:D clifff tests passed.~%" *test-count*)
  nil)
