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
     "several content matches are counted in plural")
    (test-assert
     (search "per-file limit of 2 matches reached in: src/a.lisp, src/b.lisp"
             (render-content-result
              (list :kind ':content :matches nil :count 4 :searched 2 :eligible 2
                    :total-files 2 :next-file-offset 0 :regex-fallback-error nil
                    :maximum-matches-per-file 2
                    :truncated-paths '("src/a.lisp" "src/b.lisp"))))
     "files cut off by the per-file limit are named"))
  nil)

(defun tests--library-location ()
  "Exercise locating a pinned library through its manifest."
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "clifff-library-~D/" (random most-positive-fixnum))
                                      (uiop:temporary-directory))))
         (library (merge-pathnames (fff-library-file-name) directory)))
    (unwind-protect
         (progn
           (test-assert (signals clifff-error (fff-library-locate directory "abc"))
                        "a missing library is refused")
           (tests--write-file library "binary")
           (test-assert (and (not (fff-library-current-p library "abc"))
                             (signals clifff-error (fff-library-locate directory "abc")))
                        "a library without a manifest is not current")
           (fff-library-write-manifest library "abc")
           (test-assert (and (fff-library-current-p library "abc")
                             (equal (fff-library-locate directory "abc") (truename library)))
                        "a library whose manifest records the commit is located")
           (test-assert (signals clifff-error (fff-library-locate directory "def"))
                        "a library built from another commit is refused")
           (test-assert (equal (fff-library-locate directory "def" :override library)
                               (truename library))
                        "an override library is trusted without a manifest"))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
  nil)

(defun tests--database-reset-tests ()
  "Verify cache recovery preserves lockfiles and discards rebuildable state."
  (let ((root (uiop:ensure-directory-pathname
               (merge-pathnames
                (format nil "clifff-reset-tests-~D-~D/"
                        (get-universal-time) (random most-positive-fixnum))
                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (clifff::worker--reset-databases root)
           (test-assert (not (probe-file root))
                        "resetting absent databases does not create a cache")
           (dolist (name '("frecency/" "history/"))
             (let ((directory (merge-pathnames name root)))
               (tests--write-file (merge-pathnames "lock.mdb" directory) "lock identity")
               (tests--write-file (merge-pathnames "data.mdb" directory) "dead data")
               (tests--write-file (merge-pathnames "sentinel" directory) "dead marker")
               (tests--write-file (merge-pathnames "nested/payload" directory) "dead payload")))
           (clifff::worker--reset-databases root)
           (dolist (name '("frecency/" "history/"))
             (let ((directory (merge-pathnames name root)))
               (test-assert
                (string= (uiop:read-file-string (merge-pathnames "lock.mdb" directory))
                         "lock identity")
                "reset preserves the native lockfile contents")
               (test-assert
                (and (not (probe-file (merge-pathnames "data.mdb" directory)))
                     (not (probe-file (merge-pathnames "sentinel" directory)))
                     (not (probe-file (merge-pathnames "nested/" directory))))
                "reset discards database payload, markers and nested cache state")))
           (clifff::worker--reset-databases root)
           (test-assert (probe-file (merge-pathnames "history/lock.mdb" root))
                        "reset is repeatable before the replacement helper opens"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
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
           (tests--write-file
            (merge-pathnames "docs/repeated.org" root)
            (format nil "CLIFFF_REPEATED one~%CLIFFF_REPEATED two~%CLIFFF_REPEATED three~%"))
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
              "native multi-search applies file constraints"))
           (dolist (search
                    (list (lambda (limit)
                            (engine-search-content engine "CLIFFF_REPEATED"
                                                   :maximum-matches-per-file limit))
                          (lambda (limit)
                            (engine-search-multi-content
                             engine '("CLIFFF_REPEATED")
                             :maximum-matches-per-file limit))))
             (let ((cut (funcall search 2))
                   (whole (funcall search 3)))
               (test-assert
                (and (= (getf cut :count) 2)
                     (equal (getf cut :truncated-paths) '("docs/repeated.org"))
                     (= (getf whole :count) 3)
                     (null (getf whole :truncated-paths)))
                "a file over the per-file limit keeps the limit and is reported")))
           (flet ((file-count (glob)
                    (clifff::worker--dispatch
                     engine (list :clifff-request :operation :file-count
                                                  :arguments (list glob)))))
             (test-assert
              (and (string= (file-count "**/src/example.lisp") "1")
                   (string= (file-count "**/missing/example.lisp") "0")
                   (string= (file-count "**/{src,docs}/*") "3"))
              "the worker counts the indexed files a glob matches")))
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
  (tests--library-location)
  (tests--database-reset-tests)
  (let ((library (uiop:getenv "CLIFFF_LIBRARY")))
    (when (and library (plusp (length library)))
      (tests--native-tests (pathname library))))
  (format t "~&~:D clifff tests passed.~%" *test-count*)
  nil)
