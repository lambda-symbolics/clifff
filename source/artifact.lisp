;; Bootstrap callers may load this file with only ASDF/UIOP available.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:clifff)
    (make-package '#:clifff :use '(#:cl))))

(in-package #:clifff)

;;;; -- Native Library Artifacts --

(defparameter *fff-library-manifest-version* 1
  "The version of the manifest form beside an installed fff library.")

(defun fff-library-file-name ()
  "Return the platform file name for the fff C library."
  #+darwin "libfff_c.dylib"
  #+win32 "fff_c.dll"
  #-(or darwin win32) "libfff_c.so")

(defun fff-library-manifest-pathname (library)
  "Return the manifest pathname beside LIBRARY."
  (merge-pathnames "manifest.sexp" (uiop:pathname-directory-pathname library)))

(defun fff-library-manifest (commit)
  "Return the manifest form for a library built from fff COMMIT."
  (list :fff-library :version *fff-library-manifest-version* :commit commit))

(defun fff-library-current-p (library commit &key required-artifacts)
  "Return true when LIBRARY has a COMMIT manifest and all REQUIRED-ARTIFACTS exist."
  (let ((manifest (fff-library-manifest-pathname library)))
    (and (probe-file library)
         (every #'probe-file required-artifacts)
         (probe-file manifest)
         (handler-case
             (with-open-file (stream manifest :external-format :utf-8)
               (let ((*read-eval* nil))
                 (equal (read stream nil nil) (fff-library-manifest commit))))
           (error () nil))
         t)))

(defun fff-library--temporary-pathname (target)
  "Return a private sibling pathname for publishing TARGET."
  (make-pathname :name     (format nil ".~A-~36R" (pathname-name target)
                                  (random most-positive-fixnum (make-random-state t)))
                 :defaults target))

(defun fff-library-publish (source target)
  "Atomically copy SOURCE over TARGET and return TARGET."
  (let ((temporary (fff-library--temporary-pathname target)))
    (ensure-directories-exist target)
    (unwind-protect
         (progn
           (uiop:copy-file source temporary)
           (uiop:rename-file-overwriting-target temporary target))
      (when (probe-file temporary)
        (delete-file temporary))))
  target)

(defun fff-library-write-manifest (library commit)
  "Atomically record that LIBRARY was built from fff COMMIT."
  (let* ((manifest  (fff-library-manifest-pathname library))
         (temporary (fff-library--temporary-pathname manifest)))
    (ensure-directories-exist manifest)
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output :if-exists :supersede
                                              :external-format :utf-8)
             (with-standard-io-syntax
               (prin1 (fff-library-manifest commit) stream))
             (terpri stream))
           (uiop:rename-file-overwriting-target temporary manifest))
      (when (probe-file temporary)
        (delete-file temporary))))
  library)
