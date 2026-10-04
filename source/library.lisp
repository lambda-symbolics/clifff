(in-package #:clifff)

;;;; -- Pinned Library Location --

;;; A host that builds fff's C library from a pinned commit installs it with a
;;; manifest recording that commit, so it can tell a current library from one
;;; left behind by an older build.

(defparameter *fff-library-manifest-version* 1
  "The version of the manifest form written beside an installed fff library.")

(defun fff-library-file-name ()
  "Return this platform's file name for fff's C library."
  #+darwin "libfff_c.dylib"
  #+win32 "fff_c.dll"
  #-(or darwin win32) "libfff_c.so")

(defun fff-library-manifest-pathname (library)
  "Return the manifest pathname that sits beside LIBRARY."
  (merge-pathnames "manifest.sexp" (uiop:pathname-directory-pathname library)))

(defun fff-library-manifest (commit)
  "Return the manifest form recording a library built from fff COMMIT."
  (list :fff-library :version *fff-library-manifest-version* :commit commit))

(defun fff-library-current-p (library commit)
  "Return true when LIBRARY exists and its manifest records fff COMMIT."
  (let ((manifest (fff-library-manifest-pathname library)))
    (and (probe-file library)
         (probe-file manifest)
         (handler-case
             (with-open-file (stream manifest :external-format :utf-8)
               (let ((*read-eval* nil))
                 (equal (read stream nil nil) (fff-library-manifest commit))))
           (error ()
             nil))
         t)))

(defun fff-library-write-manifest (library commit)
  "Atomically record that LIBRARY was built from fff COMMIT."
  (let* ((manifest (fff-library-manifest-pathname library))
         (temporary (make-pathname :name (format nil ".manifest-~36R"
                                                 (random most-positive-fixnum
                                                         (make-random-state t)))
                                   :defaults manifest)))
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

(defun fff-library-locate (directory commit &key override)
  "Return the truename of fff's C library built from COMMIT in DIRECTORY.

OVERRIDE, when given, names a library to use instead; it is trusted without a
manifest, as a library supplied by a package manager is. Signal CLIFFF-ERROR
with operation :LOAD when the library is missing or was built from another
commit."
  (let ((library (or override (merge-pathnames (fff-library-file-name) directory))))
    (unless (probe-file library)
      (clifff--fail ':load (format nil "The fff library is missing at ~A." library)
                    :pathname library))
    (unless (or override (fff-library-current-p library commit))
      (clifff--fail ':load (format nil "The fff library at ~A was not built from revision ~A."
                                   library commit)
                    :pathname library))
    (truename library)))
