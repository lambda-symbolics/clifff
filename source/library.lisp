(in-package #:clifff)

;;;; -- Pinned Library Location --

;;; A host that builds fff's C library from a pinned commit installs it with a
;;; manifest recording that commit, so it can tell a current library from one
;;; left behind by an older build.

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
