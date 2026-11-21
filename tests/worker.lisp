(require :asdf)

(let* ((root              (uiop:pathname-parent-directory-pathname
                           (uiop:pathname-directory-pathname *load-truename*)))
       (setup             (merge-pathnames ".qlot/setup.lisp" root))
       (*standard-output* *error-output*)
       (*compile-verbose* nil)
       (*load-verbose*    nil))
  (if (probe-file setup)
      (load setup)
      (let ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file quicklisp) (load quicklisp))))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "clifff.asd" root))
  (asdf:load-system "clifff" :verbose nil))

(clifff:worker-main)
