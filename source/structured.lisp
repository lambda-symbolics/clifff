(in-package #:clifff)

;;;; -- Structured content search --

(defun structured--blank-p (character)
  "Return whether CHARACTER separates fff tokens."
  (and (find character '(#\Space #\Tab #\Newline #\Return)) t))

(defun structured--tokens (text)
  "Split TEXT into whitespace-separated constraint tokens."
  (loop with start = nil
        for index from 0 to (length text)
        for blank = (or (= index (length text))
                        (structured--blank-p (char text index)))
        when (and blank start) collect (prog1 (subseq text start index)
                                         (setf start nil))
        do (when (and (not blank) (null start)) (setf start index))))

(defun structured--wildcard-p (token)
  "Return whether TOKEN contains a glob wildcard."
  (and (find-if (lambda (character) (find character "*?[{")) token) t))

(defun structured--directory-p (token)
  "Return whether TOKEN is a positive literal directory filter."
  (and (> (length token) 1) (char= (char token (1- (length token))) #\/)
       (char/= (char token 0) #\!) (not (structured--wildcard-p token))))

(defun structured--file-p (token)
  "Return whether fff interprets TOKEN as a literal file filter."
  (let* ((name (subseq token (1+ (or (position #\/ token :from-end t) -1))))
         (dot (position #\. name :from-end t))
         (extension (and dot (subseq name (1+ dot)))))
    (and (plusp (length token)) (char/= (char token 0) #\!)
         (char/= (char token (1- (length token))) #\/)
         (not (structured--wildcard-p token)) extension
         (<= 1 (length extension) 10) (alpha-char-p (char extension 0))
         (every #'alphanumericp extension) t)))

(defun structured--location-p (token)
  "Return whether TOKEN selects a literal directory or file."
  (or (structured--directory-p token) (structured--file-p token)))

(defun structured--location-glob (token)
  "Escape TOKEN for use as a brace-glob alternative."
  (let ((path (with-output-to-string (stream)
                (loop for character across (string-left-trim "/" token)
                      do (when (find character ",}\\") (write-char #\\ stream))
                         (write-char character stream)))))
    (if (structured--directory-p token)
        (format nil "**/~A**" path)
        (format nil "**/~A" path))))

(defun structured--constraints (engine constraints)
  "Return fff filters and a missing-path note for CONSTRAINTS on ENGINE."
  (unless (stringp constraints)
    (clifff--fail ':arguments "CONSTRAINTS must be a string."))
  (let* ((tokens (structured--tokens constraints))
         (missing (remove-if-not
                   (lambda (token)
                     (and (structured--file-p token)
                          (zerop (getf (engine-search-files engine (format nil "**/~A" token)
                                                           :glob-p t :page-size 1)
                                       :total-matched))))
                   tokens))
         (kept (remove-if (lambda (token) (member token missing :test #'string=)) tokens))
         (locations (remove-if-not #'structured--location-p kept)))
    (when (and missing (null locations))
      (clifff--fail ':arguments
                    (format nil "No indexed file matches the constraint ~{~A~^, ~}; check the path, or search its directory instead." missing)))
    (values (if (rest locations)
                (format nil "~{~A ~}{~{~A~^,~}}"
                        (remove-if #'structured--location-p kept)
                        (mapcar #'structured--location-glob locations))
                (if missing (format nil "~{~A~^ ~}" kept) constraints))
            (when missing
              (format nil "No indexed file matches ~{~A~^, ~}, so the search covered the other locations." missing)))))

(defun structured--escape-query (query)
  "Escape Lisp earmuffed names in QUERY for fff's token parser."
  (with-output-to-string (stream)
    (loop with length = (length query)
          with start = 0
          while (< start length)
          do (let* ((begin (or (position-if-not #'structured--blank-p query :start start) length))
                    (end (or (position-if #'structured--blank-p query :start begin) length))
                    (token (subseq query begin end)))
               (write-string query stream :start start :end begin)
               (when (and (>= (length token) 3) (char= (char token 0) #\*)
                          (char= (char token (1- (length token))) #\*)
                          (notany (lambda (c) (find c "*?[{/."))
                                  (subseq token 1 (1- (length token)))))
                 (write-char #\\ stream))
               (write-string token stream)
               (setf start end)))))

(defun engine-search-structured-content (engine query &rest options &key (constraints "") &allow-other-keys)
  "Search QUERY with separate CONSTRAINTS. Return the result and optional note."
  (unless (stringp query) (clifff--fail ':arguments "QUERY must be a string."))
  (multiple-value-bind (filters note) (structured--constraints engine constraints)
    (remf options :constraints)
    (values (apply #'engine-search-content engine
                   (if (string= (string-trim '(#\Space #\Tab #\Newline #\Return) filters) "")
                       (structured--escape-query query)
                       (format nil "~A ~A" filters (structured--escape-query query)))
                   options)
            note)))

(defun engine-search-structured-multi-content (engine patterns &rest options &key (constraints "") &allow-other-keys)
  "Search literal PATTERNS with separate CONSTRAINTS. Return result and note."
  (multiple-value-bind (filters note) (structured--constraints engine constraints)
    (remf options :constraints)
    (values (apply #'engine-search-multi-content engine patterns :constraints filters options) note)))

(defun structured--render (result note)
  "Render RESULT followed by its optional NOTE."
  (let ((content (render-content-result result)))
    (if note (format nil "~A~%Note: ~A" content note) content)))
