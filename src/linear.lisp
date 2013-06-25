(in-package :parse-lisp-spec)

(declaim #.*compile-decl*)

(defun replace-text (text search-re replacement)
  (let ((result (cl-ppcre:all-matches search-re text)))
    (if result
        (with-output-to-string (out)
          (loop
             with last = 0
             for (start end) on result by #'cddr
             do (progn
                  (princ (subseq text last start) out)
                  (princ replacement out)
                  (setq last end))
             finally (princ (subseq text last) out)))
        ;; No match, return the original string
        text)))

(defun replace-tex-char (text)
  (replace-text text "---" (string #\EM_DASH)))

(defun normalise-text (tokens)
  (loop
     with current-text = nil
     for token in tokens
     when (eq (car token) 'character)
     do (setq current-text (if (null current-text)
                               (cadr token)
                               (concatenate 'string current-text (cadr token))))
     else if (not (null current-text))
     collect (list 'text (replace-tex-char (prog1 current-text (setq current-text nil))))
     unless (eq (car token) 'character)
     collect token))

(defun split-token-list (tokens test)
  (let ((end tokens))
    (let ((result (loop
                     while (and end (not (funcall test (car end))))
                     collect (car end)
                     do (setq end (cdr end)))))
      (values result end))))

(defun process-system-class (name tokens)
  (print (list 'processing name))
  (multiple-value-bind (active rest) (split-token-list tokens #'(lambda (v) (eq (car v) 'endcom)))
    (print active)
    rest))

(defun dispatch (tokens)
  (case (caar tokens)
    (begincom-system-class (process-system-class (cadar tokens) (cdr tokens)))
    (t (cdr tokens))))

(defun parse-file (tokens)
  (let ((normalised (normalise-text tokens)))
    (loop
       for e = (dispatch normalised) then (dispatch e)
       while e)))
