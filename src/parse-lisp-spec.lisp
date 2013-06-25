(in-package :parse-lisp-spec)

(declaim #.*compile-decl*)

(defun test (&optional file)
  (let ((*current-line-num* 0))
    (with-open-file (in (or file (merge-pathnames #p"dpans/dict-arrays.tex" (asdf:system-source-file :parse-lisp-spec))))
      (let ((lexer (make-stream-spec-lexer in)))
        (loop
           for v = (multiple-value-list (funcall lexer))
           while (car v)
           do (print v))))))

(defun test-parse (&optional file)
  (let ((*current-line-num* 0))
    (with-open-file (in (or file (merge-pathnames #p"dpans/dict-arrays.tex" (asdf:system-source-file :parse-lisp-spec))))
      (handler-case
          (let ((lexer (make-stream-spec-lexer in)))
            (let ((result (yacc:parse-with-lexer lexer *parser-spec*)))
              result))
        (yacc:yacc-parse-error (condition) (signal-parse-error
                                            (format nil "Parse error: terminal=~s value=~s expected=~s"
                                                  (yacc:yacc-parse-error-terminal condition)
                                                  (yacc:yacc-parse-error-value condition)
                                                  (yacc:yacc-parse-error-expected-terminals condition))))))))

(defun test-linear-parse (&optional file)
  (let ((*current-line-num* 0))
    (with-open-file (in (or file (merge-pathnames #p"dpans/dict-arrays.tex" (asdf:system-source-file :parse-lisp-spec))))
      (let ((lexer (make-stream-spec-lexer in)))
        (let ((tokens (loop
                         for v = (multiple-value-list (funcall lexer))
                         while (car v)
                         collect v)))
          (parse-file tokens))))))
