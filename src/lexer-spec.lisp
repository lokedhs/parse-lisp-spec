(in-package :parse-lisp-spec)

(declaim #.*compile-decl*)

(defvar *current-line-num* nil
  "Dynamic variable used to track the current line number during parsing")

(define-condition spec-error (error)
  ((line          :type integer
                  :initarg :line
                  :initform (error "~s required when creating ~s" :line 'spec-error)
                  :reader spec-error-line
                  :documentation "The line number where the error occurred.")
   (column        :type (or nil integer)
                  :initarg :column
                  :initform nil
                  :reader spec-error-column
                  :documentation "The column index of the line where the error
occurred, if available. Otherwise NIL.")
   (message       :type string
                  :initarg :message
                  :initform (error "~s required when creating ~s" :message 'spec-error)
                  :reader spec-error-message
                  :documentation "The error message")
   (content       :type (or nil string)
                  :initarg :content
                  :initform nil
                  :reader spec-error-content
                  :documentation "The actual spec content where the error
occurred. Either the entire line, or part of it.")
   (content-index :type (or nil integer)
                  :initarg :content-index
                  :initform nil
                  :reader spec-error-content-index
                  :documentation "The position in content closest to the actual error,
or NIL if the information is not available."))
  (:documentation "Error that is raised if there is an error parsing a spec")
  (:report (lambda (condition stream)
             (with-slots (line column message content content-index) condition
               (format stream "Line ~a~@[, column ~a~]: ~a" line column message)
               (when content
                 (format stream "~%~a~%~,,v@a" content content-index "^"))))))

(defun signal-parse-error (message &optional column content content-index)
  (error 'spec-error
         :line *current-line-num*
         :column column
         :message message
         :content content
         :content-index content-index))

(defun make-lexer-actions-list (definitions)
  (mapcar #'(lambda (definition)
              (destructuring-bind (regex action)
                  definition
                (list (cl-ppcre:create-scanner (concatenate 'string "^" regex)) action)))
          definitions))

(defun parse-definition (definition)
  (cond ((keywordp definition)
         (constantly definition))
        ((symbolp definition)
         (constantly (list definition)))
        ((functionp definition)
         definition)
        ((and (listp definition) (symbolp (car definition)))
         #'(lambda (name) (list (car definition) name)))
        (t
         (error "Illegal definition: ~s" definition))))

(defun make-lexer-actions (definitions standalone-macros)
  (make-lexer-actions-list (append (mapcar #'(lambda (definition)
                                               (list (car definition)
                                                     (parse-definition (cadr definition))))
                                           definitions)
                                   (mapcar #'(lambda (name)
                                               (list (format nil "\\\\~a(?![\\w-])" name)
                                                     (constantly (list (intern (string-upcase name))))))
                                           standalone-macros))))

(defun make-spec-actions ()
  (make-lexer-actions `(("[\\n]+" :blank)
                        ("%.*" :blank)
                        ;;("\\\\begincom{([\\w-]+)}" (begincom))
                        ;;("\\\\ftype{([\\w -]+)}" (ftype))
                        ("\\\\begincom{([\\w-]+)}\\\\ftype{Function}" (begincom-ftype))
                        ("\\\\begincom{([\\w-]+)}\\\\ftype{System Class}" (begincom-system-class))
                        ("\\\\begincom{([\\w-]+)}\\\\ftype{Type}" (begincom-type))
                        ("\\\\begincom{([\\w-]+)}\\\\ftype{Accessor}" (begincom-accessor))
                        ("\\\\begincom{([\\w-]+)}\\\\ftype{Constant Variable}" (begincom-constant-variable))
                        ("\\\\label ([\\w :-]+):(?::|\\\\None)" (label))
                        ("\\\\typeref{([\\w-]+)}" (typeref))
                        ("\\\\seefuns{([\\w-]+)}" (seefuns))
                        ("\\\\Shouldchecktype{([\\w-]+)}" (should-check-type))
                        ("\\\\term{([\\w *-]+)}" (term))
                        ("\\\\issue{([\\w :-]+)}" (issue))
                        ("\\\\endissue{([\\w :-]+)}" (endissue))
                        ("\\$([^$]+)\\$" (math-section))
                        ("\\\\Seefun{([\\w -]+)}" (seefun))
                        ("\\\\seesection\\\\([\\w-]+)(?![\\w-])" (seesection))
                        ("\\\\secref\\\\([\\w-]+)(?![\\w-])" (secref))
                        ("\\\\Deftype{([\\w-]+)}" (deftype))
                        ("\\\\auxbnf{([\\w -]+)}" (auxbnf))
                        ("\\\\param{([\\w -]+)}" (param))
                        ("\\\\varref{([\\w*-]+)}" (varref))
                        ("\\\\funref{([\\w*-]+)}" (funref))
                        ("\\\\oftype{([\\w -]+)}" (oftype))
                        ("\\\\objectoftype{([\\w ()\\\\{}-]+)}" (objectoftype))
                        ("\\\\meaning{([\\w -]+)}" (meaning))
                        ("\\\\[Tt]hetype{([\\w -]+)}" (thetype))
                        ("\\\\subtypeof{([\\w -]+)}" (subtypeof))
                        ("\\\\subtypesof{([\\w -]+)}" (subtypesof))
                        ("\\\\kwd{([\\w -]+)}" (kwd))
                        ("\\\\conref{([\\w -]+)}" (conref))
                        ("\\\\macref{([\\w -]+)}" (macref))
                        ("\\\\TypePredicate{([\\w-]+)}{([\\w-]+)}" ,#'(lambda (v1 v2) `(type-predicate (,v1 ,v2))))
                        ("{" open-brace)
                        ("}" close-brace)
                        ("([\\w .,_|*:;()+!'-])" (character))
                        ("([\\t~])" ,#'(lambda (name) (declare (ignore name)) (list 'character " ")))
                        ("\\\\nil" tex-nil)
                        ("\\\\t" tex-t)
                        ("\\\\ " space)
                        ("``" ,#'(lambda () (list 'character (string #\LEFT_DOUBLE_QUOTATION_MARK))))
                        ("''" ,#'(lambda () (list 'character (string #\RIGHT_DOUBLE_QUOTATION_MARK)))))
                      '("ttbrac" "curly" "misc" "brac" "paren" "star" "tt" "f" "i" "Default"
                        "DefunWithValuesNewline" "vtop" "hbox" "key" "endcom" "code" "endcode"
                        "rest" "opt"
                        "beginlist" "itemitem" "endlist"
                        "DefunWithValues" "Defsetf")))

(defun make-stream-spec-lexer (input-stream)
  (let ((lexer-actions (make-spec-actions))
        (current-line nil)
        (current-position 0)
        (input-finish nil))
    #'(lambda ()
        (labels ((read-next-line ()
                   (unless input-finish
                     (setq current-line (read-line input-stream nil nil))
                     (incf *current-line-num*)
                     (setq current-position 0)
                     (cond (current-line
                            :blank)
                           (t
                            (setq input-finish t)
                            nil))))

                 (read-code ()
                   (loop
                      with longest-match-length = 0
                      with longest-match-exprs = nil
                      with longest-match-action = nil
                      for (regex action) in lexer-actions                      
                      do (multiple-value-bind (result exprs)
                             (cl-ppcre:scan-to-strings regex current-line :start current-position)
                           (when (and result
                                      (> (length result) longest-match-length))
                             (setq longest-match-length (length result))
                             (setq longest-match-exprs exprs)
                             (setq longest-match-action action)))
                      finally (cond ((plusp longest-match-length)
                                     (incf current-position longest-match-length)
                                     (return (apply longest-match-action (coerce longest-match-exprs 'list))))
                                    (t
                                     (signal-parse-error "Syntax error"
                                                         current-position
                                                         current-line
                                                         current-position)))))

                 (parse-token ()
                   (cond ((null current-line)
                          (read-next-line))
                         ((>= current-position (length current-line))
                          (read-next-line))
                         (t
                          (read-code))))

                 (read-code-example ()
                   (with-output-to-string (s)
                     (loop
                        for line = (read-line input-stream nil nil)
                        until (cl-ppcre:scan "^[ \\t]*\\\\endcode[ \\t]*$" line)
                        do (format s "~a~%" line)))))

          (loop
             for token = (parse-token)
             while token
             if (and (consp token) (eq (car token) 'code))
             return (values 'code (read-code-example))
             unless (eq token :blank)
             return (apply #'values token))))))
