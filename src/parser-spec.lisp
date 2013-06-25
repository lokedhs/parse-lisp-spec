(in-package :parse-lisp-spec)

(declaim #.*compile-decl*)

(defmacro short-define-parser (name initials &body definitions)
  (labels ((process-row (row)
             (let* ((arguments (car row))
                    (param-list (mapcar #'(lambda (arg) (if (listp arg) (cadr arg) arg)) arguments)))
               (append (mapcar #'(lambda (arg) (if (listp arg) (car arg) arg)) arguments)
                       (when (cadr row) (list `#'(lambda ,param-list
                                                   (declare (ignorable ,@param-list))
                                                   ,@(cdr row)))))))

           (process-definition (definition)
             (append (list (car definition))
                     (mapcar #'process-row (cdr definition)))))

    `(yacc:define-parser ,name
       ,@initials
       ,@(mapcar #'process-definition definitions))))

(short-define-parser *parser-spec* ((:start-symbol reference-item)
                                    (:terminals (begincom-ftype begincom-system-class begincom-type
                                                                ftype label typeref term issue
                                                                endissue math-section seefun
                                                                seesection secref deftype auxbnf
                                                                param varref funref oftype thetype
                                                                subtypeof subtypesof kwd open-brace
                                                                close-brace character ttbrac
                                                                curly misc brac paren
                                                                star tt f i Default
                                                                DefunWithValuesNewline vtop
                                                                hbox key nil endcom)))
  (document
   ((document-entries)
    document-entries))

  (document-entries
   ((text)
    text))

  (reference-item
   ((system-class)
    system-class))

  (system-class
   ((begincom-system-class sections endcom)
    (list :system-class begincom-system-class sections)))

  (sections
   ((section sections)
    (cons section sections))
   (()
    nil))

  (section
   ((label body)
    (list :section label body)))

  (body
   ((text body)
    (cons text body))
   ((typeref body)
    (cons (list :typeref typeref) body))
   ((term body)
    (cons (list :term term) body)))

  (text
   ((character text)
    (concatenate 'string character text))
   (()
    "")))
