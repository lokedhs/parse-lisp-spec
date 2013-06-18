(asdf:defsystem parse-lisp-spec
  :name "parse-lisp-spec"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Parse the dpans tex files to a Lisp structure"
  :depends-on (:cl-ppcre
               :yacc)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "lexer-spec")
                                     (:file "parser-spec")
                                     (:file "parse-lisp-spec")))))
