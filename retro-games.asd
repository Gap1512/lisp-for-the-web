(asdf:defsystem #:retro-games
    :description "Website desenvolvido em Lisp"
    :author "Gustavo Alves Pacheco <gap1512@gmail.com>"
    :serial t
    :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-mongo)
    :components ((:file "package")
		 (:file "retro-games" :depends-on ("package"))))
