(in-package :retro-games)

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port 8080)))

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
	(:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
	  (:ol
	   (dolist (game (games))
	     (htm
	      (:li (:a :href (format nil "vote?name=~a"
				     (url-encode (name game))) "Vote!")
		   (fmt "~A with ~d votes" (escape-string (name game))
			(votes game)))))))))

(push (create-static-file-dispatcher-and-handler "/retro.css"
						 "C:/home/lisp-for-the-web/retro.css")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler "/logo.jpg"
						 "C:/home/lisp-for-the-web/logo.jpg")
      *dispatch-table*)

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
	    (:head
	     (:meta :charset "utf-8")
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "./retro.css")
	    ,(when script
	       `(:script :type "text/javascript"
			 (str ,script))))
	    (:body
	     (:div :id "header"
		   (:img :src "./logo.jpg"
			 :alt "Commodore 64"
			 :class "logo")
		   (:span :class "strapline"
			  "Vote on your favourite Retro Game"))
	     ,@body))))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
			 :script (ps
				   (defvar add-form nil)
				   (defun validate-game-name (evt)
				     (when (= (@ add-form name value) "")
				       (chain evt (prevent-default))
				       (alert "Please enter a name.")))
				   (defun init ()
				     (setf add-form (chain document (get-element-by-id "addform")))
				     (chain add-form (add-event-listener "submit" validate-game-name false)))
				   (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
	   (:p "What is the name of the game?" (:br)
	       (:input :type "text" :name "name" :class "txt"))
	   (:p (:input :type "submit" :value "Add" :class "btn")))))

(cl-mongo:db.use "games")

(defparameter *game-collection* "game")

(defun game-from-name (name)
  (let ((found-games (docs (db.find *game-collection* ($ "name" name)))))
    (when found-games
      (doc->game (first found-games)))))

(defun game-stored? (name)
  (game-from-name name))

(defclass game ()
  ((name :reader name
	 :initarg :name)
   (votes :accessor votes
	  :initarg :votes ;necessário quando leitura feita por BD
	  :initform 0)))

(defun doc->game (game-doc)
  (make-instance 'game
		 :name (get-element "name" game-doc)
		 :votes (get-element "votes" game-doc)))

(defun add-game (name)
  (let ((game (make-instance 'game :name name)))
    (db.insert *game-collection* (game->doc game))))

(defun game->doc (game)
  ($ ($ "name" (name game))
     ($ "votes" (votes game))))

(defun unique-index-on (field)
  (db.ensure-index *game-collection*
		   ($ field 1)
		   :unique t))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))  

(defmethod vote-for :after (game)
  (let ((game-doc (game->doc game)))
    (db.update *game-collection*
	       ($ "name" (name game))
	       game-doc)))

(defun games ()
  (mapcar #'doc->game
	  (docs (iter (db.sort *game-collection*
			       :all
			       :field "votes"
			       :asc nil)))))
