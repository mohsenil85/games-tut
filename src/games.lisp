(in-package :cl)
(ql:quickload "postmodern")
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")
(ql:quickload "parenscript")
(defpackage games
  (:use :cl 
       :cl-who 
        :hunchentoot 
        :parenscript))
(in-package :games)

(defclass game ()
  ((name  :col-type string 
          :reader name
          :initarg :name)
   (votes :accessor votes
          :col-type integer
          :initform 0))
  (:metaclass  postmodern:dao-class)
  (:keys name))

;(postmodern:dao-table-definition 'game)
;(postmodern:execute (postmodern:dao-table-definition 'game))

(postmodern:connect-toplevel "lmohseni" "lmohseni" "lmohseni" "localhost")
;(postmodern:query (:select '* :from 'game ))
;(postmodern:query  "drop table game")
;(postmodern:query "create table game (game varchar(20) primary key, votes int default(0));")
;(postmodern:dao-table-definition 'game)

;(setf baz (postmodern:insert-dao (make-instance 'game :name "baz")))
;;(setf many-lost-hours (make-instance 'game :name "Tetris"))
;;(votes many-lost-hours)
;;

;(defmethod vote-for (user-selected-game)
;  (incf (votes user-selected-game)))

;;(let ((g (postmodern:get-dao  'game "bar")))
 ;; (incf (votes g)) (postmodern:update-dao g))

 (defmethod vote-for (user-selected-game)
   (let ((g (postmodern:get-dao 'game user-selected-game)))
  (incf (votes g)) 
  (postmodern:update-dao g) ))
;(vote-for "bar")
;(defvar *games* '())

;(postmodern:get-dao 'game :name "Tetris")

  ;(postmodern:query (:select 'g :from 'game :where (:like 'name '$1))) 

(defun game-from-name (name)
  (postmodern:query (:select '* :from 'game :where (:like 'name name))) )
  ;(postmodern:get-dao 'game :name name)
;(game-from-name "foo")
;(defun game-from-name (name)
;  (find name  *games* :test #'string-equal
;                      :key #'name))

;(game-stored? "foo")
(defun game-stored? (game-name)
  (game-from-name game-name))

;(postmodern:sql (select '* :from 'game :order :by 'votes :desc ))
;(postmodern:query "select * from game order by votes desc")

(defun games ()
(postmodern:query "select * from game order by votes desc"))

(defun add-game (name)
  (unless (game-stored? name)
    (postmodern:insert-dao (make-instance 'game :name name) )))

;(mapcar #'name (games))
;(add-game "foo")
;(add-game "zap")
;(vote-for "zap")
;(add-game "baz")
;(add-game "zip")
;(add-game "ding")
;;
(setf (html-mode) :html5)

;(with-html-output (*standard-output* nil :prologue t :indent t)
;  (:html
;    (:head
;      (:title "test"))
;    (:body
;      (:p "preety neett"))))


(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
       (:head
         (:title ,title)
         (:link :type "text/css"
                :rel "stylesheet"
                :href "style.css"))
       (:body 
         (:div :id "header"
          (:h1 "this is the header"))
         ,@body))))

(defvar srv (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start srv )
(hunchentoot:stop srv)
      
;(standard-page (:title "Retro Games")
 ; (:h1 "Top Retro Games")
 ; (:p "We'll write the code later..."))

(defmacro define-url-fn ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher ,(format nil "/~(~a~).htm" name ) ',name) *dispatch-table*))) 

(define-url-fn (retro-games)
  (standard-page (:title "top retro games")
                (:h1 "vote on your favorite")
                (:p "missing a game?  add it " (:a :href "new-game.htm" "here"))
                (:div :id "chart"
                 (:ol
                   (dolist (game (games))
                     (htm
                       (:li
                         (:a :href (format nil "vote.htm?name=~a" (name game)) "VOTE!")
                         (fmt "~A with ~d votes" (name game) (votes game)))))))))
(define-url-fn (vote)
  (let ((game (game-from-name (parameter "name"))))
    (if game
      (vote-for game))
    (redirect "/retro-games.htm")))
  

(define-url-fn (new-game)
  (standard-page
    (:title "add a new game")
    (:h1 "add a new game")
    (:form :action "/game-added.htm" :method "post"
     (:p "game name" (:br)
      (:input :type "text"
              :name "name"
              :class "txt"))
      (:p (:input :type "submit"
              :value "add"
              :class "btn")))))

(define-url-fn (game-added)
  (let ((name (parameter "name")))
    (unless (or (null name) (zerop (length name)))
      (add-game name))
    (redirect "/retro-games.htm")))

