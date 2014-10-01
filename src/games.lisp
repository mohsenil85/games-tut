(in-package :cl)
(ql:quickload "postmodern")
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")
(ql:quickload "parenscript")
(defpackage games
  (:use :cl 
        :cl-who 
        :hunchentoot 
        :postmodern
        :parenscript))
(in-package :games)

(defclass game ()
  ((name  :col-type string 
          :reader name
          :initarg :name)
   (votes :accessor votes
          :col-type integer
          :initform 0))
  (:metaclass  dao-class)
  (:keys name))


(connect-toplevel "lmohseni" "lmohseni" "lmohseni" "localhost")
(setf srv (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start srv )
(hunchentoot:stop srv)
;(execute (dao-table-definition 'game ))
(setf (html-mode) :html5)

(defmethod vote-for (user-selected-game)
  (let ((g (get-dao 'game user-selected-game)))
    (incf (votes g)) 
    (update-dao g) ))

(defun game-from-name (name)
  (query (:select '* :from 'game :where (:like 'name name))) )

(defun game-stored? (game-name)
  (game-from-name game-name))


(defun games ()
  (query "select * from game order by votes desc"))

(defun add-game (name)
  (unless (game-stored? name)
    (insert-dao (make-instance 'game :name name) )))

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



(defmacro define-url-fn ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher ,(format nil "/~(~a~).htm" name ) ',name) *dispatch-table*))) 


(dolist ( game(games))
  (format t "~a~%" (car game)))

(with-html-output (dolist (game (games))
                    (:li 
                      (:a :href (format nil "vote.htm?name=~a" (car game)) "VOTE!")
                      (fmt "~A with ~d votes" (name game) (cdr game)))))



(define-url-fn (retro-games)
  (standard-page (:title "top retro games")
    (:h1 "vote on your favorite")
    (:p "missing a game?  add it " (:a :href "new-game.htm" "here"))
    (:div :id "chart"
     (:ol
       (dolist (game (games))
         (htm
           (:li
             (:a :href (format nil "vote.htm?name=~a" (car game)) "VOTE!")
             (fmt "~A with ~d votes" (car game) (cdr game)))))))))

(define-url-fn (vote)
  (let ((game (parameter "name")))
    (if (game-stored? game)
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

