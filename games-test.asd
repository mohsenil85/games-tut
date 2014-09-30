#|
  This file is a part of games project.
|#

(in-package :cl-user)
(defpackage games-test-asd
  (:use :cl :asdf))
(in-package :games-test-asd)

(defsystem games-test
  :author ""
  :license ""
  :depends-on (:games
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "games"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
