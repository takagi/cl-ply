#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply-test-asd
  (:use :cl :asdf))
(in-package :cl-ply-test-asd)

(defsystem cl-ply-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:cl-ply
               :prove)
  :components ((:module "t"
                :serial t
                :components
                ((:test-file "cl-ply"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
