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
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:file "cl-ply"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
