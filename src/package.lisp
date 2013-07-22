#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply
  (:use :cl)
  (:export #:open-plyfile
           #:make-plyfile
           #:close-plyfile
           #:read-ply-element
           #:with-ply-element))
