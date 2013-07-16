#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply
  (:use :cl)
  (:export #:ply-open-for-reading
           #:ply-close
           #:ply-read
           #:with-ply-element
           #:read-ply-element))
