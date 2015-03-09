#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply-test
  (:use :cl
        :cl-ply
        :prove))
(in-package :cl-ply-test)

(plan nil)

(defparameter +test-pathname+
  (asdf:system-relative-pathname :cl-ply #P"t/test.ply"))


;;;
;;; test PLY-OPEN-FOR-READING/PLY-CLOSE functions
;;;


;;;
;;; test WITH-PLY-FOR-READING macro
;;;


;;;
;;; test PLY-ELEMENT-NAMES function
;;;

(diag "PLY-ELEMENT-NAMES")

(with-ply-for-reading (plyfile +test-pathname+)
  (is (ply-element-names plyfile)
      '("vertex" "face")))


;;;
;;; test PLY-ELEMENT-SIZE function
;;;

(diag "PLY-ELEMENT-SIZE")

(with-ply-for-reading (plyfile +test-pathname+)
  (is (ply-element-size plyfile "vertex") 2)
  (is (ply-element-size plyfile "face") 2))


;;;
;;; test PLY-READ-ELEMENT function
;;;

(diag "PLY-READ-ELEMENT")

(with-ply-for-reading (plyfile +test-pathname+)
  (is (ply-read-element plyfile "vertex") '(0.0 1.0 2.0))
  (is (ply-read-element plyfile "vertex") '(3.0 4.0 5.0))
  (is (ply-read-element plyfile "face") '((0 1 2 3)))
  (is (ply-read-element plyfile "face") '((4 5 6 7))))

(with-ply-for-reading (plyfile +test-pathname+)
  (is-error (ply-read-element plyfile "face") 'simple-error))


;;;
;;; test PLY-COMMENTS function
;;;

(diag "PLY-COMMENTS")

(with-ply-for-reading (plyfile +test-pathname+)
  (is (ply-comments plyfile) '("This is test ply data.")))


;;;
;;; test PLY-OBJ-INFO function
;;;

(diag "PLY-OBJ-INFO")

(with-ply-for-reading (plyfile +test-pathname+)
  (is (ply-obj-info plyfile) '("This is object information.")))


;;;
;;; test MAKE-PLYFILE function
;;;


;;;
;;; test PLYFILE-ELEMENT-BY-NAME function
;;;


;;;
;;; test PLYFILE-SET-FORMAT function
;;;


;;;
;;; test PLYFILE-ADD-ELEMENT function
;;;


;;;
;;; test PLYFILE-ADD-COMMENT function
;;;


;;;
;;; test PLYFILE-ADD-OBJ-INFO function
;;;



;;;
;;; test MAKE-ELEMENT function
;;;


;;;
;;; test ELEMENT-ADD-PROPERTY function
;;;


;;;
;;; test MAKE-PROPERTY function
;;;


;;;
;;; test MAKE-COMMENT function
;;;


;;;
;;; test MAKE-OBJ-INFO function
;;;


;;;
;;; test PARSE-PLY-FILE-TYPE function
;;;

(is (cl-ply::parse-ply-file-type "ascii") :ascii)

(is (cl-ply::parse-ply-file-type "binary_big_endian") :binary-big-endian)

(is (cl-ply::parse-ply-file-type "binary_little_endian") :binary-little-endian)

(is-error (cl-ply::parse-ply-file-type "foo") 'simple-error)


;;;
;;; test PARSE-INTEGER% function
;;;

(is (cl-ply::parse-integer% "1") 1)

(is (cl-ply::parse-integer% "-1") -1)

(is-error (cl-ply::parse-integer% "1.0") 'simple-error)

(is-error (cl-ply::parse-integer% "foo") 'simple-error)


;;;
;;; test PARSE-UNSIGNED-INTEGER function
;;;

(is (cl-ply::parse-unsigned-integer "1") 1)

(is-error (cl-ply::parse-unsigned-integer "-1") 'simple-error)


;;;
;;; test PARSE-SINGLE-FLOAT function
;;;

(is (cl-ply::parse-single-float "1.0") 1.0)

(is (cl-ply::parse-single-float "-1.0") -1.0)

(is (cl-ply::parse-single-float "1.0e0") 1.0)

(is (cl-ply::parse-single-float "1.0d0") 1.0)

(is-error (cl-ply::parse-single-float "foo") 'simple-error)


;;;
;;; test PARSE-DOUBLE-FLOAT function
;;;

(is (cl-ply::parse-double-float "1.0") 1.0d0)

(is (cl-ply::parse-double-float "1.0e0") 1.0d0)

(is (cl-ply::parse-double-float "1.0s0") 1.0d0)

(is (cl-ply::parse-double-float "1.0d0") 1.0d0)


;;;
;;; test PARSE-VALUE function
;;;


;;;
;;; test PARSE-HEADER function
;;;

(diag "PARSE-HEADER")

(is (cl-ply::parse-header "ply") '(:ply))

(is (cl-ply::parse-header "format ascii 1.0") '(:format :ascii 1.0))

(is (cl-ply::parse-header "element vertex 128") '(:element "vertex" 128))

(is (cl-ply::parse-header "property float x") '(:property :float "x"))

(is (cl-ply::parse-header "property list uchar int vertex_indices")
    '(:property :uchar :int "vertex_indices"))

(is (cl-ply::parse-header "comment foo bar baz") '(:comment "foo bar baz"))

(is (cl-ply::parse-header "obj_info foo bar baz") '(:obj-info "foo bar baz"))

(is (cl-ply::parse-header "end_header") '(:end-header))


;;;
;;; test READ-HEADER function
;;;


;;;
;;; test READ-ELEMENT function
;;;


;;;
;;; test READ-ELEMENT-ASCII function
;;;


;;;
;;; test READ-ELEMENT-BIG-ENDIAN function
;;;


;;;
;;; test READ-ELEMENT-LITTLE-ENDIAN function
;;;









;;;
;;; test PARSE-PLY-HEADER function
;;;

(diag "PARSE-PLY-HEADER")

(is (cl-ply::parse-header "ply")
    '(:ply) "basic case 1")

(is (cl-ply::parse-header "format ascii 1.0")
    '(:format :ascii 1.0) "basic case 2")

(is (cl-ply::parse-header "element vertex 128")
    '(:element "vertex" 128) "basic case 3")

(is (cl-ply::parse-header "property float x")
    '(:property :float "x") "basic case 4")

(is (cl-ply::parse-header "property list uchar int vertex_index")
    '(:property :uchar :int "vertex_index") "basic case 5")

(is (cl-ply::parse-header "comment foo bar baz")
    '(:comment "foo bar baz") "basic case 6")

(is (cl-ply::parse-header "end_header")
    '(:end-header) "basic case 7")


(finalize)
