#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply-test
  (:use :cl
        :cl-ply
        :cl-test-more))
(in-package :cl-ply-test)

(plan nil)

(defparameter +test-pathname+
  (asdf:system-relative-pathname :cl-ply #P"t/test.ply"))


;;;
;;; test OPEN-PLYFILE/CLOSE-PLYFILE functions
;;;



;;;
;;; test WITH-PLYFILE macro
;;;



;;;
;;; test PLYFILE-ELEMENT-SIZE function
;;;

(diag "PLYFILE-ELEMENT-SIZE")

(with-plyfile (plyfile +test-pathname+)
  (is (plyfile-element-size plyfile "vertex")
      2 "basic case 1")
  (is (plyfile-element-size plyfile "face")
      2 "basic case 2"))


;;;
;;; test READ-PLY-ELEMENT function
;;;

(diag "READ-PLY-ELEMENT")

(with-plyfile (plyfile +test-pathname+)
  ;; read vertices
  (is (read-ply-element plyfile) '(0.0 1.0 2.0)
      "basic case 1")
  (is (read-ply-element plyfile) '(3.0 4.0 5.0)
      "basic case 2")
  ;; read faces
  (is (read-ply-element plyfile) '(0 1 2 3)
      "basic case 3")
  (is (read-ply-element plyfile) '(4 5 6 7)
      "basic case 4"))


;;;
;;; test WITH-PLY-ELEMENT function
;;;

(diag "WITH-PLY-ELEMENT")

(with-plyfile (plyfile +test-pathname+)
  ;; read the first vertex
  (with-ply-element ((x y z) plyfile)
    (is x 0.0 "basic case 1")
    (is y 1.0 "basic case 2")
    (is z 2.0 "basic case 3"))
  ;; read the second vertex
  (with-ply-element ((x y z) plyfile)
    (is x 3.0 "basic case 4")
    (is y 4.0 "basic case 5")
    (is z 5.0 "basic case 6"))
  ;; read the first face
  (with-ply-element (vertex-index plyfile)
    (is vertex-index '(0 1 2 3) "basic case 7"))
  (with-ply-element (vertex-index plyfile)
    (is vertex-index '(4 5 6 7) "basic case 8")))


;;;
;;; test READ-PLY-ELEMENTS function
;;;

(diag "READ-PLY-ELEMENTS")

(is (read-ply-elements +test-pathname+ "vertex")
    '((0.0 1.0 2.0) (3.0 4.0 5.0))
    "basic case 1")

(is (read-ply-elements +test-pathname+ "face")
    '((0 1 2 3) (4 5 6 7))
    "basic case 2")


;;;
;;; test MAKE-PLYFILE function
;;;



;;;
;;; test PLYFILE-CURRENT-ELEMENT-NAME function
;;;

(diag "PLYFILE-CURRENT-ELEMENT-NAME")

(with-plyfile (plyfile +test-pathname+)
  (is (cl-ply::plyfile-current-element-name plyfile) "vertex"
      "basic case 1")
  (read-ply-element plyfile)
  (read-ply-element plyfile)
  (is (cl-ply::plyfile-current-element-name plyfile) "face"
      "basic case 2"))


;;;
;;; test PLYFILE-CURRENT-ELEMENT-SIZE function
;;;

(with-plyfile (plyfile +test-pathname+)
  (is (cl-ply::plyfile-current-element-size plyfile) 2
      "basic case 1")
  (read-ply-element plyfile)
  (read-ply-element plyfile)
  (is (cl-ply::plyfile-current-element-size plyfile) 2
      "basic case 2"))


;;;
;;; test PLYFILE-READY-STATE/PLYFILE-PROCEED-STATE  funciton
;;;

(diag "PLYFILE-READY-STATE/PLYFILE-PROCEED-STATE")

(with-plyfile (plyfile +test-pathname+)
  ;; test plyfile's initial state
  (is (cl-ply::plyfile-count plyfile) 0
      "basic case 1")
  (let ((element (cl-ply::plyfile-current-element plyfile)))
    (is (cl-ply::element-name element) "vertex"
        "basic case 2"))
  ;; proceed plyfile's state and test its new state
  (cl-ply::plyfile-proceed-state plyfile)
  (is (cl-ply::plyfile-count plyfile) 1
      "basic case 3")
  (let ((element (cl-ply::plyfile-current-element plyfile)))
    (is (cl-ply::element-name element) "vertex"
        "basic case 4"))
  ;; proceed plyfile's state until its current element changes
  (cl-ply::plyfile-proceed-state plyfile)
  (is (cl-ply::plyfile-count plyfile) 0
      "basic case 5")
  (let ((element (cl-ply::plyfile-current-element plyfile)))
    (is (cl-ply::element-name element) "face"
        "basic case 6"))
  ;; proceed plyfile's state to the end
  (cl-ply::plyfile-proceed-state plyfile)
  (cl-ply::plyfile-proceed-state plyfile)
  (is (cl-ply::plyfile-current-element plyfile) nil
      "basic case 7")
  ;; ready state again and test its state
  (cl-ply::plyfile-ready-state plyfile)
  (is (cl-ply::plyfile-count plyfile) 0
      "basic case 8")
  (let ((element (cl-ply::plyfile-current-element plyfile)))
    (is (cl-ply::element-name element) "vertex"
        "basic case 9")))

(with-plyfile (plyfile +test-pathname+)
  ;; proceed plyfile's state to the end
  (cl-ply::plyfile-proceed-state plyfile)
  (cl-ply::plyfile-proceed-state plyfile)
  (cl-ply::plyfile-proceed-state plyfile)
  (cl-ply::plyfile-proceed-state plyfile)
  ;; error if proceed plyfile's state further
  (is-error (cl-ply::plyfile-proceed-state plyfile) simple-error
            "proceed plyfile's state beyond its end"))


;;;
;;; test PLYFILE-SET-FORMAT function
;;;



;;;
;;; test PLYFILE-ADD-ELEMENT function
;;;



;;;
;;; test MAKE-ELEMENT function
;;;



;;;
;;; test ELEMENT-SCALAR-PROPERTY-P function
;;;

(diag "ELEMENT-SCALAR-PROPERTY-P")

(let ((element (cl-ply::make-element '(:element "vertex" 2))))
  (is (cl-ply::element-scalar-property-p element)
      t "basic case 1"))

(let ((element (cl-ply::make-element '(:element "vertex" 2)))
      (property (cl-ply::make-property '(:property :int "x"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-scalar-property-p element)
      t "basic case 2"))

(let ((element (cl-ply::make-element '(:element "face" 2)))
      (property (cl-ply::make-property '(:property :uchar :int "vertex_index"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-scalar-property-p element)
      nil "basic case 3"))


;;;
;;; test ELEMENT-LIST-PROPERTY-P function
;;;

(diag "ELEMENT-LIST-PROPERTY-P")

(let ((element (cl-ply::make-element '(:element "vertex" 2))))
  (is (cl-ply::element-list-property-p element)
      nil "basic case 1"))

(let ((element (cl-ply::make-element '(:element "vertex" 2)))
      (property (cl-ply::make-property '(:property :int "x"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-list-property-p element)
      nil "basic case 2"))

(let ((element (cl-ply::make-element '(:element "face" 2)))
      (property (cl-ply::make-property '(:property :uchar :int "vertex_index"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-list-property-p element)
      t "basic case 3"))


;;;
;;; test ELEMENT-ADD-PROPERTY function
;;;

(diag "ELEMENT-ADD-PROPERTY")

(let ((element (cl-ply::make-element '(:element "vertex" 2)))
      (property (cl-ply::make-property '(:property :int "x"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-scalar-property-p element)
      t "basic case 1"))

(let ((element (cl-ply::make-element '(:element "face" 2)))
      (property (cl-ply::make-property '(:property :uchar :int "vertex_index"))))
  (cl-ply::element-add-property element property)
  (is (cl-ply::element-list-property-p element)
      t "basic case 2"))

(let ((element (cl-ply::make-element '(:element "vertex" 2)))
      (property1 (cl-ply::make-property '(:property :int "x")))
      (property2 (cl-ply::make-property '(:property :uchar :int "vertex_index"))))
  (cl-ply::element-add-property element property1)
  (is-error (cl-ply::element-add-property element property2) simple-error
            "can't add a list property to an element having a scalar property"))

(let ((element (cl-ply::make-element '(:element "face" 2)))
      (property1 (cl-ply::make-property '(:property :uchar :int "vertex_index")))
      (property2 (cl-ply::make-property '(:property :int "x"))))
  (cl-ply::element-add-property element property1)
  (is-error (cl-ply::element-add-property element property2) simple-error
            "can't add a scalar property to an element having a list property"))

(let ((element (cl-ply::make-element '(:element "face" 2)))
      (property1 (cl-ply::make-property '(:property :uchar :int "vertex_index")))
      (property2 (cl-ply::make-property '(:property :uchar :int "vertex_index2"))))
  (cl-ply::element-add-property element property1)
  (is-error (cl-ply::element-add-property element property2) simple-error
            "can't add another list property to an element having a list property"))


;;;
;;; test MAKE-PROPERTY function
;;;



;;;
;;; test MAKE-COMMENT function
;;;



;;;
;;; test READ-PLY-HEADER function
;;;



;;;
;;; test READ-ELEMENT function
;;;

(diag "READ-ELEMENT")

(let ((str "0.0 1.0 2.0
3.0 4.0 5.0"))
  (with-input-from-string (stream str)
    (let ((element (cl-ply::make-element '(:element "vertex" 2)))
          (property1 (cl-ply::make-property '(:property :float "x")))
          (property2 (cl-ply::make-property '(:property :float "y")))
          (property3 (cl-ply::make-property '(:property :float "z"))))
      (cl-ply::element-add-property element property1)
      (cl-ply::element-add-property element property2)
      (cl-ply::element-add-property element property3)
      (is (cl-ply::read-element stream :ascii element)
          '(0.0 1.0 2.0) "basic case 1 - scalar properties")
      (is (cl-ply::read-element stream :ascii element)
          '(3.0 4.0 5.0) "basic case 2 - scalar properties"))))

(let ((str "4 0 1 2 3
4 4 5 6 7"))
  (with-input-from-string (stream str)
    (let ((element (cl-ply::make-element '(:element "face" 2)))
          (property (cl-ply::make-property '(:property :uchar :int "vertex_index"))))
      (cl-ply::element-add-property element property)
      (is (cl-ply::read-element stream :ascii element)
          '(0 1 2 3) "basic case 3 - list property")
      (is (cl-ply::read-element stream :ascii element)
          '(4 5 6 7) "basic case 4 - list property"))))


;;;
;;; test READ-VALUE function
;;;

(diag "READ-VALUE")

;;; for integer types
(let ((str "0 1 2 -3 -4 -5"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-value stream :ascii :char) 0)
    (is (cl-ply::read-value stream :ascii :short) 1)
    (is (cl-ply::read-value stream :ascii :int) 2)
    (is (cl-ply::read-value stream :ascii :char) -3)
    (is (cl-ply::read-value stream :ascii :short) -4)
    (is (cl-ply::read-value stream :ascii :int) -5)))

;;; error if read floating point numbers
(let ((str "0.0 1.0 2.0"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :char) simple-error)
    (is-error (cl-ply::read-value stream :ascii :short) simple-error)
    (is-error (cl-ply::read-value stream :ascii :int) simple-error)))

;;; error if read strings
(let ((str "foo bar baz"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :char) simple-error)
    (is-error (cl-ply::read-value stream :ascii :short) simple-error)
    (is-error (cl-ply::read-value stream :ascii :int) simple-error)))

;;; for non negative integers
(let ((str "0 1 2"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-value stream :ascii :uchar) 0)
    (is (cl-ply::read-value stream :ascii :ushort) 1)
    (is (cl-ply::read-value stream :ascii :uint) 2)))

;;; error if read negative integers
(let ((str "-1 -2 -3"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-value stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-value stream :ascii :uint) simple-error)))

;;; error if read floating point numbers
(let ((str "1.0 2.0 3.0"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-value stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-value stream :ascii :uint) simple-error)))

;;; error if read strings
(let ((str "foo bar baz"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-value stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-value stream :ascii :uint) simple-error)))

;;; for floating point numbers
(let ((str "1.0 2.0"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-value stream :ascii :float) 1s0)
    (is (cl-ply::read-value stream :ascii :double) 2d0)))

;;; error if read strings
(let ((str "foo bar"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-value stream :ascii :float) simple-error)
    (is-error (cl-ply::read-value stream :ascii :double) simple-error)))


;;;
;;; test READ-WORD function
;;;

(diag "READ-WORD")

(let ((str "foo 1234 2.0
3.0 \"4 5\"   "))
  (with-input-from-string (stream str)
    (is (cl-ply::read-word stream) "foo")
    (is (cl-ply::read-word stream) "1234")
    (is (cl-ply::read-word stream) "2.0")
    (is (cl-ply::read-word stream) "3.0")
    (is (cl-ply::read-word stream) "\"4")
    (is (cl-ply::read-word stream) "5\"")
    (is-error (cl-ply::read-word stream) end-of-file)))


;;;
;;; test PARSE-PLY-HEADER function
;;;

(diag "PARSE-PLY-HEADER")

(is (cl-ply::parse-ply-header "ply")
    '(:magic) "basic case 1")

(is (cl-ply::parse-ply-header "format ascii 1.0")
    '(:format :ascii "1.0") "basic case 2")

(is (cl-ply::parse-ply-header "element vertex 128")
    '(:element "vertex" 128) "basic case 3")

(is (cl-ply::parse-ply-header "property float x")
    '(:property :float "x") "basic case 4")

(is (cl-ply::parse-ply-header "property list uchar int vertex_index")
    '(:property :uchar :int "vertex_index") "basic case 5")

(is (cl-ply::parse-ply-header "comment foo bar baz")
    '(:comment "foo bar baz") "basic case 6")

(is (cl-ply::parse-ply-header "end_header")
    '(:end-header) "basic case 7")


;;;
;;; test PARSE-MAGIC-HEADER function
;;;

(diag "PARSE-MAGIC-HEADER")

(is (cl-ply::parse-magic-header "ply")
    '(:magic) "basic case 1")

(is-error (cl-ply::parse-magic-header "ply ") simple-error)

(is-error (cl-ply::parse-magic-header "plya") simple-error)

(is-error (cl-ply::parse-magic-header " ply") simple-error)


;;;
;;; test PARSE-FORMAT-HEADER function
;;;

(diag "PARSE-FORMAT-HEADER")

(is (cl-ply::parse-format-header "format ascii 1.0")
    '(:format :ascii "1.0") "basic case 1")

(is (cl-ply::parse-format-header "format binary_big_endian 1.0")
    '(:format :binary-big-endian "1.0") "basic case 2")

(is (cl-ply::parse-format-header "format binary_little_endian 1.0")
    '(:format :binary-little-endian "1.0") "basic case 3")

(is (cl-ply::parse-format-header "format  ascii  1.0")
    '(:format :ascii "1.0") "basic case 4")

(is-error (cl-ply::parse-format-header "format") simple-error)

(is-error (cl-ply::parse-format-header "format ascii") simple-error)

(is-error (cl-ply::parse-format-header " format ascii 1.0") simple-error)

(is-error (cl-ply::parse-format-header "format ascii 1.0 ") simple-error)

(is-error (cl-ply::parse-format-header "formata ascii 1.0") simple-error)


;;;
;;; test PARSE-ELEMENT-HEADER function
;;;

(diag "PARSE-ELEMENT-HEADER")

(is (cl-ply::parse-element-header "element vertex 128")
    '(:element "vertex" 128) "basic case 1")

(is (cl-ply::parse-element-header "element  vertex  128")
    '(:element "vertex" 128) "basic case 2")

(is-error (cl-ply::parse-element-header "element") simple-error)

(is-error (cl-ply::parse-element-header "element vertex") simple-error)

(is-error (cl-ply::parse-element-header " element vertex 128") simple-error)

(is-error (cl-ply::parse-element-header "element vertex 128 ") simple-error)

(is-error (cl-ply::parse-element-header "elementa vertex 128") simple-error)

(is-error (cl-ply::parse-element-header "element vertex 0") simple-error)


;;;
;;; test PARSE-PROPERTY-HEADER function
;;;

(diag "PARSE-PROPERTY-HEADER")

(is (cl-ply::parse-property-header "property float x")
    '(:property :float "x") "basic case 1")

(is (cl-ply::parse-property-header "property char x")
    '(:property :char "x") "basic case 2")

(is (cl-ply::parse-property-header "property uchar x")
    '(:property :uchar "x") "basic case 3")

(is (cl-ply::parse-property-header "property short x")
    '(:property :short "x") "basic case 4")

(is (cl-ply::parse-property-header "property ushort x")
    '(:property :ushort "x") "basic case 5")

(is (cl-ply::parse-property-header "property int x")
    '(:property :int "x") "basic case 6")

(is (cl-ply::parse-property-header "property uint x")
    '(:property :uint "x") "basic case 7")

(is (cl-ply::parse-property-header "property double x")
    '(:property :double "x") "basic case 8")

(is (cl-ply::parse-property-header "property  float  x")
    '(:property :float "x") "basic case 9")

(is-error (cl-ply::parse-property-header "property foo x") simple-error)

(is-error (cl-ply::parse-property-header "property") simple-error)

(is-error (cl-ply::parse-property-header "property float") simple-error)

(is-error (cl-ply::parse-property-header " property float x") simple-error)

(is-error (cl-ply::parse-property-header "property float x ") simple-error)

(is-error (cl-ply::parse-property-header "propertya float x") simple-error)

(is (cl-ply::parse-property-header "property list uchar int vertex_index")
    '(:property :uchar :int "vertex_index") "basic case 10")

(is (cl-ply::parse-property-header "property list ushort int vertex_index")
    '(:property :ushort :int "vertex_index") "basic case 11")

(is (cl-ply::parse-property-header "property list uint int vertex_index")
    '(:property :uint :int "vertex_index") "basic case 12")

(is (cl-ply::parse-property-header "property list uchar char vertex_index")
    '(:property :uchar :char "vertex_index") "basic case 13")

(is (cl-ply::parse-property-header "property list uchar uchar vertex_index")
    '(:property :uchar :uchar "vertex_index") "basic case 14")

(is (cl-ply::parse-property-header "property list uchar short vertex_index")
    '(:property :uchar :short "vertex_index") "basic case 15")

(is (cl-ply::parse-property-header "property list uchar ushort vertex_index")
    '(:property :uchar :ushort "vertex_index") "basic case 16")

(is (cl-ply::parse-property-header "property list uchar uint vertex_index")
    '(:property :uchar :uint "vertex_index") "basic case 17")

(is (cl-ply::parse-property-header "property list uchar float vertex_index")
    '(:property :uchar :float "vertex_index") "basic case 18")

(is (cl-ply::parse-property-header "property list uchar double vertex_index")
    '(:property :uchar :double "vertex_index") "basic case 19")


;;;
;;; test PARSE-COMMENT-HEADER function
;;;

(diag "PARSE-COMMENT-HEADER")

(is (cl-ply::parse-comment-header "comment foo bar baz")
    '(:comment "foo bar baz") "basic case 1")

(is (cl-ply::parse-comment-header "comment  foo bar baz")
    '(:comment "foo bar baz") "basic case 2")


;;;
;;; test PARSE-END-HEADER function
;;;

(diag "PARSE-END-HEADER")

(is (cl-ply::parse-end-header "end_header")
    '(:end-header) "basic case 1")

(is-error (cl-ply::parse-end-header " end_header") simple-error)

(is-error (cl-ply::parse-end-header "end_header ") simple-error)


;;;
;;; test PARSE-PLY-TYPE function
;;;



;;;
;;; test %PARSE-INTEGER function
;;;

(diag "%PARSE-INTEGER")

(is (cl-ply::%parse-integer "1")
    1 "basic case 1")

(is (cl-ply::%parse-integer "-1")
    -1 "basic case 2")

(is-error (cl-ply::%parse-integer "1.0") simple-error)

(is-error (cl-ply::%parse-integer "foo") simple-error)


;;;
;;; test PARSE-NON-NEGATIVE-INTEGER function
;;;

(diag "PARSE-NON-NEGATIVE-INTEGER")

(is (cl-ply::parse-non-negative-integer "1")
    1 "basic case 1")

(is-error (cl-ply::parse-non-negative-integer "-1") simple-error)

(is-error (cl-ply::parse-non-negative-integer "1.0") simple-error)

(is-error (cl-ply::parse-non-negative-integer "foo") simple-error)


;;;
;;; test PARSE-SINGLE-FLOAT function
;;;

(diag "PARSE-SINGLE-FLOAT")

(is (cl-ply::parse-single-float "1.0")
    1.0s0 "basic case 1")

(is (cl-ply::parse-single-float "+0.1")
    +0.1s0 "basic case 2")

(is (cl-ply::parse-single-float "-0.1")
    -0.1s0 "basic case 3")

(is (cl-ply::parse-single-float "1")
    1.0s0 "basic case 4")

(is (cl-ply::parse-single-float "-1.0s0")
    -1.0s0 "basic case 5")

(is (cl-ply::parse-single-float "1s0")
    1.0s0 "basic case 6")

(is (cl-ply::parse-single-float "1S0")
    1.0s0 "basic case 7")

(is (cl-ply::parse-single-float "1s01")
    1.0s1 "basic case 8")

(is (cl-ply::parse-single-float "1s+1")
    1.0s+1 "basic case 9")

(is (cl-ply::parse-single-float "1s-1")
    1.0s-1 "basic case 10")

(is (cl-ply::parse-single-float "0.123456789012")
    0.123456789s0 "basic case 11")

(is (cl-ply::parse-single-float "1e0")
    1.0s0 "basic case 12")

(is (cl-ply::parse-single-float "1E0")
    1.0s0 "basic case 13")

(is (cl-ply::parse-single-float "1e01")
    1.0s1 "basic case 14")

(is (cl-ply::parse-single-float "1e+1")
    1.0s+1 "basic case 15")

(is (cl-ply::parse-single-float "1e-1")
    1.0s-1 "basic case 16")

(is-error (cl-ply::parse-single-float "1e") simple-error)

(is-error (cl-ply::parse-single-float "1d0") simple-error)

(is-error (cl-ply::parse-single-float "foo") simple-error)


;;;
;;; test PARSE-DOUBLE-FLOAT function
;;; 

(diag "PARSE-DOUBLE-FLOAT")

(is (cl-ply::parse-double-float "1.0")
    1.0d0 "basic case 1")

(is (cl-ply::parse-double-float "+0.1")
    +0.1d0 "basic case 2")

(is (cl-ply::parse-double-float "-0.1")
    -0.1d0 "basic case 3")

(is (cl-ply::parse-double-float "-1")
    -1.0d0 "basic case 4")

(is (cl-ply::parse-double-float "-1.0d0")
    -1.0d0 "basic case 5")

(is (cl-ply::parse-double-float "1d0")
    1.0d0 "basic case 6")

(is (cl-ply::parse-double-float "1D0")
    1.0d0 "basic case 7")

(is (cl-ply::parse-double-float "1d01")
    1.0d1 "basic case 8")

(is (cl-ply::parse-double-float "1d+1")
    1.0d+1 "basic case 9")

(is (cl-ply::parse-double-float "1d-1")
    1.0d-1 "basic case 10")

(is (cl-ply::parse-double-float "0.123456789012")
    0.123456789012d0 "basic case 11")

(is (cl-ply::parse-double-float "1e0")
    1.0d0 "basic case 11")

(is (cl-ply::parse-double-float "1E0")
    1.0d0 "basic case 12")

(is (cl-ply::parse-double-float "1e01")
    1.0d1 "basic case 13")

(is (cl-ply::parse-double-float "1e+1")
    1.0d+1 "basic case 14")

(is (cl-ply::parse-double-float "1e-1")
    1.0d-1 "basic case 15")

(is-error (cl-ply::parse-double-float "1e") simple-error)

(is-error (cl-ply::parse-double-float "1s0") simple-error)

(is-error (cl-ply::parse-double-float "foo") simple-error)


(finalize)
