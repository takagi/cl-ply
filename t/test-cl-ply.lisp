#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-ply-test)

(plan nil)


;;;
;;; test Plyfile
;;;

(diag "test Plyfile")

(defparameter +test-ply-data+
  "ply
format ascii 1.0
comment this is test ply data.
element vertex 2
property float x
property float y
property float z
element face 2
property list uchar int vertex_index
end_header
0.0 1.0 2.0
3.0 4.0 5.0
4 0 1 2 3
4 4 5 6 7")

;;; test OPEN-PLYFILE function
(let ((path (asdf:system-relative-pathname :cl-ply #P"t/test.ply")))
  (ok (cl-ply:open-plyfile path)))

;;; test MAKE-PLYFILE function
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    ;; test PLY header
    (is (cl-ply::plyfile-file-type plyfile) :ascii)
    ;; test FORMAT header
    (is (cl-ply::plyfile-version plyfile) "1.0")
    ;; test COMMENT header
    (let ((comment (first (cl-ply::plyfile-comments plyfile))))
      (is (cl-ply::comment-text comment) "this is test ply data."))
    ;; test "vertex" ELEMENT header
    (let ((element (first (cl-ply::plyfile-elements plyfile))))
      (is (cl-ply::element-name element) "vertex")
      (is (cl-ply::element-size element) 2))
    ;; test PROPERTY header of "vertex" element
    (let ((element (first (cl-ply::plyfile-elements plyfile))))
      (destructuring-bind (prop-x prop-y prop-z)
          (cl-ply::element-properties element)
        (ok (cl-ply::scalar-property-p prop-x))
        (is (cl-ply::scalar-property-type prop-x) :float)
        (is (cl-ply::scalar-property-name prop-x) "x")
        (ok (cl-ply::scalar-property-p prop-y))
        (is (cl-ply::scalar-property-type prop-y) :float)
        (is (cl-ply::scalar-property-name prop-y) "y")
        (ok (cl-ply::scalar-property-p prop-z))
        (is (cl-ply::scalar-property-type prop-z) :float)
        (is (cl-ply::scalar-property-name prop-z) "z")))
    ;; test "face" ELEMENT header
    (let ((element (second (cl-ply::plyfile-elements plyfile))))
      (is (cl-ply::element-name element) "face")
      (is (cl-ply::element-size element) 2))
    ;; test PROPERTY header of "face" element
    (let ((element (second (cl-ply::plyfile-elements plyfile))))
      (let ((property (first (cl-ply::element-properties element))))
        (ok (cl-ply::list-property-p property))
        (is (cl-ply::list-property-count-type property) :uchar)
        (is (cl-ply::list-property-element-type property) :int)
        (is (cl-ply::list-property-name property) "vertex_index")))))

;;; PROPERTY header must follow ELEMENT header
(let ((str "ply
format ascii 1.0
property float x"))
  (with-input-from-string (stream str)
    (is-error (cl-ply:make-plyfile stream) simple-error)))

;;; test WITH-PLYFILE macro
(let ((path (asdf:system-relative-pathname :cl-ply #P"t/test.ply")))
  (cl-ply:with-plyfile (plyfile path)
    (ok (cl-ply:read-ply-element "vertex" plyfile))
    (ok (cl-ply:read-ply-element "face" plyfile))))

;;; test WITH-PLY-ELEMENT macro
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (let (result)
      (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
        (push (list x y z) result))
      (is result '((3.0 4.0 5.0) (0.0 1.0 2.0))))
    (let (result)
      (cl-ply:with-ply-element (vertex_indices "face" plyfile)
        (push vertex_indices result))
      (is result '((4 5 6 7) (0 1 2 3))))))

;;; error if variable names do not match corresponding properties
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (is-error (cl-ply:with-ply-element (x "vertex" plyfile)
                (declare (ignore x)))
              simple-error)))
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (is-error (cl-ply:with-ply-element ((x y) "vertex" plyfile)
                (declare (ignore x y)))
              error)))
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (read-ply-element "vertex" plyfile) ; consume "vertex" element
    (is-error (cl-ply:with-ply-element ((x) "face" plyfile)
                (declare (ignore x)))
              simple-error)))

;;; error if trying to read other elements than ready to be read
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                (declare (ignore foo)))
              simple-error)))

;;; error if trying to read element while reading another
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (declare (ignore x y z))
      (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                  (declare (ignore foo)))
                simple-error))))

;;; error if trying to read element after finished reading all
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (declare (ignore x y z)))
    (cl-ply:with-ply-element (vertex_indices "face" plyfile)
      (declare (ignore vertex_indices)))
    (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                (declare (ignore foo)))
              simple-error)))

;;; test READ-PLY-ELEMENT function
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (is (cl-ply:read-ply-element "vertex" plyfile)
        '((0.0 1.0 2.0) (3.0 4.0 5.0)))
    (is (cl-ply:read-ply-element "face" plyfile)
        '((0 1 2 3) (4 5 6 7)))))

;;; error if trying to read element other than ready to be read
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error)))

;;; error if trying to read element while reading one
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (declare (ignore x y z))
      (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error))))

;;; error if trying to read element after finished reading all
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    (cl-ply:read-ply-element "vertex" plyfile)
    (cl-ply:read-ply-element "face" plyfile)
    (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error)))


;;;
;;; test Plyfile - Element selectors
;;;

(diag "test Plyfile - Element selectors")

;;; test PLYFILE-ELEMENT function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  (let ((header (cl-ply::make-element-header "element vertex 128")))
    (cl-ply::plyfile-add-element plyfile header)
    (let ((element (cl-ply::plyfile-element plyfile "vertex")))
      (is (cl-ply::element-name element) "vertex"))
    (ok (null (cl-ply::plyfile-element plyfile "face")))))

;;; test PLYFILE-ELEMENT-NAMES function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  (let ((header1 (cl-ply::make-element-header "element vertex 128"))
        (header2 (cl-ply::make-element-header "element face 128")))
    (cl-ply::plyfile-add-element plyfile header1)
    (cl-ply::plyfile-add-element plyfile header2)
    (is (cl-ply::plyfile-element-names plyfile) '("vertex" "face"))))

;;; test PLYFILE-CURRENT-ELEMENT function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  (let ((header1 (cl-ply::make-element-header "element vertex 128"))
        (header2 (cl-ply::make-element-header "element face 128")))
    (cl-ply::plyfile-add-element plyfile header1)
    (cl-ply::plyfile-add-element plyfile header2)
    (cl-ply::plyfile-initialize-state plyfile)
    (let ((element (cl-ply::plyfile-current-element plyfile)))
      (is (cl-ply::element-name element) "vertex"))
    (cl-ply::transfer-state (cl-ply::plyfile-state plyfile)) ; -> :reading
    (cl-ply::transfer-state (cl-ply::plyfile-state plyfile)) ; -> :ready
    (let ((element (cl-ply::plyfile-current-element plyfile)))
      (is (cl-ply::element-name element) "face"))
    (cl-ply::transfer-state (cl-ply::plyfile-state plyfile)) ; -> :reading
    (cl-ply::transfer-state (cl-ply::plyfile-state plyfile)) ; -> ;finish
    (is-error (cl-ply::plyfile-current-element plyfile) simple-error)))


;;;
;;; test Plyfile - Plyfile builder
;;;

(diag "test Plyfile - Plyfile builder")

;;; test PLYFILE-SET-FORMAT function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  (let ((header (cl-ply::make-format-header "format ascii 1.0")))
    (cl-ply::plyfile-set-format plyfile header))
  (is (cl-ply::plyfile-file-type plyfile) :ascii)
  (is (cl-ply::plyfile-version   plyfile) "1.0"))

;;; test PLYFILE-ADD-ELEMENT function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  ;; plyfile has no elements initially
  (ok (null (cl-ply::plyfile-elements plyfile)))
  ;; add element
  (let ((header (cl-ply::make-element-header "element vertex 128")))
    (cl-ply::plyfile-add-element plyfile header))
  ;; test the first element
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (is (cl-ply::element-name element) "vertex")
    (is (cl-ply::element-size element) 128))
  ;; add another element
  (let ((header (cl-ply::make-element-header "element face 128")))
    (cl-ply::plyfile-add-element plyfile header))
  ;; test order of elements
  (is (mapcar #'cl-ply::element-name (cl-ply::plyfile-elements plyfile))
      '("vertex" "face"))
  ;; test the second element
  (let ((element (second (cl-ply::plyfile-elements plyfile))))
    (is (cl-ply::element-name element) "face")
    (is (cl-ply::element-size element) 128))
  ;; adding duplicated element causes error
  (let ((header (cl-ply::make-element-header "element face 128")))
    (is-error (cl-ply::plyfile-add-element plyfile header) simple-error)))

;;; test PLYFILE-ADD-PROPERTY function with scalar property
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  ;; plyfile has no elements initially and adding property causes error
  (let ((header (cl-ply::make-property-header "property float x")))
    (is-error (cl-ply::plyfile-add-property plyfile nil header) simple-error))
  ;; add element
  (let ((header (cl-ply::make-element-header "element vertex 128")))
    (cl-ply::plyfile-add-element plyfile header))
  ;; element has no properties initially
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (ok (null (cl-ply::element-properties element))))
  ;; add property to element
  (let ((header (cl-ply::make-property-header "property float x")))
    (cl-ply::plyfile-add-property plyfile "vertex" header))
  ;; test first property
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (let ((property (first (cl-ply::element-properties element))))
      (is (cl-ply::scalar-property-type property) :float)
      (is (cl-ply::scalar-property-name property) "x")))
  ;; add another property to element
  (let ((header (cl-ply::make-property-header "property float y")))
    (cl-ply::plyfile-add-property plyfile "vertex" header))
  ;; test order of properties
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (is (mapcar #'cl-ply::scalar-property-name
                (cl-ply::element-properties element))
        '("x" "y")))
  ;; test second property
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (let ((property (second (cl-ply::element-properties element))))
      (is (cl-ply::scalar-property-type property) :float)
      (is (cl-ply::scalar-property-name property) "y")))
  ;; adding duplicated property causes error
  (let ((header (cl-ply::make-property-header "property float y")))
    (is-error (cl-ply::plyfile-add-property plyfile "vertex" header)
              simple-error))
  ;; error if current element does not exist
  (let ((header (cl-ply::make-property-header "property float x")))
    (is-error (cl-ply::plyfile-add-property plyfile "foo" header)
              simple-error)))

;;; test PLYFILE-ADD-PROPERTY function with list property
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  ;; add element
  (let ((header (cl-ply::make-element-header "element face 128")))
    (cl-ply::plyfile-add-element plyfile header))
  ;; add property to element
  (let ((header (cl-ply::make-property-header
                    "property list uchar int vertex_index")))
    (cl-ply::plyfile-add-property plyfile "face" header))
  ;; test first property
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (let ((property (first (cl-ply::element-properties element))))
      (is (cl-ply::list-property-count-type property) :uchar)
      (is (cl-ply::list-property-element-type property) :int)
      (is (cl-ply::list-property-name property) "vertex_index")))
  ;; adding another property to element causes error
  (let ((header (cl-ply::make-property-header "property list uchar int foo")))
    (is-error (cl-ply::plyfile-add-property plyfile "face" header)
              simple-error)))

;;; test PLYFILE-ADD-COMMENT function
(let ((plyfile (cl-ply::%make-plyfile :stream nil)))
  ;; plyfile has no comments initially
  (ok (null (cl-ply::plyfile-comments plyfile)))
  ;; add comment
  (let ((header (cl-ply::make-comment-header "comment foo bar")))
    (cl-ply::plyfile-add-comment plyfile header))
  ;; test first comment
  (let ((comment (first (cl-ply::plyfile-comments plyfile))))
    (is (cl-ply::comment-text comment) "foo bar"))
  ;; add another comment
  (let ((header (cl-ply::make-comment-header "comment  foo bar baz")))
    (cl-ply::plyfile-add-comment plyfile header))
  ;; test order of comments
  (is (mapcar #'cl-ply::comment-text (cl-ply::plyfile-comments plyfile))
      '("foo bar" "foo bar baz"))
  ;; test second comment
  (let ((comment (second (cl-ply::plyfile-comments plyfile))))
    (is (cl-ply::comment-text comment) "foo bar baz")))


;;;
;;; test Plyfile - State management
;;;


;;;
;;; test Plyfile - Properties reader
;;;

(diag "test Plyfile - Properties reader")

;;; test PLYFILE-READ-PROPERTIES
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:make-plyfile stream)))
    ;; error if not in :reading state
    (is-error (cl-ply::plyfile-read-properties plyfile) simple-error)
    ;; :ready -> :reading
    (let ((state (cl-ply::plyfile-state plyfile)))
      (cl-ply::transfer-state state))
    ;; work in :reading state
    (is (cl-ply::plyfile-read-properties plyfile) '(0.0 1.0 2.0))))

;;; test READ-PROPERTIES function for scalar properties element
(let ((str "0.0 1.0 2.0
3.0 4.0 5.0"))
  (with-input-from-string (stream str)
    (let ((header0 (cl-ply::make-ply-header "element vertex 2"))
          (header1 (cl-ply::make-ply-header "property float x"))
          (header2 (cl-ply::make-ply-header "property float y"))
          (header3 (cl-ply::make-ply-header "property float z")))
      (let ((element (cl-ply::make-element header0)))
        (cl-ply::element-add-property element header1)
        (cl-ply::element-add-property element header2)
        (cl-ply::element-add-property element header3)
        (is (cl-ply::read-properties stream :ascii element) '(0.0 1.0 2.0))
        (is (cl-ply::read-properties stream :ascii element) '(3.0 4.0 5.0))))))

;;; test READ-PROPERTIES function for list properties element
(let ((str "4 0 1 2 3
4 4 5 6 7"))
  (with-input-from-string (stream str)
    (let ((header0 (cl-ply::make-ply-header "element face 2"))
          (header1 (cl-ply::make-ply-header
                       "property list uchar int vertex_index")))
      (let ((element (cl-ply::make-element header0)))
        (cl-ply::element-add-property element header1)
        (is (cl-ply::read-properties stream :ascii element) '(0 1 2 3))
        (is (cl-ply::read-properties stream :ascii element) '(4 5 6 7))))))

;;; test READ-PROPERTY function for integer types
(let ((str "0 1 2 -3 -4 -5"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :char) 0)
    (is (cl-ply::read-property stream :ascii :short) 1)
    (is (cl-ply::read-property stream :ascii :int) 2)
    (is (cl-ply::read-property stream :ascii :char) -3)
    (is (cl-ply::read-property stream :ascii :short) -4)
    (is (cl-ply::read-property stream :ascii :int) -5)))

;;; error if trying to read floating point numbers
(let ((str "0.0 1.0 2.0"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :char) simple-error)
    (is-error (cl-ply::read-property stream :ascii :short) simple-error)
    (is-error (cl-ply::read-property stream :ascii :int) simple-error)))

;;; error if trying to read strings
(let ((str "foo bar baz"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :char) simple-error)
    (is-error (cl-ply::read-property stream :ascii :short) simple-error)
    (is-error (cl-ply::read-property stream :ascii :int) simple-error)))

;;; test READ-PROPERTY function for non negative integers
(let ((str "0 1 2"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :uchar) 0)
    (is (cl-ply::read-property stream :ascii :ushort) 1)
    (is (cl-ply::read-property stream :ascii :uint) 2)))

;;; error if trying to read negative integers
(let ((str "-1 -2 -3"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-property stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-property stream :ascii :uint) simple-error)))

;;; error if trying to read floating point numbers
(let ((str "1.0 2.0 3.0"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-property stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-property stream :ascii :uint) simple-error)))

;;; error if trying to read strings
(let ((str "foo bar baz"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uchar) simple-error)
    (is-error (cl-ply::read-property stream :ascii :ushort) simple-error)
    (is-error (cl-ply::read-property stream :ascii :uint) simple-error)))

;;; test READ-PROPERTY function for floating point numbers
(let ((str "1.0 2.0"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :float) 1s0)
    (is (cl-ply::read-property stream :ascii :double) 2d0)))

;;; error if trying to read strings
(let ((str "foo bar"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :float) simple-error)
    (is-error (cl-ply::read-property stream :ascii :double) simple-error)))

;;; test READ-WORD function
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

;;; test READ-PROPERTY-BINARY-BE function

;;; test READ-PROPERTY-BINARY-LE function


;;;
;;; test Element
;;;

(diag "test Element")

;;; element with scalar property can have scalar properties only
(let ((element (cl-ply::make-element
                (cl-ply::make-ply-header "element vertex 1"))))
  (let ((prop-header1 (cl-ply::make-ply-header "property float x"))
        (prop-header2 (cl-ply::make-ply-header "property float y"))
        (prop-header3 (cl-ply::make-ply-header "property list uchar int foo")))
    (cl-ply::element-add-property element prop-header1)
    (cl-ply::element-add-property element prop-header2)
    (is-error (cl-ply::element-add-property element prop-header3)
              simple-error)))

;;; element can have one list property at most
(let ((element (cl-ply::make-element
                (cl-ply::make-ply-header "element vertex 1"))))
  (let ((prop-header1 (cl-ply::make-ply-header "property list uchar int foo"))
        (prop-header2 (cl-ply::make-ply-header "property float x"))
        (prop-header3 (cl-ply::make-ply-header "property list uchar int bar")))
    (cl-ply::element-add-property element prop-header1)
    (is-error (cl-ply::element-add-property element prop-header2)
              simple-error)
    (is-error (cl-ply::element-add-property element prop-header3)
              simple-error)))


;;;
;;; test Property
;;;


;;;
;;; test Comment
;;;


;;;
;;; test State
;;;

(diag "test State")

;;; test MAKE-STATE function
(let ((state (cl-ply::make-state '("vertex" "face"))))
  (ok (cl-ply::state-ready-p state "vertex")))
(let ((state (cl-ply::make-state '())))
  (ok (cl-ply::state-finish-p state)))
(is-error (cl-ply::make-state '(1)) simple-type-error)
(is-error (cl-ply::make-state '(nil)) simple-type-error)

;;; test TRANSFER-STATE function
(let ((state (cl-ply::make-state '("vertex" "face"))))
  (ok (cl-ply::state-ready-p state "vertex"))
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-reading-p state "vertex"))
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-ready-p state "face"))
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-reading-p state "face"))
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-finish-p state))
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-finish-p state)))

;;; test STATE-NAME function

;;; test STATE-CURRENT-ELEMENT-NAME function

;;; test STATE-READY-P function
(let ((state (cl-ply::make-state '("vertex"))))
  ;; currently (:ready "vertex")
  (ok (cl-ply::state-ready-p state "vertex"))
  (ok (not (cl-ply::state-ready-p state "face")))
  ;; currently (:reading "vertex")
  (cl-ply::transfer-state state)
  (ok (not (cl-ply::state-ready-p state "vertex")))
  ;; currently :finish
  (cl-ply::transfer-state state)
  (ok (not (cl-ply::state-ready-p state "vertex"))))

;;; test STATE-READING-P function
(let ((state (cl-ply::make-state '("vertex"))))
  ;; currently (:ready "vertex")
  (ok (not (cl-ply::state-reading-p state "vertex")))
  ;; currently (:reading "vertex")
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-reading-p state "vertex"))
  (ok (not (cl-ply::state-reading-p state "face")))
  ;; currently :finish
  (cl-ply::transfer-state state)
  (ok (not (cl-ply::state-reading-p state "vertex"))))

;;; test STATE-FINISH-P function
(let ((state (cl-ply::make-state '("vertex"))))
  ;; currently (:ready "vertex")
  (ok (not (cl-ply::state-finish-p state)))
  ;; currently (:reading "vertex")
  (cl-ply::transfer-state state)
  (ok (not (cl-ply::state-finish-p state)))
  ;; currently :finish
  (cl-ply::transfer-state state)
  (ok (cl-ply::state-finish-p state)))


;;;
;;; test making PLY headers
;;;

(diag "test making PLY headers")

(ok (cl-ply::magic-header-p    (cl-ply::make-ply-header "ply")))
(ok (cl-ply::format-header-p   (cl-ply::make-ply-header "format ascii 1.0")))
(ok (cl-ply::element-header-p  (cl-ply::make-ply-header "element vertex 128")))
(ok (cl-ply::property-header-p (cl-ply::make-ply-header "property float x")))
(ok (cl-ply::property-header-p (cl-ply::make-ply-header "property list uchar int vertex_index")))
(ok (cl-ply::comment-header-p  (cl-ply::make-ply-header "comment foo bar baz")))
(ok (cl-ply::end-header-p      (cl-ply::make-ply-header "end_header")))


;;;
;;; test Magic header
;;;

(diag "test Magic header")

(ok (cl-ply::make-magic-header "ply"))
(is-error (cl-ply::make-magic-header "ply ") simple-error)
(is-error (cl-ply::make-magic-header "plya") simple-error)
(is-error (cl-ply::make-magic-header " ply") simple-error)


;;;
;;; test Format header
;;;

(diag "test Format header")

(let ((header (cl-ply::make-format-header "format ascii 1.0")))
  (is (cl-ply::format-header-file-type header) :ascii)
  (is (cl-ply::format-header-version   header) "1.0"))
(let ((header (cl-ply::make-format-header "format binary_big_endian 1.0")))
  (is (cl-ply::format-header-file-type header) :binary-big-endian)
  (is (cl-ply::format-header-version   header) "1.0"))
(let ((header (cl-ply::make-format-header "format binary_little_endian 1.0")))
  (is (cl-ply::format-header-file-type header) :binary-little-endian)
  (is (cl-ply::format-header-version   header) "1.0"))
(ok (cl-ply::make-format-header "format  ascii  1.0"))
(is-error (cl-ply::make-format-header "format") simple-error)
(is-error (cl-ply::make-format-header "format ascii") simple-error)
(is-error (cl-ply::make-format-header " format ascii 1.0") simple-error)
(is-error (cl-ply::make-format-header "format ascii 1.0 ") simple-error)
(is-error (cl-ply::make-format-header "formata ascii 1.0") simple-error)


;;;
;;; test Element header
;;;

(diag "test Element header")

(let ((header (cl-ply::make-element-header "element vertex 128")))
  (is (cl-ply::element-header-name header) "vertex")
  (is (cl-ply::element-header-size header) 128))
(ok (cl-ply::make-element-header "element  vertex  128"))
(is-error (cl-ply::make-element-header "element") simple-error)
(is-error (cl-ply::make-element-header "element vertex") simple-error)
(is-error (cl-ply::make-element-header " element vertex 128") simple-error)
(is-error (cl-ply::make-element-header "element vertex 128 ") simple-error)
(is-error (cl-ply::make-element-header "elementa vertex 128 ") simple-error)
(is-error (cl-ply::make-element-header "element vertex 0") simple-error)


;;;
;;; test Property header
;;;

(diag "test Property header")

;;; test scalar property
(let ((header (cl-ply::make-property-header "property float x")))
  (is (cl-ply::scalar-property-header-type header) :float)
  (is (cl-ply::scalar-property-header-name header) "x"))
(ok (cl-ply::make-property-header "property char x"))
(ok (cl-ply::make-property-header "property uchar x"))
(ok (cl-ply::make-property-header "property short x"))
(ok (cl-ply::make-property-header "property ushort x"))
(ok (cl-ply::make-property-header "property int x"))
(ok (cl-ply::make-property-header "property uint x"))
(ok (cl-ply::make-property-header "property double x"))
(ok (cl-ply::make-property-header "property  float  x"))
(is-error (cl-ply::make-property-header "property foo x") simple-error)
(is-error (cl-ply::make-property-header "property") simple-error)
(is-error (cl-ply::make-property-header "property float") simple-error)
(is-error (cl-ply::make-property-header " property float x") simple-error)
(is-error (cl-ply::make-property-header "property float x ") simple-error)
(is-error (cl-ply::make-property-header "propertya float x") simple-error)

;;; test list property
(let ((header (cl-ply::make-property-header "property list uchar int vertex_index")))
  (is (cl-ply::list-property-header-count-type header) :uchar)
  (is (cl-ply::list-property-header-element-type header) :int)
  (is (cl-ply::list-property-header-name header) "vertex_index"))
(ok (cl-ply::make-property-header "property list ushort int vertex_index"))
(ok (cl-ply::make-property-header "property list uint int vertex_index"))
(ok (cl-ply::make-property-header "property list uchar char vertex_index"))
(ok (cl-ply::make-property-header "property list uchar uchar vertex_index"))
(ok (cl-ply::make-property-header "property list uchar short vertex_index"))
(ok (cl-ply::make-property-header "property list uchar ushort vertex_index"))
(ok (cl-ply::make-property-header "property list uchar uint vertex_index"))
(ok (cl-ply::make-property-header "property list uchar float vertex_index"))
(ok (cl-ply::make-property-header "property list uchar double vertex_index"))


;;;
;;; test Comment header
;;;

(diag "test Comment header")

(let ((header (cl-ply::make-comment-header "comment foo bar baz")))
  (is (cl-ply::comment-header-text header) "foo bar baz"))
(let ((header (cl-ply::make-comment-header "comment  foo bar baz")))
  (is (cl-ply::comment-header-text header) "foo bar baz"))


;;;
;;; test End header
;;;

(diag "test End header")

(ok (cl-ply::make-end-header "end_header"))
(is-error (cl-ply::make-end-header " end_header") simple-error)
(is-error (cl-ply::make-end-header "end_header ") simple-error)


;;;
;;; test parsing for types in PLY format
;;;

(diag "test parsing for types in PLY format")

;;; test %PARSE-INTEGER function
(is (cl-ply::%parse-integer "1") 1)
(is (cl-ply::%parse-integer "-1") -1)
(is-error (cl-ply::%parse-integer "1.0") simple-error)
(is-error (cl-ply::%parse-integer "foo") simple-error)

;;; test PARSE-NON-NEGATIVE-INTEGER function
(is (cl-ply::parse-non-negative-integer "1") 1)
(is-error (cl-ply::parse-non-negative-integer "-1") simple-error)
(is-error (cl-ply::parse-non-negative-integer "1.0") simple-error)
(is-error (cl-ply::parse-non-negative-integer "foo") simple-error)

;;; test PARSE-SINGLE-FLOAT function
(is (cl-ply::parse-single-float "1.0") 1.0s0)
(is (cl-ply::parse-single-float "+0.1") +0.1s0)
(is (cl-ply::parse-single-float "-0.1") -0.1s0)
(is (cl-ply::parse-single-float "1") 1.0s0)
(is (cl-ply::parse-single-float "-1.0s0") -1.0s0)
(is (cl-ply::parse-single-float "1s0") 1.0s0)
(is (cl-ply::parse-single-float "1S0") 1.0s0)
(is (cl-ply::parse-single-float "1s01") 1.0s1)
(is (cl-ply::parse-single-float "1s+1") 1.0s+1)
(is (cl-ply::parse-single-float "1s-1") 1.0s-1)
(is (cl-ply::parse-single-float "0.123456789012") 0.123456789s0)
(is (cl-ply::parse-single-float "1e0") 1.0s0)
(is (cl-ply::parse-single-float "1E0") 1.0s0)
(is (cl-ply::parse-single-float "1e01") 1.0s1)
(is (cl-ply::parse-single-float "1e+1") 1.0s+1)
(is (cl-ply::parse-single-float "1e-1") 1.0s-1)
(is-error (cl-ply::parse-single-float "1e") simple-error)
(is-error (cl-ply::parse-single-float "1d0") simple-error)
(is-error (cl-ply::parse-single-float "foo") simple-error)

;;; test PARSE-DOUBLE-FLOAT function
(is (cl-ply::parse-double-float "1.0") 1.0d0)
(is (cl-ply::parse-double-float "+0.1") +0.1d0)
(is (cl-ply::parse-double-float "-0.1") -0.1d0)
(is (cl-ply::parse-double-float "-1") -1.0d0)
(is (cl-ply::parse-double-float "-1.0d0") -1.0d0)
(is (cl-ply::parse-double-float "1d0") 1.0d0)
(is (cl-ply::parse-double-float "1D0") 1.0d0)
(is (cl-ply::parse-double-float "1d01") 1.0d1)
(is (cl-ply::parse-double-float "1d+1") 1.0d+1)
(is (cl-ply::parse-double-float "1d-1") 1.0d-1)
(is (cl-ply::parse-double-float "0.123456789012") 0.123456789012d0)
(is (cl-ply::parse-double-float "1e0") 1.0d0)
(is (cl-ply::parse-double-float "1E0") 1.0d0)
(is (cl-ply::parse-double-float "1e01") 1.0d1)
(is (cl-ply::parse-double-float "1e+1") 1.0d+1)
(is (cl-ply::parse-double-float "1e-1") 1.0d-1)
(is-error (cl-ply::parse-double-float "1e") simple-error)
(is-error (cl-ply::parse-double-float "1s0") simple-error)
(is-error (cl-ply::parse-double-float "foo") simple-error)


(finalize)
