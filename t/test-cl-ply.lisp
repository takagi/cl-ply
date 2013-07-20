#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-ply-test)

(plan nil)


;;;
;;; test reading Ply file
;;;

;;; test APPEND-EXTENSION-IF-NECESSARY function
(is (cl-ply::append-extension-if-necessary "foobar.ply") "foobar.ply")
(is (cl-ply::append-extension-if-necessary "foobar") "foobar.ply")
(is (cl-ply::append-extension-if-necessary "foo") "foo.ply")

;;; test PLY-READ function
(let ((str "ply
format ascii 1.0
comment foo
element vertex 128
property float x
property float y
property float z
element face 128
property list uchar int vertex_index
end_header"))
  (with-input-from-string (stream str)
    (let ((plyfile (cl-ply:ply-read stream)))
      ;; test PLY header
      (is (cl-ply::plyfile-file-type plyfile) :ascii)
      ;; test FORMAT header
      (is (cl-ply::plyfile-version plyfile) "1.0")
      ;; test COMMENT header
      (let ((comment (first (cl-ply::plyfile-comments plyfile))))
        (is (cl-ply::comment-text comment) "foo"))
      ;; test "vertex" ELEMENT header
      (let ((element (first (cl-ply::plyfile-elements plyfile))))
        (is (cl-ply::element-name element) "vertex")
        (is (cl-ply::element-size element) 128))
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
        (is (cl-ply::element-size element) 128))
      ;; test PROPERTY header of "face" element
      (let ((element (second (cl-ply::plyfile-elements plyfile))))
        (let ((property (first (cl-ply::element-properties element))))
          (ok (cl-ply::list-property-p property))
          (is (cl-ply::list-property-count-type property) :uchar)
          (is (cl-ply::list-property-element-type property) :int)
          (is (cl-ply::list-property-name property) "vertex_index"))))))

;;; PROPERTY header must follow ELEMENT header
(let ((str "ply
format ascii 1.0
property float x"))
  (with-input-from-string (stream str)
    (is-error (cl-ply:ply-read stream) simple-error)))

(defparameter +test-ply-data+
  "ply
format ascii 1.0
element vertex 2
property float x
property float y
property float z
element face 2
property list uchar int vertex_index
end_header
0.0 0.0 0.0
0.0 0.0 1.0
4 0 1 2 3
4 4 5 6 7")

;;; test WITH-PLY-ELEMENT macro
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (let (result)
      (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
        (push (list x y z) result))
      (is result '((0.0 0.0 1.0) (0.0 0.0 0.0))))
    (let (result)
      (cl-ply:with-ply-element (vertex_indices "face" plyfile)
        (push vertex_indices result))
      (is result '((4 5 6 7) (0 1 2 3))))))

;;; error if trying to read another element but the one ready to be read
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                (progn foo))            ; do nothing
              simple-error)))

;;; error if trying to read element while reading another
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (progn x y z)                     ; do nothing
      (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                  (progn foo))          ; do nothing
                simple-error))))

;;; error if trying to read element after finished reading all
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (progn x y z))                    ; do nothing
    (cl-ply:with-ply-element (vertex_indices "face" plyfile)
      (progn vertex_indices))           ; do nothing
    (is-error (cl-ply:with-ply-element (foo "foo" plyfile)
                (progn foo))            ; do nothing
              simple-error)))

;;; test READ-PLY-ELEMENT function
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (is (cl-ply:read-ply-element "vertex" plyfile)
        '((0.0 0.0 0.0) (0.0 0.0 1.0)))
    (is (cl-ply:read-ply-element "face" plyfile)
        '((0 1 2 3) (4 5 6 7)))))

;;; error if trying to read element other than ready to be read
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error)))

;;; error if trying to read element while reading one
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
      (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error))))

;;; error if trying to read element after finished reading all
(with-input-from-string (stream +test-ply-data+)
  (let ((plyfile (cl-ply:ply-read stream)))
    (cl-ply:read-ply-element "vertex" plyfile)
    (cl-ply:read-ply-element "face" plyfile)
    (is-error (cl-ply:read-ply-element "foo" plyfile) simple-error)))


;;;
;;; test Plyfile
;;;

;;; test PLYFILE-SET-FORMAT function
(let ((plyfile (cl-ply::make-plyfile)))
  (let ((header (cl-ply::make-format-header "format ascii 1.0")))
    (cl-ply::plyfile-set-format plyfile header))
  (is (cl-ply::plyfile-file-type plyfile) :ascii)
  (is (cl-ply::plyfile-version   plyfile) "1.0"))

;;; test PLYFILE-ADD-ELEMENT function
(let ((plyfile (cl-ply::make-plyfile)))
  ;; plyfile has no elements initially
  (ok (null (cl-ply::plyfile-elements plyfile)))
  ;; add element
  (let ((header (cl-ply::make-element-header "element vertex 128")))
    (cl-ply::plyfile-add-element plyfile header))
  ;; test first element
  (let ((element (first (cl-ply::plyfile-elements plyfile))))
    (is (cl-ply::element-name element) "vertex")
    (is (cl-ply::element-size element) 128))
   ;; add another element
   (let ((header (cl-ply::make-element-header "element face 128")))
     (cl-ply::plyfile-add-element plyfile header))
  ;; test order of elements
  (is (mapcar #'cl-ply::element-name (cl-ply::plyfile-elements plyfile))
      '("vertex" "face"))
  ;; test second element
  (let ((element (second (cl-ply::plyfile-elements plyfile))))
    (is (cl-ply::element-name element) "face")
    (is (cl-ply::element-size element) 128))
  ;; adding duplicated element causes error
  (let ((header (cl-ply::make-element-header "element face 128")))
    (is-error (cl-ply::plyfile-add-element plyfile header) simple-error)))

;;; test PLYFILE-ADD-PROPERTY function
(let ((plyfile (cl-ply::make-plyfile)))
  ;; error if current element is nil
  (let ((header (cl-ply::make-property-header "property float x")))
    (is-error (cl-ply::plyfile-add-property plyfile nil header) simple-error))
  ;; add element
  (let ((header (cl-ply::make-element-header "element vertex 128")))
    (cl-ply::plyfile-add-element plyfile header))
   ;; element has no properties initially
   (let ((element (car (cl-ply::plyfile-elements plyfile))))
     (ok (null (cl-ply::element-properties element))))
  ;; add property to element
  (let ((header (cl-ply::make-property-header "property float x")))
    (cl-ply::plyfile-add-property plyfile "vertex" header))
  ;; test first property
  (let* ((element (car (cl-ply::plyfile-elements plyfile)))
         (property (car (cl-ply::element-properties element))))
    (is (cl-ply::scalar-property-type property) :float)
    (is (cl-ply::scalar-property-name property) "x"))
  ;; add another property to element
  (let ((header (cl-ply::make-property-header "property float y")))
    (cl-ply::plyfile-add-property plyfile "vertex" header))
  ;; test order of properties
  (let ((element (car (cl-ply::plyfile-elements plyfile))))
    (is (mapcar #'cl-ply::scalar-property-name
                (cl-ply::element-properties element))
        '("x" "y")))
  ;; test second property
  (let* ((element (car (cl-ply::plyfile-elements plyfile)))
         (property (cadr (cl-ply::element-properties element))))
    (is (cl-ply::scalar-property-type property) :float)
    (is (cl-ply::scalar-property-name property) "y"))
  ;; adding duplicated property causes error
  (let ((header (cl-ply::make-property-header "property float y")))
    (is-error (cl-ply::plyfile-add-property plyfile "vertex" header)
              simple-error))
  ;; error if current element does not exist
  (let ((header (cl-ply::make-property-header "property float x")))
    (is-error (cl-ply::plyfile-add-property plyfile "foo" header)
              simple-error)))

;;; test PLYFILE-ADD-COMMENT function
(let ((plyfile (cl-ply::make-plyfile)))
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

;;; test PLYFILE-READ-PROPERTIES function
(let ((str "ply
format ascii 1.0
comment foo
element vertex 1
property float x
property float y
property float z
element face 1
property list uchar int vertex_index
end_header
0.0 0.0 0.0
4 0 1 2 3"))
  (with-input-from-string (stream str)
    (let ((plyfile (cl-ply:ply-read stream)))
      (cl-ply::plyfile-transfer-state plyfile) ; :ready   -> :reading
      (is (cl-ply::plyfile-read-properties plyfile) '(0.0 0.0 0.0))
      (cl-ply::plyfile-transfer-state plyfile) ; :reading -> :ready
      (cl-ply::plyfile-transfer-state plyfile) ; :ready   -> :reading
      (is (cl-ply::plyfile-read-properties plyfile) '(0 1 2 3))
      (cl-ply::plyfile-transfer-state plyfile) ; :reading -> :finish
      (ok (cl-ply::plyfile-finish-p plyfile))))
  (with-input-from-string (stream str)
    (let ((plyfile (cl-ply:ply-read stream)))
      ;; need to be in :reading state
      (is-error (cl-ply::plyfile-read-properties plyfile) simple-error))))


;;; test READ-PROPERTY function

(let ((str "0 1 2 3 4 5"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :uchar) 0)
    (is (cl-ply::read-property stream :ascii :char) 1)
    (is (cl-ply::read-property stream :ascii :ushort) 2)
    (is (cl-ply::read-property stream :ascii :short) 3)
    (is (cl-ply::read-property stream :ascii :uint) 4)
    (is (cl-ply::read-property stream :ascii :int) 5)))

(let ((str "-1"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uchar) simple-error))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :ushort) simple-error))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uint) simple-error)))

(let ((str "0.12345678901 0.123456789012"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :float) 0.123456789s0)
    (is (cl-ply::read-property stream :ascii :double) 0.123456789012d0)))

(let ((str "0 1"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :float) 0s0)
    (is (cl-ply::read-property stream :ascii :double) 1d0)))

(let ((str "foo"))
  (with-input-from-string (stream str)
    (is-error (cl-ply::read-property stream :ascii :uchar) error)))

(let ((str "1e0 2e-1"))
  (with-input-from-string (stream str)
    (is (cl-ply::read-property stream :ascii :float) 1s0)
    (is (cl-ply::read-property stream :ascii :double) 2d-1)))

(let ((str "1234 2.0
3.0 \"4 5\""))
  (with-input-from-string (stream str)
    (is (cl-ply::read-word stream) "1234")
    (is (cl-ply::read-word stream) "2.0")
    (is (cl-ply::read-word stream) "3.0")
    (is (cl-ply::read-word stream) "\"4")
    (is (cl-ply::read-word stream) "5\"")))


;;;
;;; test Element
;;;

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

(ok (cl-ply::make-magic-header "ply"))
(is-error (cl-ply::make-magic-header "ply ") simple-error)
(is-error (cl-ply::make-magic-header "plya") simple-error)
(is-error (cl-ply::make-magic-header " ply") simple-error)


;;;
;;; test Format header
;;;

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

(let ((header (cl-ply::make-comment-header "comment foo bar baz")))
  (is (cl-ply::comment-header-text header) "foo bar baz"))
(let ((header (cl-ply::make-comment-header "comment  foo bar baz")))
  (is (cl-ply::comment-header-text header) "foo bar baz"))


;;;
;;; test End header
;;;

(ok (cl-ply::make-end-header "end_header"))
(is-error (cl-ply::make-end-header " end_header") simple-error)
(is-error (cl-ply::make-end-header "end_header ") simple-error)


;;;
;;; test types
;;;

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
