#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply
  (:use :cl)
  (:export #:open-plyfile
           #:close-plyfile
           #:with-plyfile
           #:plyfile-element-size
           #:read-ply-element
           #:with-ply-element
           #:read-ply-elements)
  (:import-from :alexandria
                :ensure-list))
(in-package :cl-ply)


;;;
;;; Utilities
;;;

(defun cons-last (se1 se2)
  (nreverse (cons se1 (nreverse se2))))


;;;
;;; Primitive Parser - PLY file type
;;;

(defparameter +ply-file-types+
  '(("ascii"                . :ascii)
    ("binary_big_endian"    . :binary-big-endian)
    ("binary_little_endian" . :binary-little-endian)))

(defun parse-ply-file-type (string)
  (or (cdr (assoc string +ply-file-types+ :test #'string=))
      (error "PLY file type must be one of \"ascii\", \"binary_big_endian\" or \"binary_little_endian\", but ~S given." string)))


;;;
;;; Primitive Parser - PLY type
;;;

(defparameter +ply-types+ '(("char"   . :char)
                            ("uchar"  . :uchar)
                            ("short"  . :short)
                            ("ushort" . :ushort)
                            ("int"    . :int)
                            ("uint"   . :uint)
                            ("float"  . :float)
                            ("double" . :double)))

(defun parse-ply-type (string)
  (or (cdr (assoc string +ply-types+ :test #'string=))
      (error "invalid string for type: ~S" string)))


;;;
;;; Primitive Parser - integer
;;;

(defun %parse-integer (string)
  (handler-case
      (parse-integer string)
    (condition () (error "invalid string for integer: ~S" string))))


;;;
;;; Primitive Parser - non negative integer
;;;

(defparameter +non-negative-integer-regexp+ "^([1-9]\\d*|0)$")

(defun parse-non-negative-integer (string)
  (unless (cl-ppcre:scan +non-negative-integer-regexp+ string)
    (error "invalid string for unsigned integer: ~S" string))
  (parse-integer string))


;;;
;;; Primitive Parser - single/double precision floating point
;;;

(defparameter +float-regexp1+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?$")

(defparameter +float-regexp2+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?((e|E)(\\+|-)?\\d+)?$")

(defparameter +float-regexp3+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?((s|S)(\\+|-)?\\d+)?$")

(defparameter +float-regexp4+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?((d|D)(\\+|-)?\\d+)?$")

(defun parse-single-float (string)
  (cond
    (;; parse single float in default format
     (cl-ppcre:scan +float-regexp1+ string)
     (let ((s (concatenate 'string string "s0")))
       (read-from-string s)))
    (;; parse single float in exponential notation
     (cl-ppcre:scan +float-regexp2+ string)
     (let ((s (cl-ppcre:regex-replace "e|E" string "s")))
       (read-from-string s)))
    (;; parse single float in short format
     (cl-ppcre:scan +float-regexp3+ string)
     (read-from-string string))
    (;; otherwise error
     t (error "invalid string for single float: ~S" string))))

(defun parse-double-float (string)
  (cond
    (;; parse double float in default format
     (cl-ppcre:scan +float-regexp1+ string)
     (let ((s (concatenate 'string string "d0")))
       (read-from-string s)))
    (;; parse double float in exponential notation
     (cl-ppcre:scan +float-regexp2+ string)
     (let ((s (cl-ppcre:regex-replace "e|E" string "d")))
       (read-from-string s)))
    (;; parse double float in long format
     (cl-ppcre:scan +float-regexp4+ string)
     (read-from-string string))
    (;; otherwise error
     t (error "invalid string for double float: ~S" string))))


;;;
;;; PLY header parser
;;;

(defun parse-ply-header (line)
  (or (%parse-magic-header line)
      (%parse-format-header line)
      (%parse-element-header line)
      (%parse-property-header line)
      (%parse-comment-header line)
      (%parse-end-header line)
      (error "The value ~S is an invalid PLY header." line)))


;;;
;;; PLY header parser - Magic number
;;;

(defparameter +magic-header-regexp+ "^ply$")

(defun %parse-magic-header (line)
  (and (cl-ppcre:scan +magic-header-regexp+ line)
       '(:magic)))

(defun parse-magic-header (line)
  (or (%parse-magic-header line)
      (error "The value ~S is an invalid magic header." line)))


;;;
;;; PLY header parser - Format header
;;;

(defparameter +format-header-regexp+
  "^format\\s+(ascii|binary_big_endian|binary_little_endian)\\s+(1.0)$")

(defun %parse-format-header (line)
  (cl-ppcre:register-groups-bind ((#'parse-ply-file-type file-type)
                                  version)
      (+format-header-regexp+ line)
    `(:format ,file-type ,version)))

(defun parse-format-header (line)
  (or (%parse-format-header line)
      (error "The value ~S is an invalid format header." line)))


;;;
;;; PLY header parser - Element header
;;;

(defparameter +element-header-regexp+ "^element\\s+(\\w+)\\s+([1-9]\\d*)$")

(defun %parse-element-header (line)
  (cl-ppcre:register-groups-bind (name (#'parse-integer size))
      (+element-header-regexp+ line)
    `(:element ,name ,size)))

(defun parse-element-header (line)
  (or (%parse-element-header line)
      (error "The value ~S is an invalid element header." line)))


;;;
;;; PLY header parser - Property header
;;;

(defparameter +scalar-property-header-regexp+
  "^property\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defparameter +list-property-header-regexp+
  "^property\\s+list\\s+(uchar|ushort|uint)\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defun %parse-property-header (line)
  (or (cl-ppcre:register-groups-bind ((#'parse-ply-type type) name)
          (+scalar-property-header-regexp+ line)
        `(:property ,type ,name))
      (cl-ppcre:register-groups-bind ((#'parse-ply-type count-type)
                                      (#'parse-ply-type element-type)
                                      name)
          (+list-property-header-regexp+ line)
        `(:property ,count-type ,element-type ,name))))

(defun parse-property-header (line)
  (or (%parse-property-header line)
      (error "The value ~S is an invalid property header." line)))


;;;
;;; PLY header parser - Comment header
;;;

(defparameter +comment-header-regexp+ "^comment\\s+(.+)$")

(defun %parse-comment-header (line)
  (cl-ppcre:register-groups-bind (text)
      (+comment-header-regexp+ line)
    `(:comment ,text)))

(defun parse-comment-header (line)
  (or (%parse-comment-header line)
      (error "The value ~S is an invalid comment header." line)))


;;;
;;; PLY header parser - End header
;;;

(defparameter +end-header-regexp+ "^end_header$")

(defun %parse-end-header (line)
  (and (cl-ppcre:scan +end-header-regexp+ line)
       '(:end-header)))

(defun parse-end-header (line)
  (or (%parse-end-header line)
      (error "The value ~S is an invalid end header." line)))


;;;
;;; Reader
;;;

(defun read-header (stream)
  (parse-ply-header (read-line stream)))

(defun read-element (stream file-type element)
  (cond
    ((element-scalar-property-p element)
     (let ((properties (element-properties element)))
       (loop for property in properties
          collect
            (let ((element-type (scalar-property-type property)))
              (read-value stream file-type element-type)))))
    ((element-list-property-p element)
     (let ((property (car (element-properties element))))
       (let ((count-type (list-property-count-type property))
             (element-type (list-property-element-type property)))
         (let ((count (read-value stream file-type count-type)))
           (loop repeat count
              collect
                (read-value stream file-type element-type))))))
    (t (error "The value ~S is an invalid property." element))))

(defun read-value (stream file-type type)
  (ecase file-type
    (:ascii (read-ascii-value stream type))
    (:binary-big-endian (read-big-endian-value stream type))
    (:binary-little-endian (read-little-endian-value stream type))))

(defun read-ascii-value (stream type)
  (let ((word (read-word stream)))
    (ecase type
      ((:char :short :int) (%parse-integer word))
      ((:uchar :ushort :uint) (parse-non-negative-integer word))
      (:float (parse-single-float word))
      (:double (parse-double-float word)))))

(defun read-word (stream)
  (peek-char t stream t)
  (concatenate 'string
               (loop while (peek-char nil stream nil)
                  until (whitespacep (peek-char nil stream))
                  collect (read-char stream))))

(defun whitespacep (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)))

(defun read-big-endian-value (stream type)
  (declare (ignore stream type))
  (error "not implemented"))

(defun read-little-endian-value (stream type)
  (declare (ignore stream type))
  (error "not implemented"))


;;;
;;; Plyfile
;;;

(defstruct (plyfile (:constructor %make-plyfile))
  (stream nil :read-only t)
  file-type
  version
  elements
  comments
  %current-element
  count)

(defun make-plyfile (stream)
  (declare (stream stream))
  (%make-plyfile :stream stream))

(defun plyfile-current-element (plyfile)
  (car (plyfile-%current-element plyfile)))

(defun plyfile-current-element-name (plyfile)
  (element-name (plyfile-current-element plyfile)))

(defun plyfile-current-element-size (plyfile)
  (element-size (plyfile-current-element plyfile)))

(defun plyfile-ready-state (plyfile)
  ;; set count to zero
  (setf (plyfile-count plyfile) 0)
  ;; set current element to point the first element
  (setf (plyfile-%current-element plyfile) (plyfile-elements plyfile))
  ;; return itself
  plyfile)

(defun plyfile-proceed-state (plyfile)
  ;; error if at the end of elements
  (unless (plyfile-current-element plyfile)
    (error "The plyfile ~S is at the end of elements." plyfile))
  ;; increment counter
  (incf (plyfile-count plyfile))
  ;; proceed to next element if current element has been read all
  (when (= (plyfile-current-element-size plyfile)
           (plyfile-count plyfile))
    (pop (plyfile-%current-element plyfile))
    (setf (plyfile-count plyfile) 0))
  ;; return itself
  plyfile)

(defun plyfile-set-format (plyfile header)
  (cl-pattern:match header
    ((:format file-type version)
     (setf (plyfile-file-type plyfile) file-type
           (plyfile-version plyfile) version))
    (_ (error "The value ~S is an invalid FORMAT header." header))))

(defun plyfile-add-element (plyfile element)
  (declare (element element))
  (setf (plyfile-elements plyfile)
        (cons-last element (plyfile-elements plyfile))))

(defun plyfile-add-comment (plyfile comment)
  (declare (comment comment))
  (setf (plyfile-comments plyfile)
        (cons-last comment (plyfile-comments plyfile))))


;;;
;;; Plyfile - element
;;;

(defstruct (element (:constructor %make-element))
  (name :name :read-only t)
  (size :size :read-only t)
  properties)

(defun make-element (header)
  (cl-pattern:match header
    ((:element name size) (%make-element :name name :size size))
    (_ (error "The value ~S is an invalid ELEMENT header." header))))

(defun element-scalar-property-p (element)
  (let ((property (first (element-properties element))))
    (or (null property)
        (scalar-property-p property))))

(defun element-list-property-p (element)
  (let ((property (first (element-properties element))))
    (and property
         (list-property-p property))))

(defun element-add-property (element property)
  (declare (property property))
  (unless (not (and (element-properties element)
                    (element-scalar-property-p element)
                    (list-property-p property)))
    (error "The element ~S already has a scalar property." element))
  (unless (not (and (element-properties element)
                    (element-list-property-p element)
                    (scalar-property-p property)))
    (error "The element ~S already has a list property." element))
  (unless (not (and (element-properties element)
                    (element-list-property-p element)
                    (list-property-p property)))
    (error "The element ~S already has a list property." element))
  (setf (element-properties element)
        (cons-last property (element-properties element))))


;;;
;;; Plyfile - property
;;;

(defstruct (scalar-property (:constructor %make-scalar-property))
  (type :type :read-only t)
  (name :name :read-only t))

(defstruct (list-property (:constructor %make-list-property))
  (count-type :count-type :read-only t)
  (element-type :element-type :read-only t)
  (name :name :read-only t))

(deftype property ()
  '(or scalar-property list-property))

(defun make-property (header)
  (cl-pattern:match header
    ((:property type name)
     (%make-scalar-property :type type :name name))
    ((:property count-type element-type name)
     (%make-list-property :count-type count-type
                          :element-type element-type
                          :name name))
    (_ (error "The value ~S is an invalid PROPERTY header." header))))


;;;
;;; Plyfile - comment
;;;

(defstruct (comment (:constructor %make-comment))
  (text :text :read-only t))

(defun make-comment (header)
  (cl-pattern:match header
    ((:comment text) (%make-comment :text text))
    (_ (error "The value ~S is an invalid COMMENT header." header))))


;;;
;;; API
;;;

(defun open-plyfile (filespec)
  (let* ((stream (open filespec :direction :input))
         (plyfile (make-plyfile stream)))
    ;; read PLY header in the first line
    (let ((header (read-header stream)))
      (unless (eq (car header) :magic)
        (error "PLY format must start with \"ply\" magic number.")))
    ;; read FORMAT header in the second line
    (let ((header (read-header stream)))
      (unless (eq (car header) :format)
        (error "FORMAT header must follow \"ply\" magic number."))
      (plyfile-set-format plyfile header))
    ;; read the rest header lines
    (loop with  current-element = nil
          for   header = (read-header stream)
          until (eq (car header) :end-header)
       do (ecase (car header)
            (:element                   ; ELEMENT header
             (let ((element (make-element header)))
               (plyfile-add-element plyfile element)
               (setf current-element element)))
            (:property                  ; PROPERTY header
             (let ((property (make-property header)))
               (element-add-property current-element property)))
            (:comment                   ; COMMENT header
             (let ((comment (make-comment header)))
               (plyfile-add-comment plyfile comment)))))
    ;; ready plyfile for reading elements following header
    (plyfile-ready-state plyfile)
    ;; return plyfile
    plyfile))

(defun close-plyfile (plyfile)
  (close (plyfile-stream plyfile)))

(defmacro with-plyfile ((var filespec) &body body)
  `(let ((,var (open-plyfile ,filespec)))
     (unwind-protect (progn ,@body)
       (close-plyfile ,var))))

(defun plyfile-element-size (plyfile element-name)
  (element-size
    (first
      (member element-name (plyfile-elements plyfile)
              :test #'string= :key #'element-name))))

(defun read-ply-element (plyfile)
  (prog1 ;; read one element
         (let ((stream (plyfile-stream plyfile))
               (file-type (plyfile-file-type plyfile))
               (element (plyfile-current-element plyfile)))
           (read-element stream file-type element))
    ;; proceed plyfile's state
    (plyfile-proceed-state plyfile)))

(defun %read-ply-element (plyfile)
  (let ((element (plyfile-current-element plyfile)))
    (if (element-scalar-property-p element)
        (read-ply-element plyfile)
        (list (read-ply-element plyfile)))))

(defmacro with-ply-element ((vars plyfile) &body body)
  (let ((vars1 (ensure-list vars)))
    `(destructuring-bind ,vars1 (%read-ply-element ,plyfile)
       ,@body)))

(defun read-ply-elements (filespec element-name)
  (declare (string element-name))
  (with-plyfile (plyfile filespec)
    ;; skip until current element's name is ELEMENT-NAME
    (loop until (string= (plyfile-current-element-name plyfile)
                         element-name)
       do (read-ply-element plyfile))
    ;; read ELEMENT-NAME elements
    (let ((size (plyfile-current-element-size plyfile)))
      (loop repeat size
         collect (read-ply-element plyfile)))))




