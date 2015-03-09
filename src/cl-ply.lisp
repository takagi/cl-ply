#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ply
  (:use :cl)
  (:export #:ply-open-for-reading
           #:ply-close
           #:with-ply-for-reading
           #:ply-element-names
           #:ply-element-size
           #:ply-read-element
           #:ply-comments
           #:ply-obj-info))
(in-package :cl-ply)


;;;
;;; Plyfile
;;;

(defstruct (plyfile (:constructor %make-plyfile))
  (stream nil :read-only t)
  file-type
  version
  elements
  comments
  obj-info)

(defun make-plyfile (stream)
  (declare (type stream stream))
  (%make-plyfile :stream stream))

(defun plyfile-element-by-name (plyfile element-name)
  (car (member element-name (plyfile-elements plyfile)
               :test #'string= :key #'element-name)))

(defun plyfile-set-format (plyfile header)
  (cl-pattern:match header
    ((:format file-type version)
     (setf (plyfile-file-type plyfile) file-type
           (plyfile-version plyfile) version))
    (_ (error "The value ~S is an invalid FORMAT header." header)))
  plyfile)

(defun plyfile-add-element (plyfile element)
  (declare (type element element))
  (push element (plyfile-elements plyfile))
  plyfile)

(defun plyfile-add-property (plyfile property)
  (let ((current-element (car (plyfile-elements plyfile))))
    (element-add-property current-element property))
  plyfile)

(defun plyfile-add-comment (plyfile comment)
  (declare (type comment comment))
  (push comment (plyfile-comments plyfile))
  plyfile)

(defun plyfile-add-obj-info (plyfile obj-info)
  (declare (type obj-info obj-info))
  (push obj-info (plyfile-obj-info plyfile))
  plyfile)


;;;
;;; Element
;;;

(defstruct (element (:constructor %make-element))
  (name :name :read-only t)
  (size :size :read-only t)
  properties)

(defun make-element (header)
  (cl-pattern:match header
    ((:element name size) (%make-element :name name :size size))
    (_ (error "The value ~S is an invalid ELEMENT header." header))))

(defun element-add-property (element property)
  (declare (type property property))
  (push property (element-properties element))
  element)


;;;
;;; Property
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
;;; Comment
;;;

(defstruct (comment (:constructor %make-comment))
  (text :text :read-only t))

(defun make-comment (header)
  (cl-pattern:match header
    ((:comment text) (%make-comment :text text))
    (_ (error "The value ~S is an invalid COMMENT header." header))))


;;;
;;; Obj-info
;;;

(defstruct (obj-info (:constructor %make-obj-info))
  (text :text :read-only t))

(defun make-obj-info (header)
  (cl-pattern:match header
    ((:obj-info text) (%make-obj-info :text text))
    (_ (error "The value ~S is an invalid OBJ-INFO header." header))))


;;;
;;; Primitive Parser
;;;

(defparameter +ply-file-types+
  '(("ascii"                . :ascii)
    ("binary_big_endian"    . :binary-big-endian)
    ("binary_little_endian" . :binary-little-endian)))

(defun parse-ply-file-type (string)
  (or (cdr (assoc string +ply-file-types+ :test #'string=))
      (error "PLY file type must be one of \"ascii\", \"binary_big_endian\" or \"binary_little_endian\", but ~S given." string)))

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
      (error "The value ~S is an invalid PLY type." string)))

(defun parse-integer% (string)
  (handler-case (parse-integer string)
    (condition () (error "Invalid string for integer: ~A" string))))

(defun parse-unsigned-integer (string)
  (let ((value (handler-case (parse-integer% string)
                 (condition ()
                   (error "Invalid string for unsigned integer: ~A" string)))))
    (if (<= 0 value)
        value
        (error "Invalid string for unsigned integer: ~A" string))))

(defun parse-single-float (string)
  (handler-case (float (read-from-string string) 1.0)
    (condition () (error "Invalid string for single float: ~A" string))))

(defun parse-double-float (string)
  (handler-case (float (read-from-string string) 1.0d0)
    (condition () (error "Invalid string for single float: ~A" string))))

(defun parse-value (string ply-type)
  (ecase ply-type
    ((:char :short :int) (parse-integer% string))
    ((:uchar :ushort :uint) (parse-unsigned-integer string))
    (:float (parse-single-float string))
    (:double (parse-double-float string))))


;;;
;;; PLY header parser
;;;

(defun parse-header (line)
  (or (parse-ply-header line)
      (parse-format-header line)
      (parse-element-header line)
      (parse-property-header line)
      (parse-comment-header line)
      (parse-obj-info-header line)
      (parse-end-header line)
      (error "The value ~S is an invalid PLY format header." line)))

(defun parse-ply-header (line)
  (and (cl-ppcre:scan "^ply$" line)
       '(:ply)))

(defun parse-format-header (line)
  (cl-ppcre:register-groups-bind ((#'parse-ply-file-type file-type)
                                  (#'parse-single-float version))
      ("^format\\s+(\\S+)\\s+(\\S+)$" line)
    `(:format ,file-type ,version)))

(defun parse-element-header (line)
  (cl-ppcre:register-groups-bind (name (#'parse-integer% size))
      ("^element\\s+(\\S+)\\s+(\\S+)$" line)
    `(:element ,name ,size)))

(defun parse-property-header (line)
  (or (cl-ppcre:register-groups-bind ((#'parse-ply-type type) name)
          ("^property\\s+(\\S+)\\s+(\\S+)$" line)
        `(:property ,type ,name))
      (cl-ppcre:register-groups-bind ((#'parse-ply-type count-type)
                                      (#'parse-ply-type element-type)
                                      name)
          ("^property\\s+list\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)$" line)
        `(:property ,count-type ,element-type ,name))))

(defun parse-comment-header (line)
  (cl-ppcre:register-groups-bind (text)
      ("^comment\\s+(\\S.*)$" line)
    `(:comment ,text)))

(defun parse-obj-info-header (line)
  (cl-ppcre:register-groups-bind (text)
      ("^obj_info\\s+(\\S.*)$" line)
    `(:obj-info ,text)))

(defun parse-end-header (line)
  (and (cl-ppcre:scan "^end_header$" line)
       '(:end-header)))

(defun header-kind (object)
  (and (listp object)
       (car object)))

(defun ply-header-p (object)
  (eq (header-kind object) :ply))

(defun format-header-p (object)
  (eq (header-kind object) :format))

(defun element-header-p (object)
  (eq (header-kind object) :element))

(defun property-header-p (object)
  (eq (header-kind object) :property))

(defun comment-header-p (object)
  (eq (header-kind object) :comment))

(defun obj-info-header-p (object)
  (eq (header-kind object) :obj-info))

(defun end-header-p (object)
  (eq (header-kind object) :end-header))


;;;
;;; Reader
;;;

(defun read-header (stream)
  (parse-header (read-line stream)))

(defun read-element (stream file-type element)
  (ecase file-type
    (:ascii (read-element-ascii stream element))
    (:binary-big-endian (read-element-big-endian stream element))
    (:binary-little-endian (read-element-little-endian stream element))))

(defun read-element-ascii (stream element)
  (let ((words (cl-ppcre:split "\\s" (read-line stream))))
    (let ((properties (element-properties element)))
      (loop for property in properties
         collect
           (cond
             ((scalar-property-p property)
              (let ((element-type (scalar-property-type property)))
                (parse-value (pop words) element-type)))
             ((list-property-p property)
              (let ((count-type (list-property-count-type property))
                    (element-type (list-property-element-type property)))
                (let ((count (parse-value (pop words) count-type)))
                  (loop repeat count
                     collect
                       (parse-value (pop words) element-type)))))
             (t (error "The value ~S is an invalid property." property)))))))

(defun read-element-big-endian (stream type)
  (declare (ignore stream type))
  (error "Not implemented."))

(defun read-element-little-endian (stream type)
  (declare (ignore stream type))
  (error "Not implemented."))


;;;
;;; API
;;;

(defun ply-open-for-reading (filespec)
  (let* ((stream (open filespec :direction :input))
         (plyfile (make-plyfile stream)))
    ;; read PLY header
    (let ((header (read-header stream)))
      (unless (ply-header-p header)
        (error "PLY format should start with PLY header.")))
    ;; read FORMAT header
    (let ((header (read-header stream)))
      (unless (format-header-p header)
        (error "FORMAT header should follow PLY header."))
      (plyfile-set-format plyfile header))
    ;; read the rest of headers.
    (loop for header = (read-header stream)
          until (end-header-p header)
       do (ecase (header-kind header)
            (:element                   ; ELEMENT header
             (let ((element (make-element header)))
               (plyfile-add-element plyfile element)))
            (:property                  ; PROPERTY header
             (let ((property (make-property header)))
               (plyfile-add-property plyfile property)))
            (:comment                   ; COMMENT header
             (let ((comment (make-comment header)))
               (plyfile-add-comment plyfile comment)))
            (:obj-info                  ; OBJ-INFO header
             (let ((obj-info (make-obj-info header)))
               (plyfile-add-obj-info plyfile obj-info)))))
    ;; return PLYFILE object.
    plyfile))

(defun ply-close (plyfile)
  (close (plyfile-stream plyfile)))

(defmacro with-ply-for-reading ((var filespec) &body body)
  `(let ((,var (ply-open-for-reading ,filespec)))
     (unwind-protect (progn ,@body)
       (ply-close ,var))))

(defun ply-element-names (plyfile)
  (nreverse (mapcar #'element-name (plyfile-elements plyfile))))

(defun ply-element-size (plyfile element-name)
  (element-size (plyfile-element-by-name plyfile element-name)))

(defun ply-read-element (plyfile element-name)
  (let ((stream (plyfile-stream plyfile))
        (file-type (plyfile-file-type plyfile))
        (element (plyfile-element-by-name plyfile element-name)))
    (read-element stream file-type element)))

(defun ply-comments (plyfile)
  (nreverse (mapcar #'comment-text (plyfile-comments plyfile))))

(defun ply-obj-info (plyfile)
  (nreverse (mapcar #'obj-info-text (plyfile-obj-info plyfile))))
