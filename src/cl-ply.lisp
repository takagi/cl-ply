#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-ply)


;;;
;;; Plyfile
;;;

(defstruct (plyfile (:constructor %make-plyfile))
  (stream nil :read-only t)
  state
  file-type
  version
  elements
  comments)

(defun open-plyfile (filename)
  (let ((stream (open filename :direction :input)))
    (make-plyfile stream)))

(defun make-plyfile (stream)
  (let ((plyfile (%make-plyfile :stream stream)))
    ;; read PLY header in the first line
    (let ((header (make-ply-header (read-line stream))))
      (unless (magic-header-p header)
        (error "PLY file must start with \"ply\" magic number")))
    ;; read FORMAT header in the second line
    (let ((header (make-ply-header (read-line stream))))
      (unless (format-header-p header)
        (error "FORMAT header must follow PLY header"))
      (plyfile-set-format plyfile header))
    ;; read the rest of header lines
    (loop with  current-element = nil
          for   line   = (read-line stream)
          for   header = (make-ply-header line)
          until (end-header-p header)
       ;; ELEMENT header
       when (element-header-p header)
         do (plyfile-add-element plyfile header)
            (setf current-element (element-header-name header))
       ;; PROPERTY header
       when (property-header-p header)
         do (unless current-element
              (error "PROPERTY header must follow ELEMENT header"))
            (plyfile-add-property plyfile current-element header)
       ;; COMMENT header
       when (comment-header-p header)
         do (plyfile-add-comment plyfile header)
       ;; invalid header
       unless (or (element-header-p header)
                  (property-header-p header)
                  (comment-header-p header))
         do (error "found invalid header: ~S" line))
    ;; initialize state to be ready to read elements
    (plyfile-initialize-state plyfile)
    plyfile))

(defun close-plyfile (plyfile)
  (close (plyfile-stream plyfile)))

(defun read-ply-element (element-name plyfile)
  (let ((state (plyfile-state plyfile)))
    ;; check appropreate current state
    (unless (state-ready-p state element-name)
      (error "not in :ready state for ~S" element-name))
    ;; transfer state from :ready to :reading
    (transfer-state state)
    (prog1
        ;; read propreties in element and return them as list
        (loop
           with stream    = (plyfile-stream plyfile)
           with file-type = (plyfile-file-type plyfile)
           with current-element = (plyfile-current-element plyfile)
           repeat (element-size current-element)
           collect (read-properties stream file-type current-element))
      ;; transfer state from :reading to next :ready or :finish
      (transfer-state state))))

(defmacro with-ply-element ((props element-name plyfile) &body body)
  (alexandria:with-gensyms (state current-element stream file-type)
    `(progn
       ;; check appropreate current state
       (let ((,state (plyfile-state ,plyfile)))
         (unless (state-ready-p ,state ,element-name)
           (error "not in :ready state for ~S" ,element-name)))
       ;; check validity of property variables
       (let ((,current-element (plyfile-current-element ,plyfile)))
         (when (element-scalar-properties-p ,current-element)
           (unless (listp ',props)
             (error "variable names must be list: ~S" ',props)))
         (when (element-list-properties-p ,current-element)
           (unless (symbolp ',props)
             (error "variable name must be symbol: ~S" ',props))))
       ;; read properties and evaluate body forms for each properties
       (let ((,state (plyfile-state ,plyfile))
             (,current-element (plyfile-current-element ,plyfile)))
         ;; transfer state from :ready to :reading
         (transfer-state ,state)
         ;; main procedure in this macro
         (loop
            with ,stream    = (plyfile-stream ,plyfile)
            with ,file-type = (plyfile-file-type ,plyfile)
            repeat (element-size ,current-element)
            do ,(if (listp props)
                    `(destructuring-bind ,props
                         (read-properties ,stream ,file-type
                                          ,current-element)
                       ,@body)
                    `(let ((,props (read-properties ,stream ,file-type
                                                    ,current-element)))
                       ,@body)))
         ;; transfer state from :reading to next :ready or :finish
         (transfer-state ,state)
         ;; return no value
         (values)))))


;;;
;;; Plyfile - Element selectors
;;;

(defun plyfile-element (plyfile name)
  (let ((elements (plyfile-elements plyfile)))
    (find name elements :key #'element-name :test #'string=)))

(defun plyfile-element-names (plyfile)
  (mapcar #'element-name (plyfile-elements plyfile)))

(defun plyfile-current-element (plyfile)
  (let ((state (plyfile-state plyfile)))
    (unless (not (state-finish-p state))
      (error "already finished to read all elements"))
    (let ((current-element-name (state-current-element-name state)))
      (or (plyfile-element plyfile current-element-name)
          (error "element does not exist: ~S" current-element-name)))))


;;;
;;; Pryfile - Plyfile builder
;;;

(defun plyfile-set-format (plyfile header)
  (setf (plyfile-file-type plyfile) (format-header-file-type header)
        (plyfile-version   plyfile) (format-header-version   header)))

(defun plyfile-add-element (plyfile header)
  (symbol-macrolet ((elements (plyfile-elements plyfile)))
    (let ((name (element-header-name header)))
      (unless (null (plyfile-element plyfile name))
        (error "element already exists: ~S" name)))
    (let ((element (make-element header)))
      (setf elements (cons-last element elements)))))

(defun plyfile-add-property (plyfile current-element header)
  (let ((element (plyfile-element plyfile current-element)))
    (unless element
      (error "element does not exist: ~S" current-element))
    (element-add-property element header)))

(defun plyfile-add-comment (plyfile header)
  (symbol-macrolet ((comments (plyfile-comments plyfile)))
    (let ((comment (make-comment header)))
      (setf comments (cons-last comment comments)))))


;;;
;;; Plyfile - State management
;;;

(defun plyfile-initialize-state (plyfile)
  (let ((element-names (plyfile-element-names plyfile)))
    (setf (plyfile-state plyfile) (make-state element-names))))

(defun plyfile-transfer-state (plyfile)
  (let ((state (plyfile-state plyfile)))
    (transfer-state state)))

(defun plyfile-ready-p (plyfile element-name)
  (let ((state (plyfile-state plyfile)))
    (state-ready-p state element-name)))

(defun plyfile-reading-p (plyfile element-name)
  (let ((state (plyfile-state plyfile)))
    (state-reading-p state element-name)))

(defun plyfile-finish-p (plyfile)
  (let ((state (plyfile-state plyfile)))
    (state-finish-p state)))


;;;
;;; Plyfile - Properties reader
;;;

(defun plyfile-read-properties (plyfile)
  (let ((current-element (plyfile-current-element plyfile)))
    (let ((name (element-name current-element)))
      (unless (plyfile-reading-p plyfile name)
        (error "plyfile is not in :reading state")))
    (let ((stream (plyfile-stream plyfile))
          (file-type (plyfile-file-type plyfile)))
      (read-properties stream file-type current-element))))

(defun read-properties (stream file-type element)
  (cond
    ((element-scalar-properties-p element)
     (read-scalar-properties stream file-type element))
    ((element-list-properties-p element)
     (read-list-properties stream file-type element))))

(defun read-scalar-properties (stream file-type element)
  (let ((properties (element-properties element)))
    (loop for property in properties
       collect
         (let ((property-type (scalar-property-type property)))
           (read-property stream file-type property-type)))))

(defun read-list-properties (stream file-type element)
  (let ((property (first (element-properties element))))
    (let ((count-type   (list-property-count-type property))
          (element-type (list-property-element-type property)))
      (let ((count (read-property stream file-type count-type)))
        (loop repeat count
           collect (read-property stream file-type element-type))))))

(defun read-property (stream file-type element-type)
  (ecase file-type
    (:ascii (read-property-ascii stream element-type))
    (:binary-big-endian (read-property-binary-be stream element-type))
    (:binary-little-endian (read-property-binary-le stream element-type))))

(defun read-property-ascii (stream element-type)
  (let ((word (read-word stream)))
    (ecase element-type
      ((:char :short :int)    (%parse-integer word))
      ((:uchar :ushort :uint) (parse-non-negative-integer word))
      (:float                 (parse-single-float word))
      (:double                (parse-double-float word)))))

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

(defun read-property-binary-be (stream element-type)
  (declare (ignore stream element-type))
  (error "not implemented"))

(defun read-property-binary-le (stream element-type)
  (declare (ignore stream element-type))
  (error "not implemented"))


;;;
;;; Element
;;;

(defstruct (element (:constructor %make-element))
  (name nil :read-only t)
  (size nil :read-only t)
  properties)

(defun make-element (header)
  (%make-element :name (element-header-name header)
                 :size (element-header-size header)))

(defun element-add-property (element header)
  (let ((property (make-property header)))
    (if (scalar-property-p property)
        (element-add-scalar-property element property)
        (element-add-list-property element property))))

(defun element-add-scalar-property (element property)
  (symbol-macrolet ((properties (element-properties element)))
    (let ((name (scalar-property-name property)))
      (unless (null (element-property element name))
        (error "given property already exists: ~A" name))
      (unless (not (element-list-properties-p element))
        (error "element can have one list property at most"))
      (setf properties (cons-last property properties)))))

(defun element-add-list-property (element property)
  (symbol-macrolet ((properties (element-properties element)))
    (let ((name (list-property-name property)))
      (unless (null (element-property element name))
        (error "given property already exists: ~A" name))
      (unless (not (element-scalar-properties-p element))
        (error "element with scalar property can have scalar properties only"))
      (unless (not (element-list-properties-p element))
        (error "element can have one list property at most"))
      (setf properties (cons-last property properties)))))

(defun element-property (element name)
  (let ((properties (element-properties element)))
    (find name properties :key #'property-name :test #'string=)))

(defun element-scalar-properties-p (element)
  (let ((first-property (first (element-properties element))))
    (and first-property
         (scalar-property-p first-property))))

(defun element-list-properties-p (element)
  (let ((first-property (first (element-properties element))))
    (and first-property
         (list-property-p first-property))))


;;;
;;; Property
;;;

(defstruct scalar-property
  type
  name)

(defstruct list-property
  count-type
  element-type
  name)

(defun make-property (header)
  (cond
    ((scalar-property-header-p header)
     (make-scalar-property :type (scalar-property-header-type header)
                           :name (scalar-property-header-name header)))
    ((list-property-header-p header)
     (make-list-property :count-type (list-property-header-count-type header)
                         :element-type (list-property-header-element-type header)
                         :name (list-property-header-name header)))))

(defun property-name (property)
  (cond
    ((scalar-property-p property) (scalar-property-name property))
    ((list-property-p property) (list-property-name property))))


;;;
;;; Comment
;;;

(defstruct (comment (:constructor %make-comment))
  text)

(defun make-comment (header)
  (%make-comment :text (comment-header-text header)))


;;;
;;; State
;;;

(defstruct (state (:constructor %make-state)
                  (:conc-name   %state-))
  name
  element-names)

(defun make-state (element-names)
  (loop for element-name in element-names
     do (check-type element-name string))
  (if element-names
      (%make-state :name :ready
                   :element-names element-names)
      (%make-state :name :finish)))

(defun transfer-state (state)
  (symbol-macrolet ((name          (%state-name state))
                    (element-names (%state-element-names state)))
    (ecase name
      (:ready   (setf name :reading))
      (:reading (if (cdr element-names)
                    (setf name :ready
                          element-names (cdr element-names))
                    (setf name :finish
                          element-names nil)))
      (:finish  nil))                   ; noop
    state))

(defun state-name (state)
  (%state-name state))

(defun state-current-element-name (state)
  (first (%state-element-names state)))

(defun state-ready-p (state element-name)
  (check-type element-name string)
  (let ((current-state-name   (state-name state))
        (current-element-name (state-current-element-name state)))
    (and (eq current-state-name :ready)
         (string= current-element-name element-name))))

(defun state-reading-p (state element-name)
  (check-type element-name string)
  (let ((current-state-name   (state-name state))
        (current-element-name (state-current-element-name state)))
    (and (eq current-state-name :reading)
         (string= current-element-name element-name))))

(defun state-finish-p (state)
  (let ((current-state-name   (state-name state))
        (current-element-name (state-current-element-name state)))
    (and (eq current-state-name :finish)
         (null current-element-name))))


;;;
;;; PLY headers
;;;

(defun make-ply-header (line)
  (let ((keyword (first (cl-ppcre:split "\\s+" line))))
    (cond
      ((string= keyword "ply")        (make-magic-header line))
      ((string= keyword "format")     (make-format-header line))
      ((string= keyword "element")    (make-element-header line))
      ((string= keyword "property")   (make-property-header line))
      ((string= keyword "comment")    (make-comment-header line))
      ((string= keyword "end_header") (make-end-header line))
      (t (error "invalid header: ~S" line)))))


;;;
;;; Magic header
;;;

(defstruct (magic-header (:constructor %make-magic-header)))

(defparameter +magic-header-regexp+ "^ply$")

(defun make-magic-header (line)
  (unless (cl-ppcre:scan +magic-header-regexp+ line)
    (error "invalid magic header: ~S" line))
  (%make-magic-header))


;;;
;;; Format header
;;;

(defstruct (format-header (:constructor %make-format-header))
  (file-type nil :read-only t)
  (version   nil :read-only t))

(defparameter +format-header-regexp+
  "^format\\s+(ascii|binary_big_endian|binary_little_endian)\\s+(1.0)$")

(defun make-format-header (line)
  (unless (cl-ppcre:scan +format-header-regexp+ line)
    (error "invalid format header: ~S" line))
  (cl-ppcre:register-groups-bind ((#'parse-ply-file-type file-type) version)
      (+format-header-regexp+ line)
    (%make-format-header :file-type file-type
                         :version   version)))

(defparameter +ply-file-types+
  '(("ascii"                . :ascii)
    ("binary_big_endian"    . :binary-big-endian)
    ("binary_little_endian" . :binary-little-endian)))

(defun parse-ply-file-type (string)
  (or (cdr (assoc string +ply-file-types+ :test #'string=))
      (error "PLY file type must be one of \"ascii\", \"binary_big_endian\" or \"binary_little_endian")))


;;;
;;; Element header
;;;

(defstruct (element-header (:constructor %make-element-header))
  (name nil :read-only t)
  (size nil :read-only t))

(defparameter +element-header-regexp+ "^element\\s+(\\w+)\\s+([1-9]\\d*)$")

(defun make-element-header (line)
  (unless (cl-ppcre:scan +element-header-regexp+ line)
    (error "invalid element header: ~S" line))
  (cl-ppcre:register-groups-bind (name (#'parse-integer size))
      (+element-header-regexp+ line)
    (%make-element-header :name name :size size)))


;;;
;;; Property header
;;;

(defstruct (scalar-property-header (:constructor %make-scalar-property-header))
  (type nil :read-only t)
  (name nil :read-only t))

(defstruct (list-property-header (:constructor %make-list-property-header))
  (count-type   nil :read-only t)
  (element-type nil :read-only t)
  (name         nil :read-only t))

(defparameter +scalar-property-header-regexp+
  "^property\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defparameter +list-property-header-regexp+
  "^property\\s+list\\s+(uchar|ushort|uint)\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defun make-property-header (line)
  (cond
    (;; make scalar property header
     (cl-ppcre:scan +scalar-property-header-regexp+ line)
     (cl-ppcre:register-groups-bind ((#'parse-ply-type type) name)
         (+scalar-property-header-regexp+ line)
       (%make-scalar-property-header :type type :name name)))
    (;; make list property header
     (cl-ppcre:scan +list-property-header-regexp+ line)
     (cl-ppcre:register-groups-bind ((#'parse-ply-type count-type)
                                     (#'parse-ply-type element-type)
                                     name)
         (+list-property-header-regexp+ line)
       (%make-list-property-header :count-type   count-type
                                   :element-type element-type
                                   :name         name)))
    (;; otherwise error
     t (error "invalid property header: ~S" line))))

(defun property-header-p (header)
  (or (scalar-property-header-p header)
      (list-property-header-p header)))


;;;
;;; Comment header
;;;

(defstruct (comment-header (:constructor %make-comment-header))
  (text nil :read-only t))

(defparameter +comment-header-regexp+ "^comment\\s+(.+)$")

(defun make-comment-header (line)
  (unless (cl-ppcre:scan +comment-header-regexp+ line)
    (error "invalid comment header: ~S" line))
  (cl-ppcre:register-groups-bind (text)
      (+comment-header-regexp+ line)
    (%make-comment-header :text text)))


;;;
;;; End header
;;;

(defstruct (end-header (:constructor %make-end-header)))

(defparameter +end-header-regexp+ "^end_header$")

(defun make-end-header (line)
  (unless (cl-ppcre:scan +end-header-regexp+ line)
    (error "invalid end header: ~S" line))
  (%make-end-header))


;;;
;;; Parsing for types in PLY format
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
;;; Parsing integer
;;;

(defun %parse-integer (string)
  (handler-case
      (parse-integer string)
    (condition () (error "invalid string for integer: ~S" string))))


;;;
;;; Parsing non negative integer
;;;

(defparameter +non-negative-integer-regexp+ "^([1-9]\\d*|0)$")

(defun parse-non-negative-integer (string)
  (unless (cl-ppcre:scan +non-negative-integer-regexp+ string)
    (error "invalid string for unsigned integer: ~S" string))
  (parse-integer string))


;;;
;;; Parsing single/double precision floating point numbers
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
;;; Operations for list
;;;

(defun cons-last (se1 se2)
  (nreverse (cons se1 (nreverse se2))))

