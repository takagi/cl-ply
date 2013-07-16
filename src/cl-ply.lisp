#|
  This file is a part of cl-ply project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-ply)


;;;
;;; Reading Ply file
;;;

(defun ply-open-for-reading (filename)
  "Open a polygon file for reading."
  (let ((name (append-extension-if-necessary filename)))
    (let ((fp (open name :direction :input)))
      (ply-read fp))))

(defun ply-close (plyfile)
  (close (plyfile-stream plyfile)))

(defun append-extension-if-necessary (filename)
  "Tack on the extension .ply, if necessary."
  (labels ((ends-with (str suffix)
             (let* ((n (length suffix))
                    (suffix0 (reverse (subseq (reverse str) 0 n))))
               (string/= suffix0 suffix)))
           (append-extension (filename)
             (concatenate 'string filename ".ply")))
    (cond
      ((< (length filename) 4)     (append-extension filename))
      ((ends-with filename ".ply") (append-extension filename))
      (t                           filename))))

(defun ply-read (stream)
  "Given a file stream, get ready to read PLY data from the file."
  (let ((plyfile (make-plyfile :stream stream)))
    ;; read PLY header which must be the first line
    (let ((header (make-ply-header (read-line stream))))
      (unless (magic-header-p header)
        (error "PLY file must start with \"ply\" magic number")))
    ;; read FORMAT header which must follow the PLY header
    (let ((header (make-ply-header (read-line stream))))
      (unless (format-header-p header)
        (error "FORMAT header must follow PLY header"))
      (plyfile-set-format plyfile header))
    ;; read the rest of header lines
    (loop for   header = (make-ply-header (read-line stream))
          with  current-element = nil
          until (end-header-p header)
       ; ELEMENT header
       when (element-header-p header)
         do (plyfile-add-element plyfile header)
            (setf current-element (element-header-name header))
       ; PROPERTY header
       when (property-header-p header)
         do (unless current-element
              (error "PROPERTY header must follow ELEMENT header"))
            (plyfile-add-property plyfile current-element header)
       ; COMMENT header
       when (comment-header-p header)
         do (plyfile-add-comment plyfile header)
       ; invalid headers
       unless (or (element-header-p header)
                  (property-header-p header)
                  (comment-header-p header))
         do (error "found invalid header: ~A" header))
    ;; initialize state to be ready to read elements
    (plyfile-initialize-state plyfile)
    plyfile))

(defmacro with-ply-element ((props element-name plyfile) &body body)
  `(progn
     (unless (plyfile-current-state-p ,plyfile :ready ,element-name)
       (error "invalid current state: ~S" (plyfile-current-state ,plyfile)))
     (plyfile-transfer-state ,plyfile)  ; transfer :ready -> :reading
     (loop repeat (plyfile-current-element-size ,plyfile)
        do ,(if (consp props)
                `(destructuring-bind ,props
                     (plyfile-read-properties ,plyfile)
                   ,@body)
                `(let ((,props (plyfile-read-properties ,plyfile)))
                   ,@body)))
     (plyfile-transfer-state ,plyfile))) ;transfer :reading -> :ready|:finish

(defun read-ply-element (element-name plyfile)
  (unless (plyfile-current-state-p plyfile :ready element-name)
    (error "invalid current state: ~S" (plyfile-current-state plyfile)))
  (plyfile-transfer-state plyfile)     ; transfer :ready -> :reading
  (prog1
      (loop repeat (plyfile-current-element-size plyfile)
         collect (plyfile-read-properties plyfile))
    (plyfile-transfer-state plyfile))) ; transfer :reading -> :ready|:finish


;;;
;;; Plyfile 
;;;

(defstruct plyfile
  (stream :read-only)
  state
  file-type
  version
  raw-elements                          ; alist { name -> element }
  comments)


;;; selectors on elements

(defun plyfile-elements (plyfile)
  (mapcar #'cdr (plyfile-raw-elements plyfile)))

(defun plyfile-element (plyfile name)
  (let ((raw-elements (plyfile-raw-elements plyfile)))
    (let ((element (cdr (assoc name raw-elements :test #'string=))))
      (unless element
        (error "element does not exist: ~A" name))
      element)))

(defun plyfile-element-names (plyfile)
  (mapcar #'element-name (plyfile-elements plyfile)))


;;; selectors on current element

(defun plyfile-current-element (plyfile)
  (let* ((state (plyfile-state plyfile))
         (current-element-name (state-current-element-name state)))
    (plyfile-element plyfile current-element-name)))


(defun plyfile-current-element-name (plyfile)
  (let ((current-element (plyfile-current-element plyfile)))
    (element-name current-element)))

(defun plyfile-current-element-size (plyfile)
  (let ((current-element (plyfile-current-element plyfile)))
    (element-size current-element)))


;;; procedures to add headers into plyfile

(defun plyfile-set-format (plyfile header)
  (setf (plyfile-file-type plyfile) (format-header-file-type header)
        (plyfile-version   plyfile) (format-header-version   header)))

(defun plyfile-add-element (plyfile header)
  (symbol-macrolet ((raw-elements (plyfile-raw-elements plyfile)))
    (let ((name     (element-header-name header))
          (element  (make-element header)))
      (unless (null (assoc name raw-elements :test #'string=))
        (error "given element already exists: ~A" name))
      (setf raw-elements (acons-last name element raw-elements)))))

(defun plyfile-add-property (plyfile element header)
  (let ((element (plyfile-element plyfile element)))
    (element-add-property element header)))

(defun plyfile-add-comment (plyfile header)
  (symbol-macrolet ((comments (plyfile-comments plyfile)))
    (let ((comment (make-comment header)))
      (setf comments (cons-last comment comments)))))


;;; functions on plyfile state

(defun plyfile-initialize-state (plyfile)
  (let ((element-names (plyfile-element-names plyfile)))
    (setf (plyfile-state plyfile) (make-state element-names))))

(defun plyfile-current-state-p (plyfile state-name &optional element-name)
  (let ((state (plyfile-state plyfile)))
    (state-current-p state state-name element-name)))

(defun plyfile-current-state (plyfile)
  (let ((state (plyfile-state plyfile)))
    (state-current state)))

(defun plyfile-transfer-state (plyfile)
  (let ((state (plyfile-state plyfile)))
    (transfer-state state)))


;;; procedures to read properties for current element

(defun plyfile-read-properties (plyfile)
  (let ((current-element-name (plyfile-current-element-name plyfile)))
    (unless (plyfile-current-state-p plyfile :reading current-element-name)
      (error "plyfile is not in :reading state")))
  (let ((element (plyfile-current-element plyfile)))
    (cond
      ((element-scalar-properties-p element) (read-scalar-properties plyfile))
      ((element-list-properties-p element) (read-list-properties plyfile)))))

(defun read-scalar-properties (plyfile)
  (let ((element   (plyfile-current-element plyfile))
        (stream    (plyfile-stream plyfile))
        (file-type (plyfile-file-type plyfile)))
    (loop for property in (element-properties element)
       collect
         (let ((property-type (scalar-property-type property)))
           (read-property stream file-type property-type)))))

(defun read-list-properties (plyfile)
  (let* ((element   (plyfile-current-element plyfile))
         (stream    (plyfile-stream plyfile))
         (file-type (plyfile-file-type plyfile))
         (property  (first (element-properties element))))
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
      ((:char :short :int)    (parse-integer word))
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
  (error "not implemented"))

(defun read-property-binary-le (stream element-type)
  (error "not implemented"))


;;;
;;; Element
;;;

(defstruct (element (:constructor %make-element))
  (name :read-only)
  (size :read-only)
  raw-properties)                       ; alist { name -> property }

(defun make-element (header)
  (%make-element :name (element-header-name header)
                 :size (element-header-size header)))

(defun element-add-property (element header)
  (symbol-macrolet ((raw-properties (element-raw-properties element)))
    (let ((name     (property-header-name header))
          (property (make-property header)))
      (unless (null (assoc name raw-properties :test #'string=))
        (error "given property already exists: ~A" name))
      (when (element-scalar-properties-p element)
        (unless (scalar-property-p property)
          (error
           "element with scalar property can have scalar properties only")))
      (when (element-list-properties-p element)
        (error "element can have one list property at most"))
      (setf raw-properties (acons-last name property raw-properties)))))

(defun element-properties (element)
  (mapcar #'cdr (element-raw-properties element)))

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

(defun property-p (property)
  (or (scalar-property-p property)
      (list-property-p property)))

(defun property-name (property)
  (cond
    ((scalar-property-p property) (scalar-property-name property))
    ((list-property-p property)   (list-property-name property))))


;;;
;;; Comment
;;;

(defstruct (comment (:constructor %make-comment))
  text)

(defun make-comment (header)
  (%make-comment :text (comment-header-text header)))


;;;
;;; Plyfile state
;;;

(defstruct (state (:constructor %make-state))
  (name :read-only)
  element-names)

(defun make-state (element-names)
  (if element-names
      (%make-state :name :ready
                   :element-names element-names)
      (%make-state :name :finish
                   :element-names nil)))

(defun valid-state-p (state)
  (let ((state-name   (state-name state))
        (element-name (state-current-element-name state)))
    (or (and (eq state-name :ready)
             (stringp element-name))
        (and (eq state-name :reading)
             (stringp element-name))
        (and (eq state-name :finish)
             (null element-name)))))

(defun state-current-element-name (state)
  (car (state-element-names state)))

(defun state-current-p (state state-name &optional element-name)
  (assert (valid-state-p state))
  (assert (or (and (eq state-name :ready)
                   (stringp element-name))
              (and (eq state-name :reading)
                   (stringp element-name))
              (and (eq state-name :finish)
                   (null element-name))))
  (let ((current-state-name   (state-name state))
        (current-element-name (state-current-element-name state)))
    (and (eq current-state-name state-name)
         (string= current-element-name element-name))))

(defun state-current (state)
  (assert (valid-state-p state))
  (let ((state-name (state-name state))
        (current-element-name (state-current-element-name state)))
    (case state-name
      (:ready   (list :ready current-element-name))
      (:reading (list :reading current-element-name))
      (:finish  :finish))))
        
(defun transfer-state (state)
  (assert (valid-state-p state))
  (symbol-macrolet ((state-name (state-name state))
                    (state-element-names (state-element-names state)))
    (case state-name
      (:ready (setf state-name :reading))
      (:reading (if (cdr state-element-names)
                    (setf state-name :ready
                          state-element-names (cdr state-element-names))
                    (setf state-name :finish
                          state-element-names (cdr state-element-names)))))))


;;;
;;; Make PLY headers
;;;

(defun make-ply-header (line)
  (let ((keyword (car (cl-ppcre:split "\\s+" line))))
    (cond
      ((string= keyword "ply")        (make-magic-header line))
      ((string= keyword "format")     (make-format-header line))
      ((string= keyword "element")    (make-element-header line))
      ((string= keyword "property")   (make-property-header line))
      ((string= keyword "comment")    (make-comment-header line))
      ((string= keyword "end_header") (make-end-header line))
      (t (error "invalid header: ~A" line)))))    


;;;
;;; Magic header
;;;

(defstruct (magic-header (:constructor %make-magic-header)))

(defparameter +magic-header-regexp+ "^ply$")

(defun make-magic-header (line)
  (unless (cl-ppcre:scan +magic-header-regexp+ line)
    (error "invalid magic header: ~A" line))
  (%make-magic-header))


;;;
;;; Format header
;;;

(defstruct (format-header (:constructor %make-format-header))
  (file-type :read-only)
  (version :read-only))

(defparameter +format-header-regexp+
  "^format\\s+(ascii|binary_big_endian|binary_little_endian)\\s+(1.0)$")

(defparameter +parse-ply-file-type-error-string+
  "PLY file type must be one of \"ascii\", \"binary_big_endian\" or \"binary_little_endian\"")

(defun parse-ply-file-type (string)
  (cond
    ((string= string "ascii")                :ascii)
    ((string= string "binary_big_endian")    :binary-big-endian)
    ((string= string "binary_little_endian") :binary-little-endian)
    (t (error +parse-ply-file-type-error-string+))))

(defun make-format-header (line)
  (unless (cl-ppcre:scan +format-header-regexp+ line)
    (error "invalid format header: ~A" line))
  (cl-ppcre:register-groups-bind ((#'parse-ply-file-type file-type) version)
      (+format-header-regexp+ line)
    (%make-format-header :file-type file-type
                         :version   version)))


;;;
;;; Element header
;;;

(defstruct (element-header (:constructor %make-element-header))
  (name :read-only)
  (size :read-only))

(defparameter +element-header-regexp+ "^element\\s+(\\w+)\\s+([1-9]\\d*)$")

(defun make-element-header (line)
  (unless (cl-ppcre:scan +element-header-regexp+ line)
    (error "invalid element header: ~A" line))
  (cl-ppcre:register-groups-bind (name (#'parse-integer size))
      (+element-header-regexp+ line)
    (%make-element-header :name name :size size)))


;;;
;;; Property header
;;;

(defstruct (scalar-property-header (:constructor %make-scalar-property-header))
  (type :read-only)
  (name :read-only))

(defstruct (list-property-header (:constructor %make-list-property-header))
  (count-type :read-only)
  (element-type :read-only)
  (name :read-only))

(defparameter +scalar-property-header-regexp+
  "^property\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defparameter +list-property-header-regexp+
  "^property\\s+list\\s+(uchar|ushort|uint)\\s+(char|uchar|short|ushort|int|uint|float|double)\\s+(\\w+)$")

(defun make-property-header (line)
  (unless (or (cl-ppcre:scan +scalar-property-header-regexp+ line)
              (cl-ppcre:scan +list-property-header-regexp+ line))
    (error "invalid property header: ~A" line))
  ;; make scalar property header
  (cl-ppcre:register-groups-bind ((#'parse-ply-type type) name)
      (+scalar-property-header-regexp+ line)
    (return-from make-property-header
      (%make-scalar-property-header :type type :name name)))
  ;; make list property header
  (cl-ppcre:register-groups-bind ((#'parse-ply-type count-type)
                                  (#'parse-ply-type element-type)
                                  name)
      (+list-property-header-regexp+ line)
    (return-from make-property-header
      (%make-list-property-header :count-type count-type
                                  :element-type element-type
                                  :name name))))

(defun property-header-p (header)
  (or (scalar-property-header-p header)
      (list-property-header-p header)))

(defun property-header-name (header)
  (assert (property-header-p header))
  (cond
    ((scalar-property-header-p header) (scalar-property-header-name header))
    ((list-property-header-p header)   (list-property-header-name header))))


;;;
;;; Comment header
;;;

(defstruct (comment-header (:constructor %make-comment-header))
  (text :read-only))

(defparameter +comment-header-regexp+ "^comment\\s+(.+)$")

(defun make-comment-header (line)
  (unless (cl-ppcre:scan +comment-header-regexp+ line)
    (error "invalid comment header: ~A" line))
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
    (error "invalid end header: ~A" line))
  (%make-end-header))


;;;
;;; Types for PLY format
;;;

(defun parse-ply-type (string)
  (cond
    ((string= string "char")   :char)
    ((string= string "uchar")  :uchar)
    ((string= string "short")  :short)
    ((string= string "ushort") :ushort)
    ((string= string "int")    :int)
    ((string= string "uint")   :uint)
    ((string= string "float")  :float)
    ((string= string "double") :double)
    (t (error "invalid string for type: ~A" string))))

(defparameter +non-negative-integer-regex+ "^([1-9]\\d*|0)$")

(defun parse-non-negative-integer (string)
  (unless (cl-ppcre:scan +non-negative-integer-regex+ string)
    (error "invalid string for unsigned integer: ~S" string))
  (parse-integer string))

(defparameter +float-regex1+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?$")
(defparameter +float-regex2+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?(e(\\+|-)?\\d+)?$")
(defparameter +float-regex3+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?(s(\\+|-)?\\d+)?$")
(defparameter +float-regex4+
  "^(\\+|-)?([1-9]\\d*|0)(\\.\\d+)?(d(\\+|-)?\\d+)?$")

(defun parse-single-float (string)
  (cond
    ((cl-ppcre:scan +float-regex1+ string)
     (let ((s0 (concatenate 'string string "s0")))
       (read-from-string s0)))
    ((cl-ppcre:scan +float-regex2+ string)
     (let ((s0 (cl-ppcre:regex-replace "e" string "s")))
       (read-from-string s0)))
    ((cl-ppcre:scan +float-regex3+ string)
     (read-from-string string))
    (t (error "invalid string for single float value: ~S" string))))

(defun parse-double-float (string)
  (cond
    ((cl-ppcre:scan +float-regex1+ string)
     (let ((s0 (concatenate 'string string "d0")))
       (read-from-string s0)))
    ((cl-ppcre:scan +float-regex2+ string)
     (let ((s0 (cl-ppcre:regex-replace "e" string "d")))
       (read-from-string s0)))
    ((cl-ppcre:scan +float-regex4+ string)
     (read-from-string string))
    (t (error "invalid string for double float value: ~S" string))))


;;;
;;; Operations for alist
;;;

(defun acons-last (key datum alist)
  (nreverse (acons key datum (nreverse alist))))


;;;
;;; Operations for list
;;;

(defun cons-last (se1 se2)
  (nreverse (cons se1 (nreverse se2))))

