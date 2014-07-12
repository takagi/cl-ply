# Cl-Ply

A library to handle PLY file format which is known as the Polygon File Format or the Stanford Triangle Format in Common Lisp.

## Example

Here shows how to read PLY format with cl-ply.

An example PLY data is following:

    ply
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
    4 4 5 6 7

There are an element `vertex` with three float properties and another element `face` with a list property of type int.

You can read this PLY data into arrays in imperative style as following:

    (cl-ply:with-ply-file (plyfile "/path/to/file.ply")
      (let ((vertex-size (cl-ply:plyfile-element-size plyfile "vertex"))
            (face-size (cl-ply:plyfile-element-size plyfile "face")))
        (let ((vertices (make-array vertex-size))
              (faces (make-array face-size)))
          ;; read vertices
          (loop repeat vertex-size
                for i from 0
             do (cl-ply:with-ply-element ((x y z) plyfile)
                  (setf (aref vertices i) (list x y z))))
          ;; read faces
          (loop repeat face-size
                for i from 0
             do (cl-ply:with-ply-element (vertex-index plyfile)
                  (setf (aref faces i) vertex-index)))
          ;; return vertex and face arrays
          (values vertices faces))))

Additionally, cl-ply also provides a functional style interface.

    (cl-ply:read-ply-elements "/path/to/file.ply" "vertex") => ((0.0 1.0 2.0) (3.0 4.0 5.0))
    (cl-ply:read-ply-elements "/path/to/file.ply" "face") => ((0 1 2 3) (4 5 6 7))

Although the imperative one would be superior in performance, the functional one would be convenient in some cases.

## Installation

You can install cl-ply via Quicklisp:

    (ql:quicklisp :cl-ply)

## API

### [Macro] with-plyfile

    WITH-PLYFILE (plyfile filespec) &body body => results

Opens a file stream to named by `filespec` and create a plyfile object, reading PLY headers from the file. The plyfile object is bound to `plyfile` variable. `with-plyfile` evaluates `body` as an implicit progn with `plyfile` and returns the result values. When control leaves the body, either normally and abnormally, the stream is automatically closed.

### [Function] plyfile-element-size

    PLYFILE-ELEMENT-SIZE plyfile element-name => size

Returns the number of elements in `plyfile` named by `element-name`.

### [Function] read-ply-element

    READ-PLY-ELEMENT plyfile => result

Reads an element from `plyfile` and returns it as a list.

### [Macro] with-ply-element

    WITH-PLY-ELEMENT (vars plyfile) &body body => results

Reads the next element of `plyfile` using `read-ply-element` in its expanded form and binds the result to `vars` with destructuring. Then evaluates `body` and returns its result values.

### [Function] read-ply-elements

    READ-PLY-ELEMENTS filespec element-name => element-list

Reads all elements named by `element-name` from a file `filespec` and returns them as a list. The other elements before `element-name` element are skipped.

## FAQ

**Q. Does cl-ply support writing PLY format?**

A. Currenly, only reading PLY format is supported.

**Q. Does cl-ply support reading / writing PLY format in binary type?**

A. Currently, only ASCII type is supported.

**Q. Is an element able to be read which has scalar properties and list properties?**

A. Since such element supposed to be nonsense in PLY format, cl-ply does not support reading it and causes an error.

## Reference

* [PLY - Polygon File Format](http://paulbourke.net/dataformats/ply/)

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.
