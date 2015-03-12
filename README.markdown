# Cl-Ply

Cl-ply is a library to handle PLY format which is also known as the Stanford Triangle Format.

## Example

Here shows hot to read PLY file with cl-ply.

The following is an example PLY file:

    ply
    format ascii 1.0
    comment this is test ply data.
    element vertex 2
    property float x
    property float y
    property float z
    element face 2
    property list uchar int vertex_indices
    end_header
    0.0 1.0 2.0
    3.0 4.0 5.0
    4 0 1 2 3
    4 4 5 6 7

It contains two elements, `vertex` that has three float properties and `face` that has a list property of integer.

You can read the PLY file as following:

    ;; open a PLY file, closed automatically when control leaves
    (cl-ply:with-ply-for-reading (plyfile #P"/path/to/file.ply")
      ;; read and print vertex elements
      (loop repeat (cl-ply:ply-element-size plyfile "vertex")
         do (format t "element vertex ~S~%"
             (cl-ply:ply-read-element plyfile "vertex")))
      ;; read and print face elements
      (loop repeat (cl-ply:ply-element-size plyfile "face")
         do (format t "element face ~S~%"
             (cl-ply:ply-read-element plyfile "face"))))

## Installation

Cl-ply can be installed via Quicklisp.

    (ql:quickload :cl-ply)

## API

### [Macro] with-ply-for-reading

    WITH-PLY-FOR-READING (ply filespec) form* => results

Opens a file stream named by `filespec` and create a ply object, reading PLY headers from the file. The ply object is bound to `ply` variable. `with-ply` evaluates `form` as an implicit progn with `ply` and returns the result values. When control leaves the forms, either normally and abnormally, the file stream is automatically closed.

### [Function] ply-open-for-reading

    OPEN-PLY-FOR-READING filespec => ply object

bla bla bla.

### [Function] ply-close

    PLY-CLOSE ply => result

bla bla bla.

### [Function] ply-element-names

    PLY-ELEMENT-NAMES ply => element-names

Returns all names of elements in `ply`.

### [Function] ply-element-size

    PLY-ELEMENT-SIZE ply element-name => size

Returns the number of elements named by `element-name` in `ply`.

### [Function] ply-read-element

    PLY-READ-ELEMENT ply element-name => result

Reads an element of `element-name` from `ply` and returns as a list of its properties.

### [Function] ply-comments

    PLY-COMMENTS ply => comments

Returns a list of comments in `ply`.

### [Function] ply-obj-info

    PLY-OBJ-INFO ply => obj_info

Returns object information in `ply`.

## FAQ

**Q. Does cl-ply support writing PLY format?**

A. No. Currenly, only reading PLY format is supported.

**Q. Does cl-ply support reading / writing PLY format in binary type?**

A. No. Currently, only ASCII type is supported.

## Reference

* [PLY - Polygon File Format](http://paulbourke.net/dataformats/ply/)

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.
