# Cl-Ply

A library to handle PLY file format which is known as the Polygon File Format or the Stanford Triangle Format in Common Lisp.

## Usage

With `t/test.ply` file:

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

Do something with a element per line:

    (cl-ply:with-ply-file (plyfile "t/test.ply")
    
      ;; do something with scalar properties of "vertex" element
      (cl-ply:with-ply-element ((x y z) "vertex" plyfile)
        (do-something :for 'vertex :x x :y y :z))
    
      ;; do something with list properties of "face" element
      (cl-ply:with-ply-element (vertex-indices "face" plyfile)
        (do-something :for 'face :with vertex-indices)))

Get a list of properties with a element:

    (cl-ply:with-ply-file (plyfile "t/test.ply")
    
      ;; list of scalar properties of "vertex" element
      (cl-ply:read-ply-element "vertex" plyfile)  ; => ((0.0 1.0 2.0) (3.0 4.0 5.0))
    
      ;; list of list properties of "face" element
      (cl-ply:read-ply-element "face" plyfile))   ; => ((0 1 2 3) (4 5 6 7))


## Installation

Since cl-ply is not available in Quicklisp yet, please use Quicklisp's local-projects feature to use it.

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/takagi/cl-ply.git

Then `(ql:quickload :cl-ply)` from `REPL` to load it.

I will apply registering cl-ply to Quicklisp later. After it will be accepted, you will be able to load cl-ply just `(ql:quickload :cl-ply)` only.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the LLGPL License.

