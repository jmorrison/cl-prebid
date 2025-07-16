;;; -*- Mode: Lisp; Package: cl-prebid -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2022.
;;;

(in-package :cl-prebid)

(defun slurp-javascript-file (relative-pathname)
  (let ((absolute-pathname (asdf:system-relative-pathname :cl-prebid relative-pathname)))
    (cond ((cl-fad:file-exists-p absolute-pathname)
	   (uiop:read-file-string absolute-pathname))
	  (t
	   (error "slurp-javascript-file ~s: no such file" relative-pathname)))))
