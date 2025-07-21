;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: cl-prebid/parenscript; -*- 

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;; 
;;; License "Golden Rule License"
;;;
;;; First   (load "cl-prebid.asd")
;;; then    (asdf:oos 'asdf:load-op :cl-prebid/parenscript)
;;;

(defpackage #:cl-prebid/parenscript
  (:use
   :cl
   :parenscript
   :water
   :paren6)
  (:documentation
   "Collection of parenscript package and additions."))
