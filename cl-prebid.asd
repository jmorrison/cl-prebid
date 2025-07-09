;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: cl-user; -*- 

;;;
;;; Copyright Symbolic Simulation, LLC, 2012.
;;; 
;;; License "Golden Rule License"
;;;
;;; First   (load "cl-prebid.asd")
;;; then    (asdf:oos 'asdf:load-op :cl-prebid)
;;; finally (cl-prebid:run)
;;;

(defpackage #:cl-prebid.asd
  (:use #:cl #:asdf)
  (:export #:run))

(defpackage #:cl-prebid)
(defpackage #:com.symsim.utils)		; Forward reference for the source-control-hack

(in-package :cl-prebid.asd)

(defsystem #:cl-prebid
    :name "cl-prebid"
    :version "0.0.1"
    :maintainer "jm@symbolic-simulation.com"
    :author "jm@symbolic-simulation.com"
    :licence "Golden Rule License"
    :description "cl-prebid implementation"
    :depends-on (#:hunchentoot
		 #:log4cl
		 #:cl-who
		 #:cl-css		; Hopefully, for CSS use
		 #-RELEASE #:clouseau ; For debugging
		 #:cl-cryptolens      ; For license management
		 #:com.symsim.oss.global-params	; For configurability
		 ;; #:com.symsim.licensing		; For products, services, and hunchentoot additions
		 #:com.symsim.utils ; For hunchentoot additions
		 )
    :components ((:file "package")
		 (:file "easy-handlers" :depends-on ("package"))
		 (:file "cl-prebid" :depends-on ("package"))
		 )
    :perform (load-op :after (op c)
		      (provide "cl-prebid") ; string designator
		      (format t "~&;; try (cl-prebid:run)~%")))
