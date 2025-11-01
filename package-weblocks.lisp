;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: cl-user; -*-                                 
           
;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(defpackage #:cl-prebid/weblocks
  (:use
   :cl
   :cl-prebid
   ;; :hunchentoot
   :weblocks
   :cl-who
   :cl-fad)
  (:import-from :hunchentoot
                #:header-in
                #:set-cookie
                #:set-cookie*
                #:cookie-in
                #:user-agent
                #:referer
                #:define-easy-handler
                #:content-type*)
#|
  (:import-from :rune-dom
		#:dovector)
|#
  (:export
   #:run
   )
  )
