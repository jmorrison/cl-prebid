;;; -*- Mode: Lisp; Package: cl-prebid/hunchentoot -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(in-package :cl-prebid/hunchentoot)

(defvar *normal-acceptor* (make-instance
			   #-NIL 'hunchentoot:easy-acceptor
			   #+NIL 'acceptor-which-logs
			   :name 'normal-acceptor
			   ;; 'hunchentoot:easy-ssl-acceptor
			   ;; :ssl-certificate-file #P"symbology.crt"
			   ;; :ssl-privatekey-file #P"symbology.key"
			   :port 4242
			   :document-root (cl-fad:merge-pathnames-as-directory
					   (uiop/os:getcwd)
					   #P"www/")))



(defun run (&key (acceptor *normal-acceptor* acceptor-supplied-p))
  (hunchentoot:start acceptor))
