;;; -*- Mode: Lisp; Package: cl-prebid/weblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(in-package :cl-prebid/weblocks)

(defwebapp cl-prebid-weblocks
    :name "CL-Prebid-Weblocks"
    :prefix "/cl-prebid-weblocks"
    :description "Test for CL-Prebid-Weblocks"
    :init-user-session #'init-user-session
    :autostart t
    :debug t
    :html-indent-p t
    :js-backend :jquery
    :dependencies
    (list
      (make-instance 'script-dependency
		     :url (make-instance 'puri:uri
					 :path "/geoblackboard/pub/scripts/jquery-1.8.2.js")
		     :local-path "./assets/jquery-1.8.2/jquery-1.8.2.js")
      (make-instance 'script-dependency
		     :url (make-instance 'puri:uri
					 :path "/geoblackboard/pub/scripts/jquery-1.8.2.js")
		     :local-path "./assets/jquery-1.8.2/jquery-1.8.2.js")))

(defmethod initialize-instance :after ((self cl-prebid-weblocks) &rest args)
  #+NIL (setf (public-files-uri-prefix self) (concatenate
					      'string
					      (weblocks-webapp-prefix self)
					      "/pub"))

  (when (log:debug)
    (format t "initialize-instance ~a" self)
  self)

(defmethod initialize-webapp :after ((self cl-prebid-weblocks))
  (format t "initialize-webapp~%")
  self)

(defmethod render-page-headers :before ((self cl-prebid-weblocks))
  (clouseau:inspect (list 3 self))
  #+NIL (clouseau:inspect weblocks:*current-page-headers*)
  #+NIL (clouseau:inspect *weblocks-server*)
  #-NIL (pushnew
	 (lambda (&rest args)
	   (clouseau:inspect (list :foobar args))
	   (list "<base href=\"foobar\">"))
	 weblocks:*current-page-headers*)
  #+NIL (setf *current-page-headers* '(:base "/foobar"))
  #+NIL (setf (hunchentoot:header-out "base") "/foobar")
  )

(defun run (&key (acceptor *normal-acceptor* acceptor-supplied-p))
  (start-webapp 'cl-prebid-weblocks))
