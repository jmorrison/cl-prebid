;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(ql:quickload :clouseau)

(load "../../dists/quicklisp/software/reblocks-20241012-git/examples/simple-form.lisp")



;; (in-package :reblocks-examples/simple-form)

#+NIL
(defmethod reblocks/page:render-headers :before (app)
  (format t "================================ reblocks/page:render-headers ~s~%" app)
  (clouseau:inspect `(:render-headers :app ,app)))

(defvar *prebid-dep* (reblocks/dependencies:make-dependency
	       "./Prebid.js/dist/not-for-prod/prebid.js"
	       :system :reblocks
	       :cache-in-memory t
	       )
  )

#+NIL
(defmethod reblocks/page:render-headers :around (app)
  (format t "================================ reblocks/page:render-headers ~s~%" app)

  (format t "1111~%")
  (describe app)
  (clouseau:inspect app)

  (format t "2222~%")
  (describe *prebid-dep*)
  (clouseau:inspect *prebid-dep*)

  (format t "3333~%")
  (describe (reblocks/page-dependencies::push-dependency *prebid-dep*))

  (format t "4444~%")
  (describe (reblocks/dependencies:get-dependencies app))
  (clouseau:inspect (reblocks/dependencies:get-dependencies app))

  (format t "5555~%")

  (describe (reblocks/page:current-page))
  (clouseau:inspect (reblocks/page:current-page))

  (format t "6666~%")

  (describe (reblocks/page:page-metadata (reblocks/page:current-page) :dependencies))
  (clouseau:inspect (reblocks/page:page-metadata (reblocks/page:current-page) :dependencies))

  (format t "7777~%")

  (describe (reblocks/page:current-page))
  (clouseau:inspect (reblocks/page:current-page))

  (format t "8888~%")
  (let ((foo (call-next-method)))
    (clouseau:inspect `(:render-headers-around :app ,app :call-next-method ,foo)))
  #+NIL (clouseau:inspect `(:render-headers :app ,app)))

#+NIL
(defmethod reblocks/page:render-body :before (app inner-html)
  (format t "================================ reblocks/page:render-body ~s~%" app)
  #+NIL (clouseau:inspect `(:render-body :app ,app :inner-html ,inner-html)))

#+NIL
(defmethod reblocks/page:render :before ((app reblocks-examples/simple-form::simple-form) inner-html &rest rest)
  ;; (error "reblocks/page:render")
  (format t "================================ reblocks/page:render ~s~%" app)
  (clouseau:inspect `(:render :app ,app :inner-html ,inner-html)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Cut-and-paste from examples/simple-form.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(uiop:define-package #:reblocks-examples/simple-form
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/server)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:update
                #:defwidget)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:documentation "This example demonstrating a simple web-page with a form having multiple fields and submit button.

                   When user pushes the submit button, entered data are processed on the backend and are rendered in read-only mode."))
(in-package #:reblocks-examples/simple-form)


(defapp simple-form
  :prefix "/")


(defun start (&key (port 8080) (interface "localhost") )
  (reblocks/server:stop interface
                        port)
  (reblocks/server:start :apps 'simple-form
                         :port port
                         :interface interface))


(defwidget form-page (reblocks-ui:ui-widget)
  ((edit-mode :initform t
              :accessor edit-mode-p)
   (name :initform ""
         :accessor name)
   (name-error :initform nil
               :accessor name-error)
   (profession :initform ""
               :accessor profession)
   (bio :initform ""
        :accessor bio)))


(defmethod reblocks/page:init-page ((app simple-form) (url-path string) expire-at)
  (check-type expire-at (or null local-time::timestamp))
  (make-instance 'form-page))


(defmethod reblocks/widget:render ((widget form-page))
  (labels ((to-edit-mode (&rest args)
             (log:info "Switching to edit mode" args)
             (setf (edit-mode-p widget)
                   t)
             (update widget))
           (valid-p (data)
             (let ((has-errors nil))
               (setf (name-error widget)
                     nil)
               
               (when (or (null (getf data :name))
                         (string= "" (getf data :name)))
                 (setf has-errors t)
                 (setf (name-error widget)
                       "Name is required field"))
               (values (not has-errors))))
           (handle-submission (&rest args &key submit-button &allow-other-keys)
             (log:info "Form was submitted" args)
             (cond
               ((and (not (null submit-button))
                     (string-equal submit-button
                                   "cancel"))
                (setf (edit-mode-p widget)
                      nil))
               ;; Saving
               (t
                (when (valid-p args)
                  (setf (name widget)
                        (getf args :name))
                  (setf (profession widget)
                        (getf args :profession))
                  (setf (bio widget)
                        (getf args :bio))
                  (setf (edit-mode-p widget)
                        nil))))
             (update widget)))
    
    (with-html
      (:div :style "width: 50%; margin: 4rem auto"
            (cond
              ((edit-mode-p widget)
               (reblocks-ui/form:with-html-form (:post #'handle-submission)
                 (when (name-error widget)
                   (:div :class "label alert"
                         (name-error widget)))
                 (:input :type "text"
                         :name "name"
                         :placeholder "Enter your name"
                         :value (name widget))

                 (:select :name "profession"
                          (loop for profession in '("Nothing special"
                                                    "Software Developer"
                                                    "Designer"
                                                    "Product Manager")
                                for selected = (string= (profession widget)
                                                        profession)
                                do (:option :selected selected
                                            profession)))
                 (:textarea :name "bio"
                            (bio widget))
                 
                 ;; This works:
                 (:button :type "submit"
                          :class "button"
                          :name "submit-button"
                          :value "submit"
                          "Submit")
                 (:button :class "button secondary"
                          :name "submit-button"
                          :value "cancel"
                          "Cancel")

                 
                 ;; And this should work too:
                 ;; (:input :class "button"
                 ;;         :name "submit-button"
                 ;;         :type "submit"
                 ;;         :value "submit")
                 ;; (:input :class "button secondary"
                 ;;         :type "submit"
                 ;;         :name "submit-button"
                 ;;         :value "cancel")
                 ))
              (t
               (:dl
                (:dt "Name")
                (:dd (name widget))
                (:dt "Profession")
                (:dd (profession widget))
                (:dt "Bio")
                (:dd (bio widget)))
               
               (render-form-and-button "Edit"
                                       #'to-edit-mode)))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; this is a try
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-prebid/reblocks)

;;
;; Our test widget
;;

(reblocks/widget:defwidget cl-prebid-widget (reblocks-ui:ui-widget)
  ((edit-mode :initform t
              :accessor edit-mode-p)
   (name :initform ""
         :accessor name)
   (name-error :initform nil
               :accessor name-error)
   (profession :initform ""
               :accessor profession)
   (bio :initform ""
        :accessor bio)))

(defmethod reblocks/widget:render ((widget cl-prebid-widget))
  (labels ((to-edit-mode (&rest args)
             (log:info "Switching to edit mode" args)
             (setf (edit-mode-p widget)
                   t)
             (update widget))
           (valid-p (data)
             (let ((has-errors nil))
               (setf (name-error widget)
                     nil)
               
               (when (or (null (getf data :name))
                         (string= "" (getf data :name)))
                 (setf has-errors t)
                 (setf (name-error widget)
                       "Name is required field"))
               (values (not has-errors))))
           (handle-submission (&rest args &key submit-button &allow-other-keys)
             (log:info "Form was submitted" args)
             (cond
               ((and (not (null submit-button))
                     (string-equal submit-button
                                   "cancel"))
                (setf (edit-mode-p widget)
                      nil))
               ;; Saving
               (t
                (when (valid-p args)
                  (setf (name widget)
                        (getf args :name))
                  (setf (profession widget)
                        (getf args :profession))
                  (setf (bio widget)
                        (getf args :bio))
                  (setf (edit-mode-p widget)
                        nil))))
             (update widget)))
    
    (with-html
      (:div :style "width: 50%; margin: 4rem auto"
            (cond
              ((edit-mode-p widget)
               (reblocks-ui/form:with-html-form (:post #'handle-submission)
                 (when (name-error widget)
                   (:div :class "label alert"
                         (name-error widget)))
                 (:input :type "text"
                         :name "name"
                         :placeholder "Enter your name"
                         :value (name widget))

                 (:select :name "profession"
                          (loop for profession in '("Nothing special"
                                                    "Software Developer"
                                                    "Designer"
                                                    "Product Manager")
                                for selected = (string= (profession widget)
                                                        profession)
                                do (:option :selected selected
                                            profession)))
                 (:textarea :name "bio"
                            (bio widget))
                 
                 ;; This works:
                 (:button :type "submit"
                          :class "button"
                          :name "submit-button"
                          :value "submit"
                          "Submit")
                 (:button :class "button secondary"
                          :name "submit-button"
                          :value "cancel"
                          "Cancel")

                 
                 ;; And this should work too:
                 ;; (:input :class "button"
                 ;;         :name "submit-button"
                 ;;         :type "submit"
                 ;;         :value "submit")
                 ;; (:input :class "button secondary"
                 ;;         :type "submit"
                 ;;         :name "submit-button"
                 ;;         :value "cancel")
                 ))
              (t
               (:dl
                (:dt "Name")
                (:dd (name widget))
                (:dt "Profession")
                (:dd (profession widget))
                (:dt "Bio")
                (:dd (bio widget)))
               
               (reblocks-ui/form:render-form-and-button "Edit"
                                       #'to-edit-mode)))))))

;;
;;
;;

;;
;; A page with the Right Stuff
;;
    
(defclass cl-prebid-page (reblocks/page:page)
  ())

(reblocks/app:defapp prebid-app
    :prefix "/")

(reblocks-navigation-widget:defroutes frobnatz
    ("/" (reblocks/widgets/string-widget:make-string-widget "STRING 0"))
  ("/string1" (reblocks/widgets/string-widget:make-string-widget "STRING 1"))
  ("/string2" (reblocks/widgets/string-widget:make-string-widget "STRING 2"))
  ("/string3" (reblocks/widgets/string-widget:make-string-widget "STRING 3"))
  ("/prebid" (make-instance 'cl-prebid-widget))
  #+NIL ("/form-page" (make-instance 'reblocks-examples/simple-form::simple-form)))

(defmethod reblocks/page:init-page ((app prebid-app) (url-path string) expire-at)
  (check-type expire-at (or null local-time::timestamp))
  (make-frobnatz))

;;
;;
;;

(defun start (&key (port 8080) (interface "localhost") )
  (reblocks/server:stop interface
                        port)
  (reblocks/server:start :apps 'prebid-app
                         :port port
                         :interface interface))




