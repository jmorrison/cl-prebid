;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;


(in-package #:cl-prebid/reblocks)

#|
(trace REBLOCKS-UI2/WIDGET:GET-DEPENDENCIES)
(trace REBLOCKS/DEPENDENCIES:GET-DEPENDENCIES)
(trace REBLOCKS-UI2/CONTAINERS/TABS/THEMES/TAILWIND::GET-DEPENDENCIES)
(trace CL-PREBID/REBLOCKS::GET-DEPENDENCIES)
|#

#+NIL
(defmethod reblocks-ui2/themes/tailwind::get-dependencies ((widget reblocks-ui2/widget:ui-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (clouseau:inspect (list :get-dependencies 00 widget theme))
  (list*
   (reblocks/dependencies:make-dependency
    #P"./my-tailwind/output.css.00000000"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))

;;
;; This binds the STRING values of the various HTML parameters (in lowercase)
;;

(defmacro reblocks-request-params (lambda-list &body body)
  (let ((temp-var (gensym)))
    `(let ((,temp-var (reblocks/request:get-parameters)))
       #+NIL (clouseau:inspect ,temp-var)
       (let ,(loop for var in lambda-list
		   for i from 0
		   collect `(,var (aget ,temp-var ,(string-downcase (symbol-name var)))))
	 ,@body))))

;;
;; (reblocks/preview:preview (make-instance 'todo::cl-prebid-container))
;;

(defwidget cl-prebid-container (reblocks-ui2/containers/column:column-widget)
  ((reblocks-ui2/containers/container:subwidgets
    :initform (list
	       (reblocks/widgets/string-widget:make-string-widget "foo")
	       (reblocks/widgets/string-widget:make-string-widget "bar")))
   (content
    :initarg :content
    :accessor content
    :initform nil)))

(defmethod reblocks-ui2/widget:render ((cpc cl-prebid-container) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list :reblocks-ui2/widget-render cpc))
  (with-html ()
    (:div :id "banner"
	  :class "banner"
	  (:h1 :class "text-2xl my-8" "cbc h1")
	  (:a "banner text"))
    (:div :class "text-8xl text-gray-500 dar:text-gray-400" (:a "FOOO"))
    (:div :content
	  (reblocks-ui2/widget:render (content cpc) theme))
    (:div :id "bodystuff"
	  :class "bodystuff"
	  (:a "bodystuff text")
	  (loop for item in (reblocks-ui2/containers/container:subwidgets cpc) do
		#+NIL (clouseau:inspect item)
		(reblocks-ui2/widget:render item theme)))))

;; https://40ants.com/reblocks/dependencies/#x-28REBLOCKS-2FDOC-2FDEPENDENCIES-3A-3A-40DEPENDENCIES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29

#-NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget cl-prebid-container) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #-NIL (clouseau:inspect (list :get-dependencies widget theme))
  (list*
   (reblocks-lass:make-dependency
    '(.banner
      :border 20px solid "red"
      :padding 10em
      :background-color "blue"))
   (reblocks-lass:make-dependency
    '(.bodystuff
      :border 20px solid "green"
      :padding 10em
      :background-color "pink"))
   (reblocks/dependencies:make-dependency
    #P"./Prebid.js/dist/not-for-prod/prebid.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (reblocks/dependencies:make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method))) ; Otherwise a remote CDN dependency upon tailwind

;;;;;; Application

#-NIL
(defun wrap-with-frame (widget)
  ;; (clouseau:inspect (list 1 widget))
  (make-instance 'cl-prebid-container
		 :content widget))

;;;;
;;;; Pages
;;;;


(defwidget a-string-widget (ui-widget)
  ((content :accessor content :initarg :content :initform "")))

#+NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget a-string-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (clouseau:inspect (list 1 :get-dependencies :a-string-widget widget theme))
  (list*
   (reblocks/dependencies:make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))

;;
;; If I don't define THIS method, then the CDN version of tailwind will be fetched!
;;

#-NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget reblocks-ui2/widget:ui-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (clouseau:inspect (list :get-dependencies :reblocks-ui2/ui-widget widget theme))
  (list*
   (reblocks/dependencies:make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))

;;
;; This should block the built-in hard-coded CDN fetch of tailwind
;;

#+NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget reblocks-ui2/widget:ui-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (clouseau:inspect (list :get-dependencies :reblocks-ui2/ui-widget widget theme))
  (list*
   (reblocks/dependencies:make-dependency
     "https://cdn.tailwindcss.com/3.3.5/FOOOOOOOOOOOO"
     ;; Old URLs:
     ;; "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"
     ;; "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio,line-clamp"
     :type :js)
   (call-next-method)))


(defmethod render ((widget a-string-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (reblocks-request-params (foo baz frob)
		     (with-html ()
		       (:div :class "text-2xl my-8"
			     (:p (content widget))
			     (:p (format nil "foo ~s" foo))
			     (:p (format nil "baz ~s" baz))
			     (:p (format nil "looking up frob ~s" frob))))))

;;;;
;;;; App
;;;;

(defapp my-app
    :prefix "/"
    :page-constructor #'wrap-with-frame
    :routes ((page ("/" :name "root") (make-instance 'a-string-widget :content "ROOT"))
	     (page ("/one" :name "foo") (make-instance 'a-string-widget :content "ONE"))
	     (page ("/two" :name "two") (make-instance 'a-string-widget :content "TWO"))))

;;;;
;;;; Plumbing
;;;;


(defun start (&rest args
	      &key
		(port (parse-integer
		       (or (uiop:getenv "PORT")
			   reblocks-ui2-demo/server::*default-port*)))
		(server-type :hunchentoot)
		(interface reblocks-ui2-demo/server::*default-interface*)
		(request-timeout reblocks-ui2-demo/server::*default-request-timeout*)
		(debug t))

  (setf reblocks/variables:*pages-expire-in* (* 10 60))
  (setf reblocks/variables:*max-pages-per-session* 10)

  (setf *current-theme*
        (reblocks-ui2/themes/tailwind:make-tailwind-theme))

  #-NIL
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'my-app
                         :server-type server-type
                         :request-timeout request-timeout
                         :debug debug)

  #+NIL
  (apply 'reblocks-ui2-demo/server::start args)
  )
