;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;


(in-package #:cl-prebid/reblocks)

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
  (clouseau:inspect (list :reblocks-ui2/widget-render cpc))
  (with-html ()
    (:div :id "banner"
	  :class "banner"
	  (:h1 :class "text-2xl my-8" "cbc h1")
	  (:a "banner text"))
    (:div :content
	  (reblocks-ui2/widget:render (content cpc) theme))
    (:div :id "bodystuff"
	  :class "bodystuff"
	  (:a "bodystuff text")
	  (loop for item in (reblocks-ui2/containers/container:subwidgets cpc) do
		#+NIL (clouseau:inspect item)
		(reblocks-ui2/widget:render item theme)))))

;; https://40ants.com/reblocks/dependencies/#x-28REBLOCKS-2FDOC-2FDEPENDENCIES-3A-3A-40DEPENDENCIES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29

#+NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget cl-prebid-container) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list widget theme))
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
   (call-next-method)))

;;;;;; Application

#-NIL
(defun wrap-with-frame (widget)
  ;; (clouseau:inspect (list 1 widget))
  (make-instance 'cl-prebid-container
		 :content widget))

;;;;
;;;;
;;;;


(reblocks/app:defapp my-app
    :prefix "/my-app"
    :subclasses (reblocks-ui2-demo/app::app)
    :routes ((reblocks/routes:page ("/foo" :name "foo") (reblocks-ui2-demo/pages/form::make-form-page)))
    )

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

  (setf reblocks-ui2/themes/api::*current-theme*
        (reblocks-ui2/themes/tailwind:make-tailwind-theme))

  #+NIL
  (reblocks/server:start :port port
			 :interface interface
                         :apps #+NIL 'reblocks-ui2-demo/app::app #-NIL 'my-app
                         :server-type server-type
                         :request-timeout request-timeout
                         :debug debug)

  #-NIL
  (apply 'reblocks-ui2-demo/server::start args)
  )
