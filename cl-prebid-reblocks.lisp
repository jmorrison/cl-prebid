;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;


(in-package #:cl-prebid/reblocks)

(trace REBLOCKS-FILE-SERVER/CORE::FILE-SERVER-HANDLER :break nil)
(trace (method reblocks-ui2/widget:render (reblocks-file-server/core::file-widget reblocks-ui2/themes/tailwind::tailwind-theme)) :break nil)

;;
;; Kludge to prevent dependency on CDN-delivered tailwind.
;;
;; The reblocks ui2 widget primary get-dependencies method fetches the
;; CDN version of tailwind.  So, over-ride it here.  Probably would be
;; more responsible to make an :around method instead, and prune the
;; CDN dependency from those returned by call-next-method
;;

(defmethod reblocks-ui2/widget:get-dependencies ((widget reblocks-ui2/widget:ui-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies :reblocks-ui2/ui-widget widget theme))
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
;; This macro binds the STRING values of the various HTML parameters (in lowercase).  It is useful 
;;
;;  Use this to bind string values of request params foo, baz, and frob, respectively
;;
;; (reblocks-request-params (foo baz frob)
;;    <body referencing foo, baz, and frob as bound vars> )
;; 

(defmacro reblocks-request-params (lambda-list &body body)
  (let ((temp-var (gensym)))
    `(let ((,temp-var (reblocks/request:get-parameters)))
       #+NIL (clouseau:inspect ,temp-var)
       (let ,(loop for var in lambda-list
		   for i from 0
		   collect `(,var (aget ,temp-var ,(string-downcase (symbol-name var)))))
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;; A string widget, styled with tailwind
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget a-string-widget (ui-widget)
  ((content :accessor content :initarg :content :initform "")))

(defmethod render ((widget a-string-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  (reblocks-request-params (foo baz frob)
		     (with-html ()
		       (:div :class "text-2xl my-8" ; Reference tailwind style(s)
			     (:p (content widget))
			     (:p (format nil "foo ~s" foo))
			     (:p (format nil "baz ~s" baz))
			     (:p (format nil "looking up frob ~s" frob))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A prebid container widget (page wrap, really)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

    (:div :id "foobar" (:img :src "/pub/image/full-logo.png" :alt "full-logo.png"))

    ;; These are styled with our locally-built, enormously
    ;; over-inclusive tailwind css file

    (:div :id "tailwind00" :class "mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 md:text-5xl lg:text-6xl dark:text-white"
	  (:a "tailwind00 text"))
    (:div :id "header" (:h1 :class "text-2xl my-8" (:a "header text")))
    (:div :id "foo" :class "text-8xl text-gray-500 dar:text-gray-400" (:a "FOOO"))

    ;; These are styled using the reblocks-lass defined in the following get-dependencies method

    (:div :id "banner" :class "banner" (:a "banner text"))
    (:div :id "bodystuff"
	  :class "bodystuff"
	  (:a "bodystuff text")
	  (loop for item in (reblocks-ui2/containers/container:subwidgets cpc) do
		#+NIL (clouseau:inspect item)
		(reblocks-ui2/widget:render item theme)))

    ;; Here we render the contents of this container

    (:div :content (reblocks-ui2/widget:render (content cpc) theme))
    )
  )

;; https://40ants.com/reblocks/dependencies/#x-28REBLOCKS-2FDOC-2FDEPENDENCIES-3A-3A-40DEPENDENCIES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29

#-NIL
(defmethod reblocks-ui2/widget:get-dependencies ((widget cl-prebid-container) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies widget theme))
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
   #+NIL
   (reblocks/dependencies:make-dependency
    #P"./images/full-logo.png"
    :system :cl-prebid
    :type :png
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method))) ; Otherwise a remote CDN dependency upon tailwind

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Application
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-NIL
(defun wrap-with-frame (widget)
  #+NIL (clouseau:inspect (list 1 widget))
  (make-instance 'cl-prebid-container
		 :content widget))

;;;;
;;;; Pages
;;;;

;;;;
;;;; App
;;;;

(defapp my-app
    :prefix "/"
    ;; :page-constructor #'wrap-with-frame
    :routes ((page ("/" :name "root") (make-instance 'a-string-widget :content "ROOT"))
	     (page ("/one" :name "foo") (make-instance 'a-string-widget :content "ONE"))
	     (page ("/two" :name "two") (make-instance 'a-string-widget :content "TWO"))
	     (page ("/three" :name "three") (make-instance 'cl-prebid-container :content (make-instance 'a-string-widget :content "THREE")))
	     (reblocks/routes:static-file
	      "/favicon.ico"
	      (asdf:system-relative-pathname :cl-prebid "pub/image/favicon.ico"))
	     (40ants-routes/defroutes:get ("/pub3/<int:id>" :name "article")
					  (progn
					    (clouseau:inspect (list :id id reblocks/session::*env*))
					    (format t "Handler for article with ID ~D was called." id)))
	     (40ants-routes/defroutes:get ("/pub4/<string:fname>" :name "article")
					  (progn
					    (clouseau:inspect (list :fname fname reblocks/session::*env*))
					    (format t "Handler for article with FNAME ~S was called." fname)
					    (list 200
						  (list :content-type "image/png")
						  (asdf:system-relative-pathname :cl-prebid/reblocks "./pub/image/full-logo.png"))))
	     (40ants-routes/defroutes:get ("/image/<int:id>" :name "article")
					  (progn
					    (clouseau:inspect (list :id id reblocks/page::*current-page*))
					    (format t "Handler for article with ID ~D was called." id)))
	     (reblocks-file-server/core:file-server 
	      "/pub/"
	      :name "pub"
	      :root (asdf:system-relative-pathname
		     :cl-prebid
		     (make-pathname :directory '(:relative "pub"))))
	     #+NIL (make-instance
		    '40ants-routes/defroutes::route-class
		    :root (asdf:system-relative-pathname
			   :cl-prebid
			   (make-pathname :directory '(:relative "pub")))
		    :handler #'(lambda (&key path) (clouseau:inspect path) nil)
		    :dir-listing nil)
	     #+NIL (reblocks-file-server:make-route :uri "/static/" :root "./pub/images/" :filter "*.png")
	     )
    )

#+NIL
(defmethod reblocks/dependencies:get-dependencies ((app my-app))
  (clouseau:inspect app)
  (list*
   (reblocks/dependencies:make-dependency
     "https://cdn.tailwindcss.com/3.3.6"
     ;; Old URLs:
     ;; "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"
     ;; "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio,line-clamp"
     :type :js)
   (call-next-method)))

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
