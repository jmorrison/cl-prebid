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
;;;; cl-prebid widgets
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget prebid-banner-widget (ui-widget)
  ((div-id :accessor div-id :initform (symbol-name (gensym "prebid-banner")))))

(defmethod render ((widget prebid-banner-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list :reblocks-ui2/widget-render cpc))
  (with-html ()
    (:script (:raw "alert('HI');"))
    (:script (:raw (ps:ps*
		    `(let ()
		      (setf pbjs (or pbjs (ps:create)))
		      (setf (ps:chain pbjs que) (or (ps:chain pbjs que) (array)))
		      (defparameter ad-unit-code "adUnitCode-0000")

		      (defparameter
			  ad-units
			(array
			 (ps:create
			  media-types (ps:create banner (ps:create sizes (array 600 500)))
			  code ad-unit-code
			  bids (array (ps:create bidder "testBidder" params (ps:create))))))
		      (defun foo (bid)
			((ps:chain console log) "foo: " bid) bid)
		      (ps:chain pbjs que (push (lambda ()
						 (ps:chain pbjs (register-bid-adapter
								 nil
								 "testBidder"
								 (ps:create
								  supported-media-types (array "banner" "video" "native")
								  is-bid-request-valid (lambda (bid) t))))
						 (ps:chain pbjs (set-config (ps:create
									     debugging (ps:create enabled t
												  intercept (array (ps:create
														    when (ps:create bidder "testBidder")
														    then (ps:create creative-id "testCreativeId")))))))
						 ;; Bid Response Simulation Section End
						 (ps:chain pbjs (add-ad-units ad-units))
						 (ps:chain pbjs (request-bids (ps:create
									       add-unit-codes (array ad-unit-code)
									       bids-back-handler (lambda ()
												   (let* ((bids (ps:chain pbjs (get-highest-cpm-bids ad-unit-code)))
													  (winning-bid (aref bids 0))
													  (div (ps:chain document (get-element-by-id #+NIL "foobar" #-NIL ,(div-id widget))))
													  (iframe (ps:chain document (create-element "iframe"))))
												     (ps:chain div (append-child iframe))
												     (let ((iframe-doc (ps:chain iframe content-window document)))
												       (ps:chain pbjs (render-ad iframe-doc (ps:chain winning-bid ad-id))))))))))))))))
    (:div :id #+NIL "foobar" #-NIL (div-id widget))))

(defmethod reblocks-ui2/widget:get-dependencies ((widget prebid-banner-widget) (theme reblocks-ui2/themes/tailwind:tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies widget theme))
  (list
   (reblocks/dependencies:make-dependency
    #P"./Prebid.js/dist/not-for-prod/prebid.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   #+NIL
   (reblocks/dependencies:make-dependency
    #P"./pub/js/prebid-banner.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   )
  )

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
;;;; Navigation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+NIL
(reblocks-navigation-widget:defroutes top-level-navigation
    ("/" (make-instance 'a-string-widget :content "ROOT"))
  ("/one" (make-instance 'a-string-widget :content "ONE"))
  ("/two" (make-instance 'a-string-widget :content "TWO")))

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

(defmacro pub-subdir-route (subdir name mimetype)
  "Make a route for a pub subdirectory"
  `(40ants-routes/defroutes:get (,(format nil "/pub/~a/<string:fname>" subdir) :name ,name)
				(progn
				  (format *terminal-io* "============================= ./pub/~a/~a~%" ,subdir fname)
				  (list 200
					(list :content-type ,mimetype)
					(asdf:system-relative-pathname
					 :cl-prebid/reblocks
					 (format nil "./pub/~a/~a" ,subdir fname))))))
(defapp my-app
    :prefix "/"
    ;; :page-constructor #'wrap-with-frame
    :routes (
	     (reblocks/routes:static-file
	      "/favicon.ico"
	      (asdf:system-relative-pathname :cl-prebid "pub/images/favicon.ico"))

	     (pub-subdir-route "css"     "css"     "text/css")
	     (pub-subdir-route "fonts"   "fonts"  "font/otf")
	     (pub-subdir-route "images"  "images" "image/png")
	     (pub-subdir-route "js"      "js"     "application/javascript")

	     (reblocks-file-server/core:file-server 
	      "/files/"
	      :name "files"
	      :root (asdf:system-relative-pathname
		     :cl-prebid
		     (make-pathname :directory '(:relative "pub"))))

	     #+NIL (page ("/nav" :name "nav") (make-top-level-navigation))

	     (page ("/" :name "root") (make-instance 'a-string-widget :content "ROOT"))
	     (page ("/one" :name "foo") (make-instance 'a-string-widget :content "ONE"))
	     (page ("/two" :name "two") (make-instance 'a-string-widget :content "TWO"))
	     #-NIL (page ("/three" :name "three") (make-instance 'cl-prebid-container :content (make-instance 'a-string-widget :content "THREE")))
	     #+NIL (page ("/three" :name "three") (make-instance 'a-string-widget :content "THREE"))

	     (page ("/banner" :name "banner") (make-instance 'prebid-banner-widget))
#|
	     (40ants-routes/defroutes:get ("/pub3/<int:id>" :name "article")
					  (progn
					    (clouseau:inspect (list :id id reblocks/session::*env*))
					    (format t "Handler for article with ID ~D was called." id)))
|#
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
