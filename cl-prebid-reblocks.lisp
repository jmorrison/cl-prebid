;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;


(in-package #:cl-prebid/reblocks)

(shadowing-import 'reblocks-ui2-demo/server::*default-interface*)
(shadowing-import 'reblocks-ui2-demo/server::*default-port*)
(shadowing-import 'reblocks-ui2-demo/server::*default-request-timeout*)
(shadowing-import 'reblocks-ui2/buttons/button:button)
(shadowing-import 'reblocks-ui2/containers/column:column)
(shadowing-import 'reblocks-ui2/containers/column:column-widget)
(shadowing-import 'reblocks-ui2/containers/container:subwidgets)
(shadowing-import 'reblocks-ui2/containers/row:row)
(shadowing-import 'reblocks-ui2/form/validators:valid-integer)
(shadowing-import 'reblocks-ui2/form/validators:valid-password)
(shadowing-import 'reblocks-ui2/form/validators:valid-string)
(shadowing-import 'reblocks-ui2/form:form)
(shadowing-import 'reblocks-ui2/inputs/text-input:input)
(shadowing-import 'reblocks-ui2/themes/styling:join-css-classes)
(shadowing-import 'reblocks-ui2/themes/tailwind:tailwind-theme)
(shadowing-import 'reblocks-ui2/widget:get-dependencies)
(shadowing-import 'reblocks-ui2/widget:render)
(shadowing-import 'reblocks-ui2/widget:ui-widget)
(shadowing-import 'reblocks/dependencies:make-dependency)
(shadowing-import 'reblocks/variables:*current-app*)
(shadowing-import 'reblocks/variables:*max-pages-per-session*)
(shadowing-import 'reblocks/variables:*pages-expire-in*)
(shadowing-import 'reblocks/widget:update)
(shadowing-import 'reblocks/widgets/string-widget:make-string-widget)
;; (shadowing-import 'reblocks-ui2/containers/controls-row:controls-row)
;; (shadowing-import 'reblocks-ui2/themes/tailwind:make-tailwind-theme)
;; (shadowing-import 'reblocks/server:start)

(shadowing-import 'reblocks-ui2-demo/pages/buttons::make-buttons-page)
(shadowing-import 'reblocks-ui2-demo/pages/cards::make-cards-page)
(shadowing-import 'reblocks-ui2-demo/pages/containers::make-containers-page)
(shadowing-import 'reblocks-ui2-demo/pages/form::make-form-page)
(shadowing-import 'reblocks-ui2-demo/pages/tabs::make-tabs-page)
(shadowing-import 'reblocks-ui2-demo/pages/text-input::make-text-input-page)


(trace REBLOCKS-FILE-SERVER/CORE::FILE-SERVER-HANDLER :break nil)
;; (trace (method reblocks-ui2/widget:render (reblocks-file-server/core::file-widget tailwind-theme)) :break nil)

;;
;; Kludge to prevent dependency on CDN-delivered tailwind.
;;
;; The reblocks ui2 widget primary get-dependencies method fetches the
;; CDN version of tailwind.  So, over-ride it here.  Probably would be
;; more responsible to make an :around method instead, and prune the
;; CDN dependency from those returned by call-next-method
;;

(defmethod get-dependencies ((widget ui-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies :ui-widget widget theme))
  (list*
   (make-dependency
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
;;;; Test widgets
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; A simple string widget styled with tailwind
;;

(defwidget string-tailwind-test-widget (ui-widget)
  ((content :accessor content :initarg :content :initform "")))

(defmethod render ((widget string-tailwind-test-widget) (theme tailwind-theme))
  (reblocks-request-params (foo baz frob)
		     (with-html ()
		       (:div :class "text-2xl my-8" ; Reference tailwind style(s)
			     (:p (content widget))
			     (:p (format nil "param foo ~s" foo))
			     (:p (format nil "param baz ~s" baz))
			     (:p (format nil "param frob ~s" frob))))))

(defmethod get-dependencies ((widget string-tailwind-test-widget) (theme tailwind-theme))
  (list*
   (make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))

;;
;; A lass (inline) styling test widget
;;

(defwidget reblocks-lass-test-widget (column-widget)
  ((subwidgets
    :initform (list
	       (make-string-widget "foo")
	       (make-string-widget "bar")))))

(defmethod render ((widget reblocks-lass-test-widget) (theme tailwind-theme))
  (with-html ()
    (:div :id "banner" :class "banner" (:a "banner text"))
    (:div :id "bodystuff"
	  :class "bodystuff"
	  (:a "bodystuff text")
	  (loop for item in (subwidgets widget) do
		#+NIL (clouseau:inspect item)
		(render item theme)))))

(defmethod get-dependencies ((widget reblocks-lass-test-widget) (theme tailwind-theme))
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
   (make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))


;;
;; A container test widget
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A test widget
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget tailwind-test-widget (ui-widget)
  ())

(defmethod render ((widget tailwind-test-widget) (theme tailwind-theme))
  (with-html ()
    (:div
     :id "tailwind-test-widget"

     ;; These are styled with our locally-built, enormously
     ;; over-inclusive tailwind css file

     (:div :id "tailwind00" :class "mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 md:text-5xl lg:text-6xl dark:text-white"
	   (:a "tailwind00 text"))
     (:div :id "header" (:h1 :class "text-2xl my-8" (:a "text-2xl my-8")))
     (:div :id "h1" "FOO" (:a "h1 text-2xl my-8"))
     (:div :id "foo" :class "text-8xl text-gray-500 dar:text-gray-400" (:a "text-8xl text-gray-500 dar:text-gray-400"))
     (:div :id "Font test 1" :class "font-custom text-2xl" (:a "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
     (:div :id "Font test 2" :class "text-2xl" (:a "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
     (:div :id "Font test 3" :class "font-custom text-2xl" (:a "abcdefghijklmnopqrstuvwxyz"))
     (:div :id "Font test 4" :class "text-2xl" (:a "abcdefghijklmnopqrstuvwxyz"))
     (:div :id "Font test 6" :class "font-custom text-2xl" (:a "The quick brown fox jumps over the lazy dog"))
     (:div :id "Font test 6" :class "text-2xl" (:a "The quick brown fox jumps over the lazy dog"))
     )
    #+NIL (call-next-method)))

(defmethod get-dependencies ((widget tailwind-test-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies widget theme))
  (list*
   (make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method)))

;;
;; (reblocks/preview:preview (make-instance 'todo::cl-prebid-container))
;;

(defwidget cl-prebid-container (column-widget)
  ((subwidgets
    :initform (list
	       (make-string-widget "foo")
	       (make-string-widget "bar")))
   (content
    :initarg :content
    :accessor content
    :initform nil)))

(defmethod render ((widget cl-prebid-container) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :widget-render widget))
  (with-html ()
    (:div
     :id "foobar"

     ;; Here we render the contents of this container

     (:div :content (render (content widget) theme))
     )
    )
  )

;; https://40ants.com/reblocks/dependencies/#x-28REBLOCKS-2FDOC-2FDEPENDENCIES-3A-3A-40DEPENDENCIES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29

#-NIL
(defmethod get-dependencies ((widget cl-prebid-container) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies widget theme))
  (list*
   (make-dependency
    #P"./my-tailwind/output.css"
    :system :cl-prebid
    :type :css
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   (call-next-method))) ; Otherwise a remote CDN dependency upon tailwind

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; cl-prebid widgets
;;;;
;;;; We commented-out the static JS file, and use Parenscript to
;;;; compile dynamically.
;;;;
;;;; Might need a custom renderer to eliminate repeat-x
;;;;
;;;; https://docs.prebid.org/dev-docs/renderer.html
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; The banner widget
;;

(defwidget prebid-banner-widget (ui-widget)
  ((div-id
    :accessor div-id
    :initform (symbol-name (gensym "prebid-banner")))))

(defmethod render ((widget prebid-banner-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :widget-render widget))
  (with-html ()
    (:script
     (:raw
      (ps:ps*
       `(let ()
	  (setf pbjs (or pbjs (ps:create)))
	  (setf (ps:chain pbjs que) (or (ps:chain pbjs que) (array)))
	  (defparameter ad-unit-code "adUnitCode-0000")

	  (defparameter
	      ad-units
	    (array
	     (ps:create
	      media-types (ps:create banner (ps:create
					     sizes (array
						    (array 600 500) ; The demo image
						    (array 728 90)
						    (array 200 250)
						    (array 336 280)
						    (array 320 50)
						    (array 160 600)
						    (array 300 600)
						    (array 120 600)
						    (array 970 250))))
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
											      (div (ps:chain document (get-element-by-id ,(div-id widget))))
											      (iframe (ps:chain document (create-element "iframe"
																	 (ps:create
																	  scrolling "no"
																	  |background-repeat| "no-repeat"
																	  ;; width "100%"
																	  ;; height "100%"
																	  )))))
											 (ps:chain iframe (set-attribute "scrolling" "no")) ; WORKS
											 (ps:chain iframe (set-attribute "background-repeat" "no-repeat"))
											 ((ps:@ console log) "iframe: " iframe)
											 (ps:chain div (append-child iframe))
											 (let ((iframe-doc (ps:chain iframe content-window document)))
											   (ps:chain pbjs (render-ad iframe-doc (ps:chain winning-bid ad-id)))
											   ((ps:@ console log) "content-document: " (ps:chain iframe content-document)))))))))))))))
    (:div
     :id (div-id widget)
     :class
     #+NIL "flex w-full justify-center overflow-hidden"
     #+NIL "w-400 h-300 justify-center overflow-hidden"
     #-NIL "flex justify-center overflow-hidden"
     )
    )
  )


(defmethod get-dependencies ((widget prebid-banner-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies :prebid-banner-widget widget theme))
  (list
   (make-dependency
    #P"./Prebid.js/dist/not-for-prod/prebid.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   #+NIL
   (make-dependency
    #P"./pub/js/prebid-banner.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   #-NIL
   (reblocks-lass:make-dependency
    '(.body
      :scrollbar-width "none"
      :background-repeat "no-repeat"
      :background-size "cover"
      :background-size "cover"
      )
    )
   )
  )

;;
;; Native test
;;

(defwidget prebid-native-widget (ui-widget)
  ((div-id :accessor div-id :initform (symbol-name (gensym "prebid-native")))))

(defmethod render ((widget prebid-native-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :widget-render :prebid-native-widget widget))
  (with-html ()
    #-NIL (:script (:raw "alert('prebid-native-widget');"))
    #-NIL
    (:script
     (:raw
      (ps:ps*
       `(progn
	  (defvar sizes '(300 200))
	  (defvar *prebid_timeout* 700)
	  (defvar ad-units (array (ps:create
				   code "/19968336/header-bid-tag-1"
				   media-types (ps:create banner (ps:create sizes sizes))
				   bids (array (ps:create bidder "testBidder"
							  params
							  #-NIL (ps:create)
							  #+NIL (ps:create placement-id "XXXXXXX"))))))
	  (defvar googletag (or googletag (ps:create)))
	  (setf (ps:chain googletag cmd) (or (ps:chain googletag cmd) (array)))
	  (ps:chain googletag cmd (push (lambda () (ps:chain googletag (pubads) (disable-initial-load)))))
	  (defvar pbjs (or pbjs (ps:create)))
	  (setf (ps:chain pbjs que) (or (ps:chain pbjs que) (array)))
	  (ps:chain pbjs que (push (lambda ()
				     (ps:chain pbjs (add-ad-units ad-units))
				     (ps:chain pbjs (request-bids (ps:create bids-back-handler init-adserver))))))
	  (defun init-adserver ()
	    (cond ((ps:chain pbjs init-adserver-set) nil)
		  (t
		   (setf (ps:chain pbjs init-adserver-set) t)
		   (ps:chain googletag cmd (push (lambda ()
						   (cond ((ps:chain pbjs lib-loaded)
							  (ps:chain pbjs que (push (lambda ()
										     (ps:chain pbjs (set-targeting-for-g-p-t-async))
										     (ps:chain googletag (pubads) (refresh))))))
							 (t
							  (ps:chain googletag (pubads) (refresh))))))))))
	  (set-timeout (lambda () (init-adserver)) *prebid_timeout*)
	  (ps:chain googletag cmd (push (lambda ()
					  (ps:chain googletag (define-slot "/19968336/header-bid-tag-1" sizes ,(div-id widget)) (add-service (ps:chain googletag (pubads))))
					  (ps:chain googletag (pubads) (enable-single-request))
					  (ps:chain googletag (enable-services)))))))))
    (:div
     :id (div-id widget)
     :style "min-height:250px")
    (:script
     (:raw
      (ps:ps*
       `(ps:chain googletag cmd (push (lambda ()
					(ps:chain googletag (display ,(div-id widget)))))))))))

(defmethod get-dependencies ((widget prebid-native-widget) (theme tailwind-theme))
  #+NIL (clouseau:inspect (list :get-dependencies :prebid-banner-widget widget theme))
  (list
   (make-dependency
    #P"./Prebid.js/dist/not-for-prod/prebid.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   #+NIL
   (make-dependency
    #P"./pub/js/prebid-banner.js"
    :system :cl-prebid
    :type :js
    :crossorigin "anonymous"
    ;; :cache-in-memory t
    )
   )
  )

;;
;; Video test
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Page wrapper
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget page-frame-widget (ui-widget)
  ((banner
    :accessor banner
    :initform (make-instance 'prebid-banner-widget))
   (contnt-widget
    :initarg :content
    :reader content)))

(defun wrap-with-frame (widget)
  #+NIL (clouseau:inspect (list 1 widget))
  (make-instance 'page-frame-widget
		 :content widget))


(defmethod render ((widget page-frame-widget) (theme t))
  (reblocks/html:with-html ()
    (:header
     (banner widget)
     (:div
      :id "symsim-logo"
      :class "flex justify-center"
      (:a :href "http://www.symbolic-simulation.com"
	  (:img :src "/pub/images/SymbolicSimulationLLC.png" :alt "full-logo.png")))

     (:div :class "navbar"
	   (:div :class "main-logo"
		 (:div :class "title text-4xl my-8 text-center text-stone-800 dark:text-stone-300"
                       (:a :href "/"
                           "Reblocks UI2 Demo App")))))

    (:div :class "flex"
          (let* ((menu-item-classes "text-xl py-2 px-4 shadow-lg hover:shadow-md rounded-r-xl border border-stone-200")
                 (current-menu-item-classes (join-css-classes
					     theme
                                             menu-item-classes
                                             (:colors-bg-action theme)))
                 (sections (append
                            (sort (list
				   #|
				   '("button" "Button")
                                   '("form" "Form")
                                   '("text-input" "Text Input")
                                   '("card" "Card")
                                   '("containers" "Containers")
                                   '("tabs" "Tabs")

				   '("banner" "Banner")

				   |#

				   '("string-tailwind-test-widget" "String Tailwind Test Widget")
				   '("tailwind-test-widget" "Tailwind Test Widget")
				   '("reblocks-lass-test-widget" "Reblocks Lass Test Widget")
  				   '("cl-prebid-container" "CL Prebid Container")
  				   '("postgis-login-form-page" "PostGIS Login Form Page")

  				   ;; '("css" "CSS")
  				   ;; '("fonts" "Fonts")
  				   ;; '("images" "Images")
  				   ;; '("js" "JavaScript")

  				   )
                                  #'string<
                                  :key #'car)
                              (list
                               #+NIL '("sources" "Sources" :path "")))))
              (:ul :class "w-[200px] flex flex-col gap-4"
                   (loop for (page-name title . route-args) in sections
                         for full-path = (apply #'40ANTS-ROUTES/ROUTE-URL:route-url
                                                page-name
                                                ;; :namespace "server"
                                                route-args)
                         ;; Here we use starts-with, because for Sources
                         ;; section there might be different URLs behind the prefix.
                         for current = (str:starts-with-p full-path
                                                          (reblocks/request:get-path))
                         do (:li :class (if current
                                            current-menu-item-classes
                                            menu-item-classes)
                                 (:a :class "block text-right"
                                     :href full-path
                                     title)))))
            
            (:div :class "page-content w-1/2 mx-auto"
                  (content widget)))

      (:div :class "footer w-1/2 mx-auto my-4 text-slate-400"
            (:p "Have a question?")
            (:p "File an issue: "
                (:a :class "text-blue-400"
                    :href "https://github.com/40ants/reblocks-ui2"
                    "https://github.com/40ants/reblocks-ui2")))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;
  ;;;; Pages
  ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; PostGIS dialog/form
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defwidget postgis-login-form-page ()
  ((submittedp :initform nil
               :type boolean
               :accessor submittedp)
   (pghost :accessor pghost)
   (pgport :accessor pgport)
   (dbname :accessor dbname)
   (pguser :accessor pguser)
   (pgpassword :accessor pgpassword)))



(defmethod connect-to-db ((widget postgis-login-form-page))
  (let ((db (make-instance 'com.symsim.oss.postgis::db-cache
			   :host     #+NIL "localhost" #-NIL (coerce (pghost widget) 'simple-string)
			   :user     #+NIL "siminabox" #-NIL (pguser widget)
			   :password #+NIL "siminabox" #-NIL (pgpassword widget)
			   :port     #+NIL 5432        #-NIL (pgport widget)
			   :db-name  #+NIL "siminabox" #-NIL (dbname widget))))
    ;; (clouseau:inspect (list :submittedp widget))
    #+NIL (clouseau:inspect (list :db 1 db))
    (let ((retval nil))
      (setf retval (handler-case (com.symsim.oss.postgis::connect-to-postgis db)
		     (cl-postgres:database-socket-error (c)
		       (clouseau:inspect (list :db 2 db))
		       (break (format nil "database-socket-error ~s" c))
		       (sb-ext:quit)
		       (return-from connect-to-db nil))
		     (cl-postgres:database-error (c)
		       (clouseau:inspect (list :db 3 db))
		       (break (format nil "database-error ~s" c))
		       (sb-ext:quit)
		       (return-from connect-to-db nil))
		     (simple-error (c)
		       (clouseau:inspect (list :db 4 db))
		       (break (format nil "simple-error ~s" c))
		       (sb-ext:quit)
		       (return-from connect-to-db nil))
		     (condition (c)
		       (clouseau:inspect (list :db 5 db))
		       (break (format nil "simple-error ~s" c))
		       (sb-ext:quit)
		       (return-from connect-to-db nil))))
      #+NIL (clouseau:inspect (list :db 7 retval))
      #+NIL (clouseau:inspect (list :db 8 (COM.SYMSIM.OSS.POSTGIS:TABLES-LIST db)))
      #+NIL (pprint (COM.SYMSIM.OSS.POSTGIS:TABLES-LIST db))
      #+NIL (break "after handler-case")
      #+NIL (sb-ext:quit)
      db)
    )
  )

;; (trace com.symsim.oss.postgis::connect-to-postgis)
;; (trace postmodern::connect-toplevel)
;; (trace connect-to-db)

(defun make-postgis-login-form-page ()
  (make-instance 'postgis-login-form-page))

(defmethod reblocks/widget:render ((widget postgis-login-form-page))
  (with-html ()
    (:h2 :class "text-xl my-4"
         "PostGIS Connection Parameters")
    (cond
      ((submittedp widget)
       (reblocks/widget:render
        (form
         (row (make-string-widget
               "Thank you for submission!!!!!!!!")
              #+NIL (button "Try again" :view :action))
         :on-submit
         (lambda (form)
           (declare (ignore form))
           (log:warn "Form reset was called")
	   #-NIL (clouseau:inspect (list :on-submit form))
           (setf (submittedp widget)
                 nil)
           (update widget)))))
      (t
       (reblocks/widget:render
        (form
         (column
          (row
	   (input :name "pghost"
		  :placeholder "pghost"
		  :validator (valid-string)
		  :size :xl)
           (input :name "pgport"
                  :placeholder "5432"
                  :validator (valid-integer :min 18)
                  :size :xl))
	  (row
	   (input :name "pguser"
		  :placeholder "pguser"
		  :validator (valid-string)
		  :size :xl)
	   (input :name "dbname"
                  :placeholder "dbname"
                  :validator (valid-string)
                  :size :xl))
          (row
           (input :name "pgpassword"
                  :type :password
                  :validator
		  (valid-password :min-length 1)
                  :size :xl
                  :placeholder "siminabox"))
          (reblocks-ui2/containers/controls-row:controls-row
           (button "Open Connection to PostGIS" :view :action)))
         :on-submit
         (lambda (form &key pghost pguser dbname #+NIL email pgport pgpassword)
           (declare (ignore form))
           (log:warn "On Submit was called:" pghost pguser dbname #+NIL email pgport pgpassword)
	   (setf (pghost widget) pghost)
	   (setf (pgport widget) pgport)
	   (setf (dbname widget) dbname)
	   (setf (pguser widget) pguser)
	   (setf (pgpassword widget) pgpassword)
	   (connect-to-db widget)
           (setf (submittedp widget) t)
	   (update widget))))))))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;
  ;;;; Application
  ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      :page-constructor #'wrap-with-frame
      :debug t
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
  		     #+NIL (make-pathname :directory '(:relative "pub"))
  		     (cl-fad:merge-pathnames-as-directory  (user-homedir-pathname) (make-pathname :directory '(:relative
  													       "dev"
  													       "workflow"
  													       "images"
  													       "podman-archives"
  													       "databases")))
  		     )
  	      )

  	     (page ("/form" :name "form") (make-form-page))
             (page ("/card" :name "card") (make-cards-page))
             (page ("/text-input" :name "text-input") (make-text-input-page))
             (page ("/containers" :name "containers") (make-containers-page))
             (page ("/button" :name "button") (make-buttons-page))
             (page ("/tabs" :name "tabs") (make-tabs-page))
             

  	     #+NIL (page ("/nav" :name "nav") (make-top-level-navigation))

  	     (page ("/" :name "root") (make-instance 'string-tailwind-test-widget :content "ROOT"))
  	     (page ("/string-tailwind-test-widget" :name "string-tailwind-test-widget") (make-instance 'string-tailwind-test-widget :content "ONE"))
  	     (page ("/tailwind-test-widget" :name "tailwind-test-widget") (make-instance 'tailwind-test-widget))
  	     (page ("/reblocks-lass-test-widget" :name "reblocks-lass-test-widget") (make-instance 'reblocks-lass-test-widget))
	     (page ("/cl-prebid-container" :name "cl-prebid-container") (make-instance
									 'cl-prebid-container
									 :content (make-instance 'string-tailwind-test-widget :content "THREE")))

  	     (page ("/banner" :name "banner") (make-instance 'prebid-banner-widget))
  	     (page ("/native" :name "native") (make-instance 'prebid-native-widget))
	     (page ("/postgis-login-form-page" :name "postgis-login-form-page") (make-instance 'postgis-login-form-page))
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
   (make-dependency
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
			   *default-port*)))
		(server-type :hunchentoot)
		(interface *default-interface*)
		(request-timeout *default-request-timeout*)
		(debug t))

  (setf *pages-expire-in* (* 10 60))
  (setf *max-pages-per-session* 10)

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
  (apply 'reblocks/server:start args)
  )
