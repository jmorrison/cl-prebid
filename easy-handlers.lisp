;;; -*- Mode: Lisp; Package: cl-prebid -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2022.
;;;


(in-package :cl-prebid)

(eval-when (:compile-toplevel)
	   (setf cl-who::*downcase-tokens-p* NIL))

;;
;; These two should get moved to com.symsim.utils
;;

(defun parse-easy-handler-param (l)
  (let* ((lisp-symbol-name (first l))
	 (type-logical-expr (second l))
	 (type-inner-logical-expr (second type-logical-expr))
	 (param-triple (third type-inner-logical-expr))
	 (string-name (second param-triple))
	 (lisp-type (third param-triple)))
    ;; (clouseau:inspect (list l string-name lisp-type))
    (list string-name lisp-type)))

(defun parse-easy-handler-params (l)
  (cond ((null l) nil)
	(t
	 (let ((maybe-key (first l))
	       (paramlist (rest l)))
	   (mapcar #'parse-easy-handler-param paramlist)))))
			   
;;
;; Error condition pages
;;
;; Should probably also be moved to com.symsim.utils
;;

(defun bad-sidc-page (error-condition-var)
  (let ((msg (error-text error-condition-var)))
    (log:error msg)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "404: Not Found" *standard-output*)
       (:body
	(:h1 "404: Not Found")
	(:h2 "Bad SIDC Condition Message")
	(:p (str msg))
	(:h2 "Details of Request that Elicited the Error")
	(info-table
	 (headers-in       *request*)
	 (request-method   *request*)
	 (request-uri      *request*)
	 (server-protocol  *request*)
	 (local-addr       *request*)
	 (local-port       *request*)
	 (remote-addr      *request*)
	 (remote-port      *request*)
	 (hunchentoot::content-stream   *request*)
	 (cookies-in       *request*)
	 (get-parameters   *request*)
	 (post-parameters  *request*)
	 (script-name      *request*)
	 (query-string     *request*)))
       (setf (return-code*) +http-not-found+)
       nil))))

(defun simple-error-page (error-condition-var)
  (let ((msg #+NIL (apply
		    'format
		    nil
		    (simple-condition-format-control error-condition-var)
		    (simple-condition-format-arguments error-condition-var))
	     #-NIL (with-standard-io-syntax (format nil "~a" error-condition-var))))
    (log:error msg)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "404: Not Found" *standard-output*)
       (:body
	(:h1 "404: Not Found")
	(:h2 "Simple-Error Condition Message")
	(:p (str msg))
	(:h2 "Details of Request that Elicited the Error")
	(info-table
	 (headers-in       *request*)
	 (request-method   *request*)
	 (request-uri      *request*)
	 (server-protocol  *request*)
	 (local-addr       *request*)
	 (local-port       *request*)
	 (remote-addr      *request*)
	 (remote-port      *request*)
	 (hunchentoot::content-stream   *request*)
	 (cookies-in       *request*)
	 (get-parameters   *request*)
	 (post-parameters  *request*)
	 (script-name      *request*)
	 (query-string     *request*))))
      (setf (return-code*) +http-not-found+)
      nil)))
;;
;; The landing page for the CL-PREBID stuff.
;;

(define-easy-handler (cl-prebid :uri "/simbology" :acceptor-names '(normal-acceptor))
    ()
  "Top-level, human-readable web server page.  Has links to most interesting human-readable URLs"
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Symbolic Simulation, LLC's Joint Military Symbology Web Service/Server" *standard-output*)
       (:body
        (:p
         (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
        (:h1 "Joint Military Symbology Web Service/Server")

	;; Symbology test pages

        (:h2 (fmt "~a Demo Pages" (cl-cryptolens:product-name *simbology-product*)))
        (:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (:tr
		     (:td (:b "Specification"))
		     (:td (:b "SVG"))
		     (:td (:b "PNG")))
		    (:tr
		     (:td (:a "MIL-STD-2525C"))
		     (:td (:a :href (make-and-render-puri-uri "demo-2525c.svg") "Selected 2525C"))
		     (:td (:a :href (make-and-render-puri-uri "demo-2525c.png") "Selected 2525C")))
		    (:tr
		     (:td (:a "MIL-STD-2525D"))
		     (:td (:a :href (make-and-render-puri-uri "demo-2525d.svg") "Selected 2525D"))
		     (:td (:a :href (make-and-render-puri-uri "demo-2525d.png") "Selected 2525D")))
		    (:tr
		     (:td (:a "All Files (several thousand)"))
		     (:td (:a :href (make-and-render-puri-uri "demo-show-all-files.svg") "All Files"))
		     (:td (:a :href (make-and-render-puri-uri "demo-show-all-files.png") "All Files")))))
        (:p "Enter your own 2525C SIDC "
	    (:a :href (make-and-render-puri-uri "demo-enter-2525c") "here"))
        (:p "Enter your own 2525D SIDC "
	    (:a :href (make-and-render-puri-uri "demo-enter-2525d") "here"))

	;; Platform stuff that is useful when installing/debugging

	(:h2 "Platform Information Pages")
        (:p "See your License Status  "
            (:a :href (make-and-render-puri-uri "about-license-status") "here"))
        (:p "See the REST API (via introspection) "
            (:a :href (make-and-render-puri-uri "rest-api") "here"))
        (:p "See details about the Symbolic Simulation, LLC software (via introspection) "
            (:a :href (make-and-render-puri-uri "about-components") "here"))
        (:p "See details about the underlying software platform and server (via introspection) "
            (:a :href (make-and-render-puri-uri "about-platform") "here"))

        (:h2 "Credits")
        (:p "The open-source SVG/XML data is available at "
	    (:a :href (str "//github.com/Esri/joint-military-symbology-xml") "this GitHub page")
	    " courtesy of "
	    (:a :href (str "//esri.com") "ESRI"))
        (:p "This server is product of "
	    (:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC")))))))

(define-easy-handler (about-components :uri "/about-components" :acceptor-names '(normal-acceptor))
    ()
  "This page uses introspection to return lots of useful debugging information."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Symbolic Simulation, LLC's Joint Military Symbology Web Service/Server" *standard-output*)
       (:body
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "About the Symbolic Simulation, LLC Software")
	(info-table
	 *source-control-revision*
	 *default-pathname-defaults*
	 (uiop/os:getcwd)
	 *png-directory*
	 (cl-fad:merge-pathnames-as-directory (uiop/os:getcwd) #P"www/")
	 (asdf/system:system-relative-pathname :cl-prebid #P"www/")))))))

(define-easy-handler (about-platform :uri "/about-platform" :acceptor-names '(normal-acceptor))
  ()
  "This page uses introspection to return lots of useful debugging information."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Symbolic Simulation, LLC's Joint Military Symbology Web Service/Server" *standard-output*)
       (:body
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "About the Underlying Software Platform")
	(info-table
	 (local-time:now)
	 *hunchentoot-version*
	 ;; (ql-dist::dist-version "quicklisp")
	 *quicklisp-dist-version*
	 ;; These are from the CLHS
	 (lisp-implementation-type)
	 (lisp-implementation-version)
	 (room)
	 (software-type)
	 (software-version)
	 ;; Function of machine.
	 (ql-network::host-address (machine-instance))
	 (short-site-name)
	 (long-site-name)
	 (machine-instance)
	 (machine-type)
	 (machine-version)))))))

(define-easy-handler (rest-api :uri "/rest-api" :acceptor-names '(normal-acceptor))
  ()
  "This human-readable page describe the REST API, parameters for each entry, and docstring for each handler."
  (when (licensedp)
    (let ((sorted-easy-handlers (ht-sorted-easy-handlers-relative-urls)))
      (with-html-output-to-string (*standard-output* nil :prologue t)
	(:html
	 (head-of-page "REST API" *standard-output*)
	 (:body
	  (:p
	   (:a :href "//www.symbolic-simulation.com/"
	       (:img
		:src "customLogo.png"
		:alt "Symbolic Simulation, LLC")))
	  (:h1 "Joint Military Symbology Web Service/Server")
	  (:h2 "REST API")
          #+NIL (clouseau:inspect sorted-easy-handlers)
	  (dolist (handler-triple sorted-easy-handlers)
            #+NIL (clouseau:inspect handler-triple)
            (let* ((func-str (first handler-triple))
                   (func-uri (make-and-render-puri-uri  func-str))
                   (foo (rest handler-triple))
                   (acceptors (first foo))
                   (func-symbol (second foo))
                   (func (symbol-function func-symbol))
                   (unclear-what-this-is (third foo)))
              #+NIL (clouseau:inspect (list func-uri func-symbol func))
              (htm (:p (:a (:big (:b "URL: "))) (:big (:a :href func-uri (str func-str)))))
	      (htm (:p (:a (:b "Documentation: ")) (:a (str (documentation func 'function)))))
	      (htm (:ul
		    #|
		    (htm (:p (fmt "func-str ~a" func-str)))
		    (htm (:p (fmt "func-symbol ~a" func-symbol)))
		    (htm (:p (fmt "func ~s" func)))
		    (htm (:p (fmt "foo ~s" foo)))
		    (htm (:p (fmt "unclear-what-this-is ~s" unclear-what-this-is)))
		    (htm (:p (fmt "arglist ~a" (swank/backend:arglist func))))
		    |#
		    (dolist (param-type-pair (parse-easy-handler-params (swank/backend:arglist func)))
		      #|
		      (htm (:p (str param-type-pair)))
		      (htm (:p (str (format nil "1111> parameter ~S~%" param-type-pair))))
		      |#
		      #+sbcl
		      (htm (:li
			    (:p (:a (:b "Parameter: "))
				(:a (str (format nil "~a (type ~a)~%" (first param-type-pair) (second param-type-pair)))))))
		      #+ccl
		      (htm (:li (:p (:a (:b "Parameter: "))
				    (:a (str (format nil "Parameter ~a~%" (symbol-name l))))))))))
              ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SVG
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-easy-handler (demo-file.svg :uri "/demo-file.svg" :acceptor-names '(normal-acceptor))
    (name)
  "This shouldn't-be-necessary API returns an SVG file from the XML database."
  (when (licensedp)
    (setf (hunchentoot:content-type*) "image/svg+xml")
    (when (hunchentoot:header-in :origin *request*)
      (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
      (log:debug "ORIGIN - ~s~%" *request*)
      #+NIL (clouseau:inspect *request*))
    (handler-case (dom:map-document (cxml:make-string-sink) (cxml-doc-of-svg-file name))
      (error ()
	(log:error "demo-file.svg error for ~S" name)
	(setf (return-code*) +http-not-found+)
	nil))))


(define-easy-handler (2525c.svg
		      :uri "/2525c.svg"
		      :acceptor-names '(normal-acceptor)
		      :default-parameter-type 'string
		      :default-request-type :get)
    ((sidc :parameter-type 'string))
  "This returns an SVG file given a MIL-STD-2525C SIDC."
  (when (licensedp)
    (handler-case
	(let ((svg (lookup-2525c-svg (hunchentoot:get-parameters*))))
	  (setf (hunchentoot:content-type*) "image/svg+xml")
	  (when (hunchentoot:header-in :origin *request*)
	    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
	    (log:debug "ORIGIN - ~s~%" *request*)
	    #+NIL (clouseau:inspect *request*))
	  (cond (svg svg)
		(t (bad-sidc-svg (format nil "Missing graphics SIDC: ~a with params ~s" sidc (hunchentoot:get-parameters*))))))
      (bad-sidc (var)
	(log:error "2525c.svg bad-sidc error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (get-output-stream-string (bad-sidc-svg (error-text var)))
	#+SYMSIM-DEBUG (bad-sidc-page var))
      (error (c)
	(log:error "2525c.svg simple error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (get-output-stream-string (bad-sidc-svg (format nil "Error 2525c ~S" (hunchentoot:get-parameters*))))
	#+SYMSIM-DEBUG (simple-error-page c)))))

(define-easy-handler (2525d.svg
		      :uri "/2525d.svg"
		      :acceptor-names '(normal-acceptor)
		      :default-parameter-type 'string
		      :default-request-type :get)
    ((sidc :parameter-type 'string))
  "This returns an SVG file given a MIL-STD-2525D SIDC."
  (when (licensedp)
    (handler-case
	(let ((svg (lookup-2525d-svg (hunchentoot:get-parameters*))))
	  (setf (hunchentoot:content-type*) "image/svg+xml")
	  (when (hunchentoot:header-in :origin *request*)
	    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
	    (log:debug "ORIGIN - ~s~%" *request*)
	    #+NIL (clouseau:inspect *request*))
	  (cond (svg svg)
		(t (bad-sidc-svg (format nil "Missing graphics SIDC: ~a with params ~s" sidc (hunchentoot:get-parameters*))))))
      (bad-sidc (var)
	(log:error "2525d.svg bad-sidc error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (get-output-stream-string (bad-sidc-svg (error-text var)))
	#+SYMSIM-DEBUG (bad-sidc-page var))
      (error (c)
	(log:error "2525d.svg simple error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (get-output-stream-string (bad-sidc-svg (format nil "Error 2525D ~S" (hunchentoot:get-parameters*))))
	#+SYMSIM-DEBUG (simple-error-page c)))))

;;
;; Shows a table of all the SVG files in the (svg-hash-table *ms2525d*)
;;

(define-easy-handler (demo-show-all-files.svg :uri "/demo-show-all-files.svg" :acceptor-names '(normal-acceptor))
  ()
  "This returns several thousand SVG files.  Guaranteed to make your browser unhappy."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Show All SVG Files" *standard-output*)
       (:body
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Showing All SVG Files")
	(:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (maphash
		     #'(lambda (k v)
			 ;; (let ((query (format nil "/demo-file.svg?name=~A" k)))
			 (let ((query (make-and-render-puri-uri "demo-file.svg" (format nil "name=~a" k))))
			   (htm (:tr
				 (:td (:a :href (str query) (esc k)))
				 (:td (:object :data query :type "image/svg+xml"))))))
		     (svg-hash-table *ms2525d*))))
	(:p "This server is product of "
	    (:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC")))))))

(define-easy-handler (demo-2525c.svg :uri "/demo-2525c.svg" :acceptor-names '(normal-acceptor))
    ()
  "This returns the SVG files of some canned, hard-coded SVG MIL-STD-2525C SIDCs."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html 
       (head-of-page "Demo 2525C SVG" *standard-output*)
       (:body 
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Selected 2525C SIDCs as SVG")
	(:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (dolist (sidc *ms2525c-sidc-list*)
		      (let ((query (make-and-render-puri-uri "2525c.svg" (format nil "sidc=~a" sidc))))
		      (htm
		       (:tr
			(:td (esc query))
			(:td
			 #+NIL (:object
				:style "height:256px ; width:256px"
				:type "image/svg+xml"
				:data (esc query))
			 #-NIL (:img
				:src query
				:alt query
				:height "256px"
				:width "256px"))))))))
	(:p "This server is product of "
	    (:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC")))))))

(define-easy-handler (demo-2525d.svg :uri "/demo-2525d.svg" :acceptor-names '(normal-acceptor))
  ()
  "This returns the SVG files of some canned, hard-coded SVG MIL-STD-2525D SIDCs."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html 
       (head-of-page "Demo 2525D SVG" *standard-output*)
       (:body 
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Selected 2525D SIDCs as SVG")
	(:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (dolist (sidc *ms2525d-sidc-list*)
		      (let ((query (make-and-render-puri-uri "2525d.svg" (format nil "sidc=~a" sidc))))
			(htm (:tr
			      (:td (esc query))
			      (:td
			       #+NIL (:object
				;; :style "height:100% ; width:100%"
				;; :style "height:auto ; width:auto"
				      :style "height:256px ; width:256px"
				      :type "image/svg+xml"
				      :data (esc query))
			       #-NIL (:img
				      :src query
				      :alt query
				      :height "256px"
				      :width "256px"))))))))
	(:p "This server is product of "
	    (:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; PNG
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-easy-handler (demo-file.png
		      :uri "/demo-file.png"
		      :acceptor-names '(normal-acceptor)
		      :default-request-type :get)
    ((name :parameter-type 'string))
  "This shouldn't-be-necessary API returns a PNG file from the XML database."
  (when (licensedp)
    (setf (hunchentoot:content-type*) "image/png")
    (when (hunchentoot:header-in :origin *request*)
      (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
      (log:debug "ORIGIN - ~s~%" *request*)
      #+NIL (clouseau:inspect *request*))
    (handler-case (serialize-png-image (imago-png-file *ms2525d* name))
      (error ()
	(log:error "demo-file.png error for ~S" name)
	(setf (return-code*) +http-not-found+)
	nil))))

(define-easy-handler (2525c.png
		      :uri "/2525c.png"
		      :acceptor-names '(normal-acceptor)
		      :default-request-type :get)
    ((sidc :parameter-type 'string)
     (maxsize :parameter-type 'integer))
  "This returns a PNG file given a MIL-STD-2525C SIDC."
  (when (licensedp)
    (handler-case
	(let ((original (lookup-2525c-png (hunchentoot:get-parameters*))))
	  (setf (hunchentoot:content-type*) "image/png")
	  (when (hunchentoot:header-in :origin *request*)
	    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
	    (log:debug "ORIGIN - ~s~%" *request*))
	  (cond (original
		 (serialize-png-image
		  (if maxsize
		      (scale-png-to-max-size original maxsize)
		      original)))
		(t
		 (bad-sidc-png (format nil "Missing graphics SIDC: ~a with params ~s" sidc (hunchentoot:get-parameters*))))))
      (bad-sidc (var)
	(log:error "2525c.png bad-sidc error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (progn
			 (setf (hunchentoot:content-type*) "image/png")
			 (when (hunchentoot:header-in :origin *request*)
			   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
			   (log:debug "ORIGIN - ~s~%" *request*))
			 (bad-sidc-png (error-text var)))
	#+SYMSIM-DEBUG (bad-sidc-page var))
      (error (c)
	(log:error "2525c.png simple error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (progn
			 (setf (hunchentoot:content-type*) "image/png")
			 (when (hunchentoot:header-in :origin *request*)
			   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
			   (log:debug "ORIGIN - ~s~%" *request*))
			 (bad-sidc-png (format nil "Error 2525C ~S" (hunchentoot:get-parameters*))))
	#+SYMSIM-DEBUG (simple-error-page c)))))

(define-easy-handler (2525d.png
		      :uri "/2525d.png"
		      :acceptor-names '(normal-acceptor)
		      :default-request-type :get)
    ((sidc :parameter-type 'string)
     (maxsize :parameter-type 'integer))
  "This returns a PNG file given a MIL-STD-2525D SIDC."
  (when (licensedp)
    (handler-case
	(let ((original (lookup-2525d-png (hunchentoot:get-parameters*))))
	  (setf (hunchentoot:content-type*) "image/png")
	  (when (hunchentoot:header-in :origin *request*)
	    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
	    (log:debug "ORIGIN - ~s~%" *request*))
	  (cond (original
		 (serialize-png-image
		  (if maxsize
		      (scale-png-to-max-size original maxsize)
		      original)))
		(t
		 (bad-sidc-png (format nil "Missing graphics SIDC: ~a with params ~s" sidc (hunchentoot:get-parameters*))))))
      (bad-sidc (var)
	(log:error "2525d.png bad-sidc error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (progn
			 (setf (hunchentoot:content-type*) "image/png")
			 (when (hunchentoot:header-in :origin *request*)
			   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
			   (log:debug "ORIGIN - ~s~%" *request*))
			 (bad-sidc-png (error-text var)))
	#+SYMSIM-DEBUG (bad-sidc-page var))
      (error (c)
	(log:error "2525d.png simple error for SIDC ~S with params ~S" sidc (hunchentoot:get-parameters*))
	#-SYMSIM-DEBUG (progn
			 (setf (hunchentoot:content-type*) "image/png")
			 (when (hunchentoot:header-in :origin *request*)
			   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*") ; $$ Siminabox instead of wildcard?
			   (log:debug "ORIGIN - ~s~%" *request*))
			 (bad-sidc-png (format nil "Error 2525D ~S" (hunchentoot:get-parameters*))))
	#+SYMSIM-DEBUG (simple-error-page c)))))

(define-easy-handler (demo-show-all-files.png :uri "/demo-show-all-files.png" :acceptor-names '(normal-acceptor))
    ()
  "This returns several thousand PNG files.  Guaranteed to make your browser unhappy."
  (when (licensedp)
    (hunchentoot::with-debugger
	(with-html-output-to-string (*standard-output* nil :prologue t)
	  (:html
	   (head-of-page "Show All PNG Files" *standard-output*)
	   (:body
	    (:p
	     (:a :href "//www.symbolic-simulation.com/"
		 (:img
		  :src "customLogo.png"
		  :alt "Symbolic Simulation, LLC")))
	    (:h1 "Joint Military Symbology Web Service/Server")
	    (:h2 "Showing All PNG Files")
	    (:p (:table :border 1 :cellpadding 2 :cellspacing 0
			(maphash
			 #'(lambda (k v)
			     (let ((query (make-and-render-puri-uri "demo-file.png" (format nil "name=~A" k))))
			       (htm (:tr
				     (:td (:a :href (str query) (esc k)))
				     #+NIL (:td (:object :data query :type "image/png"))
				     (:td (:object :data query))))))
			 (png-hash-table *ms2525d*))))
	    (:p "This server is product of "
		(:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC"))))))))

(define-easy-handler (demo-2525c.png :uri "/demo-2525c.png" :acceptor-names '(normal-acceptor))
    ()
  "This returns the PNG files of some canned, hard-coded SVG MIL-STD-2525C SIDCs."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html 
       (head-of-page "Demo 2525C PNG" *standard-output*)
       (:body 
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Selected 2525C SIDCs as PNG")
	(:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (dolist (sidc *ms2525c-sidc-list*)
		      (let ((query (make-and-render-puri-uri "2525c.png" (format nil "sidc=~A" sidc))))
			(htm (:tr
			      #-NIL (:td (esc query))
			      #+NIL (:td (:iframe :src (esc query)))
			      #+NIL (:td (:object :data (esc query) :type "image/png"))
			      #-NIL (:td (:object
					  ;; :style "height:50% ; width:50%"
					  ;; :style "height:100px ; width:100px"
					  :style "height:auto ; width:auto"
					  :data (esc query)
					  :type "image/png")))))))
	    (:p "This server is product of "
		(:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC"))))))))

(define-easy-handler (demo-2525d.png :uri "/demo-2525d.png" :acceptor-names '(normal-acceptor))
    ()
  "This returns the PNG files of some canned, hard-coded SVG MIL-STD-2525D SIDCs."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html 
       (head-of-page "Demo 2525D PNG" *standard-output*)
       (:body 
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Selected 2525D SIDCs as PNG")
	(:p (:table :border 1 :cellpadding 2 :cellspacing 0
		    (dolist (sidc *ms2525d-sidc-list*)
		      (let ((query (make-and-render-puri-uri
				    "/2525d.png"
                                    (format nil "sidc=~A" sidc))))
			(htm (:tr
			      #-NIL (:td (esc query))
			      #+NIL (:td (:iframe :src (esc query)))
			      #+NIL (:td (:object :data (esc query) :type "image/png"))
			      #-NIL (:td (:object
					  ;; :style "height:50% ; width:50%"
					  ;; :style "height:100px ; width:100px"
					  :style "height:auto ; width:auto"
					  :data (esc query)
					  :type "image/png"))))))))
	(:p "This server is product of "
	    (:a :href (str "//www.symbolic-simulation.com") "Symbolic Simulation, LLC")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; User test
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(trace cxml-doc-of-svg-file)
(trace get-graphic-attribute-basename)

(trace get-frame-graphic)
(trace get-icon-graphic)
(trace get-status-graphic)
(trace get-hqtfdummy-graphic)
(trace get-amplifier-graphic)
(trace get-first-modifier-graphic)
(trace get-second-modifier-graphic)
|#

(defun break-down-ms2525d (ms2525d-sidc)
  (with-html-output (*standard-output*)
    (let ((sidc-object (make-instance 'ms2525d-sidc)))
      (parse sidc-object ms2525d-sidc)
      (htm (:table :border 1 :cellpadding 2 :cellspacing 0
		   (:tr
		    (:td "2525D SIDC:")
		    (:td (:p (esc ms2525d-sidc))))
		   (:tr
		    (:td "SVG:")
		    (:td (:object
			  :data (make-and-render-puri-uri
				 "/2525d.svg"
                                 (format nil "sidc=~a" ms2525d-sidc))
			  :style "height:auto ; width:auto"
			  ;; :style "height:100px ; width:100px"
			  :type "image/svg+xml")))
		   (:tr
		    (:td "PNG:")
		    (:td (:object
			  :data (make-and-render-puri-uri
				 "/2525d.png"
                                 (format nil "sidc=~a" ms2525d-sidc))
			  :style "height:auto ; width:auto"
			  ;; :style "height:100px ; width:100px"
			  :type "image/png")))
		   (loop for label-xml-node-pair in `(("Frame" ,(get-frame-graphic *ms2525d* sidc-object))
						      ("Icon" ,(get-icon-graphic *ms2525d* sidc-object))
						      ("Status" ,(get-status-graphic *ms2525d* sidc-object))
						      ("HQTFDummy" ,(get-hqtfdummy-graphic *ms2525d* sidc-object))
						      ("Amplifier" ,(get-amplifier-graphic *ms2525d* sidc-object))
						      ("First-Modifier" ,(get-first-modifier-graphic *ms2525d* sidc-object))
						      ("Second-Modifier" ,(get-second-modifier-graphic *ms2525d* sidc-object)))
			 do (progn
			      (log:debug "label-xml-node-pair ~S~%" label-xml-node-pair)
			      (htm (:tr
				    (:td (:p (esc (format nil "SVG ~A file: ~A"
							  (first label-xml-node-pair)
							  (get-graphic-attribute-basename (second label-xml-node-pair))))))
				    (:td (:p (:object
					      :data (make-and-render-puri-uri
                                                     "/demo-file.svg"
                                                     (format
						      nil
						      "name=~A"
						      (get-graphic-attribute-basename (second label-xml-node-pair))))
					      :style "height:auto ; width:auto"))))))))))))

(define-easy-handler (demo-enter-2525d :uri "/demo-enter-2525d"
				       :default-request-type :post
				       :acceptor-names '(normal-acceptor))
    (#+NIL (ms2525d-sidc :parameter-type 'string)
     (version :parameter-type 'string)
     (standard-identity :parameter-type 'string)
     (symbol-set :parameter-type 'string)
     (status :parameter-type 'string)
     (hq-task-force-dummy :parameter-type 'string)
     (amplifier-descriptor :parameter-type 'string)
     (entity :parameter-type 'string)
     (entity-type :parameter-type 'string)
     (entity-subtype :parameter-type 'string)
     (sector-1-modifier :parameter-type 'string)
     (sector-2-modifier :parameter-type 'string)
     (symbology-originator-identifier :parameter-type 'string)
     (symbology-originator-symbol-set :parameter-type 'string)
     (specified-by-the-symbology-originator :parameter-type 'string))
  "This test page allows you to debug the construction of a MIL-STD-2525D icon."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Joint Military Symbology Web Services Demo - 2525D" *standard-output*)
       (:body 
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Please enter a 2525D SIDC")
	(:p (:form :method :post
		   (:table :border 1 :cellpadding 2 :cellspacing 0
			   #+NIL
			   (:tr (:td "2525D SIDC:")
			    (:td (:input :type :text
					 :name "ms2525d-sidc"
					 :value (or ms2525d-sidc
						    #-NIL "300015000012010300000000000000"
						    #+NIL "300001000011010000000000000000"))))
			   ;; Set A
			   (:tr (:td "Version:")
				(:td (:input :type :text :maxlength 2 :name "version"
					     :value (or version "30"))))
			   (:tr (:td "Standard Identity")
				(:td (:input :type :text :maxlength 2 :name "standard-identity" 
					     :value (or standard-identity "02"))))
			   (:tr (:td "Symbol Set")
				(:td (:input :type :text :maxlength 2 :name "symbol-set"
					     :value (or symbol-set "01"))))
			   (:tr (:td "Status")
				(:td (:input :type :text :maxlength 1 :name "status"
					     :value (or status "0"))))
			   (:tr (:td "HQ Task Force Dummy")
				(:td (:input :type :text :maxlength 1 :name "hq-task-force-dummy"
					     :value (or hq-task-force-dummy "0"))))
			   (:tr (:td "Amplifier/Descriptor")
				(:td (:input :type :text :maxlength 2 :name "amplifier-descriptor"
					     :value (or amplifier-descriptor "00"))))
			   ;; Set B
			   (:tr (:td "Entity")
				(:td (:input :type :text :maxlength 2 :name "entity"
					     :value (or entity "11"))))
			   (:tr (:td "Entity Type")
				(:td (:input :type :text :maxlength 2 :name "entity-type"
					     :value (or entity-type "07"))))
			   (:tr (:td "Entity Subtype")
				(:td (:input :type :text :maxlength 2 :name "entity-subtype"
					     :value (or entity-subtype "07"))))
			   (:tr (:td "Sector 1 Modifier")
				(:td (:input :type :text :maxlength 2 :name "sector-1-modifier"
					     :value (or sector-1-modifier "00"))))
			   (:tr (:td "Sector 2 Modifier")
				(:td (:input :type :text :maxlength 2 :name "sector-2-modifier"
					     :value (or sector-2-modifier "00"))))
			   ;; Conditional Set C
			   (:tr (:td "Symbology Originator Identifier")
				(:td (:input :type :text :maxlength 3 :name "symbology-originator-identifier"
					     :value (or symbology-originator-identifier "110"))))
			   (:tr (:td "Symbology Originator Symbol Set")
				(:td (:input :type :text :maxlength 1 :name "symbology-originator-symbol-set"
					     :value (or symbology-originator-symbol-set "7"))))
			   (:tr (:td "Specified by the Symbology Originator")
				(:td (:input :type :text :maxlength 6 :name "specified-by-the-symbology-originator"
					     :value (or specified-by-the-symbology-originator "000000"))))
			   ;; Submit button
			   (:tr 
			    (:td :colspan 2
				 (:input :type "submit"))))))

	(when version
	  (let ((ms2525d-sidc
		 (concatenate
		  'string
		  version
		  standard-identity
		  symbol-set
		  status
		  hq-task-force-dummy
		  amplifier-descriptor
		  entity
		  entity-type
		  entity-subtype
		  sector-1-modifier
		  sector-2-modifier
		  symbology-originator-identifier
		  symbology-originator-symbol-set
		  specified-by-the-symbology-originator)))
	    #+NIL (clouseau:inspect (list
				     ms2525d-sidc
				     version
				     standard-identity
				     symbol-set
				     status
				     hq-task-force-dummy
				     amplifier-descriptor
				     entity
				     entity-type
				     entity-subtype
				     sector-1-modifier
				     sector-2-modifier
				     symbology-originator-identifier
				     symbology-originator-symbol-set
				     specified-by-the-symbology-originator))
	    (handler-case (break-down-ms2525d ms2525d-sidc)
	      (bad-sidc (var)
		#+NIL (clouseau:inspect var)
		(with-html-output (*standard-output*)
		  (htm (:h2 (str (error-text var))))))
	      (error (c)
		#+NIL (clouseau:inspect c)
		(let ((msg #+NIL (apply
				  'format
				  nil
				  (simple-condition-format-control c)
				  (simple-condition-format-arguments c))
			   #-NIL (with-standard-io-syntax (format nil "~a" c))))
		  (with-html-output (*standard-output*)
		    (htm (:h2 (str msg))))))))))))))

(defun break-down-ms2525c (ms2525c-sidc)
  (with-html-output (*standard-output*)
    (let ((sidc-object (make-instance 'ms2525c-sidc :sidc-to-parse ms2525c-sidc)))
      (parse sidc-object ms2525c-sidc)
      (htm (:table :border 1 :cellpadding 2 :cellspacing 0
		   (:tr
		    (:td "2525C SIDC:")
		    (:td (esc ms2525c-sidc)))
		   (:tr
		    (:td "SVG:")
		    (:td (:object
			  :data (make-and-render-puri-uri "2525c.svg" (format nil "sidc=~a" ms2525c-sidc))
			  :style "height:auto ; width:auto"
			  :type "image/svg+xml")))
		   (:tr
		    (:td "PNG:")
		    (:td (:object
			  :data (make-and-render-puri-uri "2525c.png" (format nil "sidc=~a" ms2525c-sidc))
			  :style "height:auto ; width:auto"
			  :type "image/png")))
	      
		   (htm
		      (loop for label-xml-node-pair in `(("Frame" ,(get-legacy-frame-graphic *ms2525c* sidc-object))
							 ("Icon" ,(get-legacy-icon-graphic *ms2525c* ms2525c-sidc sidc-object))
							 ("Status" ,(get-status-graphic2 *ms2525c* sidc-object))
							 ("HQTFDummy" ,(get-hqtfdummy-graphic *ms2525c* sidc-object))
							 #+NIL ("Amplifier" ,(get-amplifier-graphic *ms2525c* sidc-object))
							 #-NIL ("Modifier" ,(get-modifier-graphic *ms2525c* sidc-object))
							 #+NIL ("First-Modifier" ,(get-first-modifier-graphic *ms2525c* sidc-object))
							 #+NIL ("Second-Modifier" ,(get-second-modifier-graphic *ms2525c* sidc-object))
							 )
			    do (htm (:tr
				     (:td (:p (esc (format nil "SVG ~A file: ~A"
							   (first label-xml-node-pair) (second label-xml-node-pair)))))
				     (:td (:p (:object
					       :data (make-and-render-puri-uri
						      "/demo-file.svg"
						      (format nil "name=~a" (second label-xml-node-pair)))
					       :style "height:auto ; width:auto"
					       :type "image/svg+xml"
					       ))))))))))))

(define-easy-handler (demo-enter-2525c :uri "/demo-enter-2525c"
				       :default-request-type :post
				       :acceptor-names '(normal-acceptor))
    (#+NIL (ms2525c-sidc :parameter-type 'string)
	   (coding-scheme :parameter-type 'string)
	   (standard-identity :parameter-type 'string)
	   (battle-dimension :parameter-type 'string)
	   (operational-condition :parameter-type 'string)
	   (function-id :parameter-type 'string)
	   (symbol-modifier :parameter-type 'string)
	   (country-code :parameter-type 'string)
	   (order-of-battle :parameter-type 'string))
  "This test page allows you to debug the construction of a MIL-STD-2525C icon."
  (when (licensedp)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Joint Military Symbology Web Services Demo - 2525C" *standard-output*)
       (:body
	(:p
	 (:a :href "//www.symbolic-simulation.com/"
	     (:img
	      :src "customLogo.png"
	      :alt "Symbolic Simulation, LLC")))
	(:h1 "Joint Military Symbology Web Service/Server")
	(:h2 "Please enter a 2525C SIDC")
	(:p (:form :method :post
		   (:table :border 1 :cellpadding 2 :cellspacing 0
			   #+NIL
			   (:tr
			    (:td "2525C:")
			    (:td (:input :type :text
					 :name "ms2525c-sidc"
					 :value (or ms2525c-sidc "SUPPS-----*****"))))
			   (:tr (:td "Coding Scheme") 
				(:td (:input :type :text :maxlength 1 :name "coding-scheme"
					     :value (or coding-scheme "S"))))
			   (:tr (:td "Standard Identity")
				(:td (:input :type :text :maxlength 1 :name "standard-identity"
					     :value (or standard-identity "U"))))
			   (:tr (:td "Battle Dimension")
				(:td (:input :type :text :maxlength 1 :name "battle-dimension"
					     :value (or battle-dimension "P"))))
			   (:tr (:td "Status/Operational Condition")
				(:td (:input :type :text :maxlength 1 :name "operational-condition"
					     :value (or operational-condition "P"))))
			   (:tr (:td "Function ID")
				(:td (:input :type :text :maxlength 6 :name "function-id"
					     :value (or function-id "S-----"))))
			   (:tr (:td "Symbol Modifier")
				(:td (:input :type :text :maxlength 2 :name "symbol-modifier"
					     :value (or symbol-modifier "**"))))
			   (:tr (:td "Country Code")
				(:td (:input :type :text :maxlength 2 :name "country-code"
					     :value (or country-code "**"))))
			   (:tr (:td "Order of Battle")
				(:td (:input :type :text :maxlength 1 :name "order-of-battle"
					     :value (or order-of-battle "*"))))
			   (:tr
			    (:td :colspan 2
				 (:input :type "submit"))))))

	(when coding-scheme
	  (let ((ms2525c-sidc
		 (concatenate
		  'string
		  coding-scheme
		  standard-identity
		  battle-dimension
		  operational-condition
		  function-id
		  symbol-modifier
		  country-code
		  order-of-battle)))
	    (handler-case (break-down-ms2525c ms2525c-sidc)
	      (bad-sidc (var)
		#+NIL (clouseau:inspect var)
		(with-html-output (*standard-output*)
		  (htm (:h2 (str (error-text var))))))
	      (error (c)
		#+NIL (clouseau:inspect c)
		(let ((msg #+NIL (apply
				  'format
				  nil
				  (simple-condition-format-control c)
				  (simple-condition-format-arguments c))
			   #-NIL (with-standard-io-syntax (format nil "~a" c))))
		  (with-html-output (*standard-output*)
		    (htm (:h2 (str msg))))))))))))))

;;
;; The dead-end landing page for the CL-PREBID in case of license failure.
;;

#+NIL
(define-easy-handler (cl-prebid-dead-end :uri "/symbology" :acceptor-names '(unlicensed-acceptor))
  ()
  "Top-level, human-readable web server page.  Popped up in the case of license management failure."
  
  (multiple-value-bind (validp err)
      (cl-cryptolens:skey-file-valid-p cl-cryptolens:*cryptolens-endpoint*
				       cl-cryptolens:*simbology-product-id*
				       cl-cryptolens:*simbology-skey-filename*)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (head-of-page "Symbolic Simulation, LLC's Joint Military Simbology Web Service/Server" *standard-output*)
       (:body (:subtitle "License management failure")
	      #+NIL (with-html-output (*standard-output*)
		      (info-table
		       (lib-cryptolens::cryptolens-error-t.call cl-cryptolens::*current-error*)
		       (lib-cryptolens::cryptolens-error-t.subsystem cl-cryptolens::*current-error*)
		       (lib-cryptolens::cryptolens-error-t.reason cl-cryptolens::*current-error*)
		       (lib-cryptolens::cryptolens-error-t.extra cl-cryptolens::*current-error*)
		       ))
	      (htm (:p (str (format
			     nil
			     "Serial key ~s failed because: ~s~%"
			     (cl-cryptolens:read-skey-from-file cl-cryptolens:*simbology-skey-filename*) 
			     err))))
	      )
       )
      )
    )
  )

(define-easy-handler (cl-prebid-test-error-402 :uri "/test-error-402" :acceptor-names '(normal-acceptor))
    ()
  "Try to return the 402 error code"
  (setf (return-code*) +http-payment-required+)
  nil)

(define-easy-handler (test-error-handling :uri "/test-error-handling" :acceptor-names '(normal-acceptor))
    ((args :parameter-type '(list string)))
  #+NIL (clouseau:inspect (list :test-error-handling args))
  #+NIL
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (head-of-page "Test Error Handling" *standard-output*)
     (:body (:subtitle "*request* info")
	    (info-table
	     (headers-in       *request*)
	     (request-method   *request*)
	     (request-uri      *request*)
	     (server-protocol  *request*)
	     (local-addr       *request*)
	     (local-port       *request*)
	     (remote-addr      *request*)
	     (remote-port      *request*)
	     (hunchentoot::content-stream   *request*)
	     (cookies-in       *request*)
	     (get-parameters   *request*)
	     (post-parameters  *request*)
	     (script-name      *request*)
	     (query-string     *request*)))))
  #-NIL
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (head-of-page "Test Error Handling" *standard-output*)
     (:body
      (:subtitle "Subtitle")
      (:h1 "Test 404 Page H1")
      (:a "This is the prose"))
     (setf (return-code*) +http-not-found+)
     nil)))

(define-easy-handler (cl-prebid-license-test
		      :uri "/about-license-status"
		      :acceptor-names '(normal-acceptor))
    ()
  "See license status"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (head-of-page "Symbolic Simulation, LLC's Joint Military Simbology Web Service/Server" *standard-output*)
     (:body
      (:p
       (:a :href "//www.symbolic-simulation.com/"
	   (:img
	    :src "customLogo.png"
	    :alt "Symbolic Simulation, LLC")))
      (:h1 "Joint Military Simbology Web Service/Server")
      (:h2 "License Status (any will do)")
      (:h3 (fmt "~a License Status" (cl-cryptolens:product-name *simbology-product*)))
      (progn
	(cl-cryptolens:find-valid-skey-file *simbology-product*)
	(info-table
	 (cl-cryptolens:validp *simbology-product*)
	 (cl-cryptolens:license *simbology-product*)
	 (cl-cryptolens:error-message *simbology-product*)
	 (cl-cryptolens:skey-filename *simbology-product*)
	 (cl-cryptolens:skey-string *simbology-product*)
	 (cl-cryptolens:skey-basename *simbology-product*)
	 (cl-cryptolens:skey-filename-paths *simbology-product*)))
      (:h3 (fmt "~a License Status" (cl-cryptolens:product-name *geoblackboard-product*)))
      (progn
	(cl-cryptolens:find-valid-skey-file *geoblackboard-product*)
	(info-table
	 (cl-cryptolens:validp *geoblackboard-product*)
	 (cl-cryptolens:license *geoblackboard-product*)
	 (cl-cryptolens:error-message *geoblackboard-product*)
	 (cl-cryptolens:skey-filename *geoblackboard-product*)
	 (cl-cryptolens:skey-string *geoblackboard-product*)
	 (cl-cryptolens:skey-basename *geoblackboard-product*)
	 (cl-cryptolens:skey-filename-paths *geoblackboard-product*)))
      #+NIL
      (:h3 (fmt "~a License Status" (cl-cryptolens:product-name *simengine-product*)))
      #+NIL
      (progn
	(cl-cryptolens:find-valid-skey-file *simengine-product*)
	(info-table
	 (cl-cryptolens:validp *simengine-product*)
	 (cl-cryptolens:license *simengine-product*)
	 (cl-cryptolens:error-message *simengine-product*)
	 (cl-cryptolens:skey-filename *simengine-product*)
	 (cl-cryptolens:skey-string *simengine-product*)
	 (cl-cryptolens:skey-basename *simengine-product*)
	 (cl-cryptolens:skey-filename-paths *simengine-product*)))))))
