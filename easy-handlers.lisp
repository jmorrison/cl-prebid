;;; -*- Mode: Lisp; Package: cl-prebid -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(in-package :cl-prebid/hunchentoot)

(eval-when (:compile-toplevel)
	   (setf cl-who::*downcase-tokens-p* NIL))

(defvar *licensedp* nil)
(defvar *js*
  (ps:ps*
   '(progn
     (defvar sizes '(300 200))
     (defvar *prebid_timeout* 700)
     (defvar ad-units (array (ps:create
			      code "/19968336/header-bid-tag-1"
			      media-types (ps:create banner (ps:create sizes sizes))
			      bids (array (ps:create bidder "appnexus" params (ps:create placement-id "XXXXXXX"))))))
     (defvar googletag (or googletag (ps:create)))
     (setf (ps:@ googletag cmd) (or (ps:@ googletag cmd) (array)))
     (ps:chain googletag cmd (push (lambda () (ps:chain googletag (pubads) (disable-initial-load)))))
     (defvar pbjs (or pbjs (create)))
     (setf (ps:@ pbjs que) (or (ps:@ pbfs que) (array)))
     (ps:chain pbjs que (push (lambda ()
				(ps:chain pbjs (add-ad-units ad-units))
				(ps:chain pbjs (request-bids (ps:create bids-back-handler init-adserver))))))
     (defun init-adserver ()
       (cond ((ps:@ init-adserver-set) nil)
	     (t
	      (setf (ps:@ pbjs init-adserver-set) t)
	      (ps:chain googletag cmd (push (lambda ()
					      (cond ((ps:@ pbjs lib-loaded)
						     (ps:chain pbjs que (push (lambda ()
										(ps:chain pbjs (set-targeting-for-g-p-t-async))
										(ps:chain googletag (pubads) (refresh))))))
						    (t
						     (ps:chain googletag (pubads) (refresh))))))))))
     (set-timeout (lambda () (init-adserver)) *prebid_timeout*)
     (ps:chain googletag cmd (push (lambda ()
				     (ps:chain googletag (define-slot "/19968336/header-bid-tag-1" sizes "div-1") (add-service (ps:chain googletag (pubads))))
				     (ps:chain googletag (pubads) (enable-single-request))
				     (ps:chain googletag (enable-services)))))
     (alert "foo"))))

; (clouseau:inspect (ps:ps* '(alert "bar")))

;;
;; Error condition pages
;;
;; Should probably also be moved to com.symsim.utils
;;

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

(define-easy-handler (landing-page :uri "/" :acceptor-names '(normal-acceptor))
    ()
  "Landing test page"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "Landing Page")
      (:base :href "/")
      (:script (str (cl-prebid::slurp-javascript-file "Prebid.js/build/dev/prebid.js")))
      )
     (:body
      (:div
       :id="div-0" :style "min-height:250px"
       (:p (:a "landing page div-0"))
       (:p (:a "foo 1: " cl-prebid::*foo*))
       (:p (:a "foo 2: " (str cl-prebid::*foo*)))
       #+NIL (:p (:a "prebid-js 1: " cl-prebid::*prebid-js*))
       #+NIL (:p (:a "prebid-js 2: " (str cl-prebid::*prebid-js*)))
       )))))
      
;;
;;

(define-easy-handler (cl-prebid :uri "/foo" :acceptor-names '(normal-acceptor))
    ()
  "Landing test page"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "LICENSED TITLE")
      (:base :href "/")
      (:script :type "text/javascript" :src "https://cdn.jsdelivr.net/npm/prebid.js@latest/dist/not-for-prod/prebid.js")
      (:script :type "text/javascript" :src "https://securepubads.g.doubleclick.net/tag/js/gpt.js")
      (:script :type "text/javascript" (if *licensedp* nil (cl-who:str *js*)))
      ;; (:style (str com.symsim.utils::*font-face-style-css-string*))
      ;; (:style (str com.symsim.utils::*use-custom-font-style-string*))
      )
     (:body
      (:div
       :id="div-1" :style "min-height:250px"
       (:a "licensed div-1"))
      (:div
       :id="div-2" :style "min-height:250px"
       (:a "licensed div-2"))))))

