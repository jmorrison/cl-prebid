;;; -*- Mode: Lisp; Package: cl-prebid -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(in-package :cl-prebid/hunchentoot)

(eval-when (:compile-toplevel)
	   (setf cl-who::*downcase-tokens-p* NIL))

(defvar *licensedp* t)

;;
;; Banner test
;;

(setf
 *test-bidder-banner-ps*
 '(let ()
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
										       (div (ps:chain document (get-element-by-id "banner")))
										       (iframe (ps:chain document (create-element "iframe"))))
										  (ps:chain div (append-child iframe))
										  (let ((iframe-doc (ps:chain iframe content-window document)))
										    (ps:chain pbjs (render-ad iframe-doc (ps:chain winning-bid ad-id))))))))))))))

;; (ps:ps* *test-bidder-banner-ps*)

#+NIL
(setf
 *test-bidder-banner-js*
       "
console.log(pbjs);
            var pbjs = pbjs || {};
            pbjs.que = pbjs.que || [];

            const adUnitCode = 'adUnit-0000';

            const adUnits = [{
                mediaTypes: {
                    banner: {
                        sizes: [600, 500]
                    }
                },
                code: adUnitCode,
                bids: [
                    {bidder: 'testBidder', params: {}}
                ]
            }]

            pbjs.que.push(function () {

                /**
                 * BID RESPONSE SIMULATION SECTION START
                 *
                 * This section handles simulating a bidder
                 * that will always respond with bid responses.
                 *
                 * This part should not be present in production.
                 */
                pbjs.registerBidAdapter(null, 'testBidder', {
                    supportedMediaTypes: ['banner', 'video', 'native'],
                    isBidRequestValid: () => true
                });

                pbjs.setConfig({
                    debugging: {
                        enabled: true,
                        intercept: [
                            {
                                when: {
                                    bidder: 'testBidder',
                                },
                                then: {
                                    creativeId: 'testCreativeId',
                                }
                            }
                        ]
                    }
                });
                /**
                 * BID RESPONSE SIMULATION SECTION END
                 */

                pbjs.addAdUnits(adUnits);
                pbjs.requestBids({
                    adUnitCodes: [adUnitCode],
                    bidsBackHandler: function() {
                        const bids = pbjs.getHighestCpmBids(adUnitCode);
                        const winningBid = bids[0];
                        const div = document.getElementById('banner');
                        let iframe = document.createElement('iframe');
                        iframe.frameBorder = '0';
                        div.appendChild(iframe);
                        var iframeDoc = iframe.contentWindow.document;
                        pbjs.renderAd(iframeDoc, winningBid.adId);
                    }
                });
            });
")

(define-easy-handler (test-bidder-banner-example :uri "/test-bidder-banner-example" :acceptor-names '(normal-acceptor))
    ()
  "testBidderBannerExample"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "testBidderBannerExample")
      (:script (str (cl-prebid::slurp-javascript-file "prebid10.2.0.js")))
      (:script (str (ps:ps* *test-bidder-banner-ps*))))
     (:body
      (:div
       :id "banner")
      (:div
       (:p (:a "pa1"))
       (:p (:a "pa2")))))))

(define-easy-handler (test-bidder-banner-example2 :uri "/test-bidder-banner-example2" :acceptor-names '(normal-acceptor))
    ()
  "testBidderBannerExample2"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "testBidderBannerExample2")
      (:script (str (cl-prebid::slurp-javascript-file "prebid10.2.0.js")))
      (:script (str (ps:ps* *test-bidder-banner-ps*))))
     (:body
      (:div
       :id "banner")
      (:div
       :id "debugging"
       (:p (:a (fmt "*licensedp*: ~s" *licensedp*))))
      (:div
       :id "regular body")
      (:p (:a "pa1"))
      (:p (:a "pa2"))))))

(defun fake-licensed-p ()
  (format t "fake-license-check~%")
  #+NIL t
  #-NIL nil
  )

(define-easy-handler (test-bidder-banner-example3 :uri "/test-bidder-banner-example3" :acceptor-names '(normal-acceptor))
    ()
  "testBidderBannerExample3"
  (let ((licensed-p (funcall #'fake-licensed-p)))
    (format t "protected-page - ~s~%" licensed-p)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (:head
	(:title "testBidderBannerExample3")
	(unless licensed-p
	  (htm (:script (str (cl-prebid::slurp-javascript-file "prebid10.2.0.js"))))
	  (htm (:script (str (ps:ps* *test-bidder-banner-ps*))))))
       (:body
	(unless licensed-p (htm (:div :id "banner")))
	(:div
	 :id "debugging"
	 (:p (:a (fmt "licensed-p: ~s" licensed-p))))
	(:div
	 :id "regular body")
	(:p (:a "pa1"))
	(:p (:a "pa2")))))))

;;
;; Native test
;;

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
     (setf (ps:chain googletag cmd) (or (ps:chain googletag cmd) (array)))
     (ps:chain googletag cmd (push (lambda () (ps:chain googletag (pubads) (disable-initial-load)))))
     (defvar pbjs (or pbjs (create)))
     (setf (ps:chain pbjs que) (or (ps:chain pbfs que) (array)))
     (ps:chain pbjs que (push (lambda ()
				(ps:chain pbjs (add-ad-units ad-units))
				(ps:chain pbjs (request-bids (ps:create bids-back-handler init-adserver))))))
     (defun init-adserver ()
       (cond ((ps:chain init-adserver-set) nil)
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
				     (ps:chain googletag (define-slot "/19968336/header-bid-tag-1" sizes "div-1") (add-service (ps:chain googletag (pubads))))
				     (ps:chain googletag (pubads) (enable-single-request))
				     (ps:chain googletag (enable-services)))))
     (alert "foo"))))

; (clouseau:inspect (ps:ps* '(alert "bar")))

#+NIL
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
       :id "div-1" :style "min-height:250px"
       (:a "licensed div-1"))
      (:div
       :id "div-2" :style "min-height:250px"
       (:a "licensed div-2"))))))

(define-easy-handler (test-bidder-native-example :uri "/test-bidder-native-example" :acceptor-names '(normal-acceptor))
    ()
  "testNativeBannerExample"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "testNativeBannerExample")
      ;; (:script (str (cl-prebid::slurp-javascript-file "Prebid.js/build/dist/prebid.js")))
      (:script (str (cl-prebid::slurp-javascript-file "prebid10.2.0.js")))
      (:script
       "
            var pbjs = pbjs || {};
            pbjs.que = pbjs.que || [];

            const adUnitCode = 'adUnit-0000';

            const adUnits = [{
                mediaTypes: {
                    native: {
                        ortb: {
                            assets: [
                                { 
                                    required: true,
                                    id: 1,
                                    img: {
                                        type: 3,
                                        wmin: 100,
                                        hmin: 100,
                                    }
                                },
                                {
                                    required: true,
                                    id: 2,
                                    img: {
                                        type: 3,
                                        wmin: 200,
                                        hmin: 200
                                    }
                                },
                                {
                                    required: true,
                                    id: 3,
                                    data: {
                                        type: 3,
                                        len: 20
                                    }
                                },
                                {
                                    required: true,
                                    id: 4,
                                    title: {
                                        len: 20
                                    }
                                }
                            ]
                        }
                    }
                },
                code: adUnitCode,
                bids: [
                    {bidder: 'testBidder', params: {}}
                ]
            }]

            pbjs.que.push(function () {

                /**
                 * BID RESPONSE SIMULATION SECTION START
                 *
                 * This section handles simulating a bidder
                 * that will always respond with bid responses.
                 *
                 * This part should not be present in production.
                 */
                pbjs.registerBidAdapter(null, 'testBidder', {
                    supportedMediaTypes: ['banner', 'video', 'native'],
                    isBidRequestValid: () => true
                });

                pbjs.setConfig({
                    debugging: {
                        enabled: true,
                        intercept: [
                            {
                                when: {
                                    bidder: 'testBidder',
                                },
                                then: {
                                    creativeId: 'testCreativeId',
                                }
                            }
                        ]
                    }
                });
pbjs.setConfig({'debug':true});
                /**
                 * BID RESPONSE SIMULATION SECTION END
                 */

                pbjs.addAdUnits(adUnits);
                pbjs.requestBids({
                    adUnitCodes: [adUnitCode],
                    bidsBackHandler: function() {
                        const bids = pbjs.getHighestCpmBids(adUnitCode);
console.log(bids);
                        const bid = bids[0];
                        const slot = document.getElementById('native');
                        const iframe = document.createElement('iframe');
                        Object.entries({
                            frameBorder: 0,
                            marginWidth: 0,
                            marginHeight: 0,
                            width: 600,
                            height: 500,
                            scrolling: 'no',
                            srcdoc: document.getElementById('native-template').innerHTML
                        }).forEach(([prop, val]) => iframe.setAttribute(prop, val));
                        slot.appendChild(iframe);
console.log('adUnitCode: ', adUnitCode);
console.log('bids:', bids);
console.log('bid: ', bid);
console.log('bid.adId: ', bid.adId);
console.log(iframe.contentDocument);
                        iframe.onload = () => pbjs.renderAd(iframe.contentDocument, bid.adId);
                    }
                });
            });
")
      )
     (:body
      #-NIL
      "        <template id='native-template'>
            <style>
                body {
                    display: inline-block;
                }
        
                .container {
                    display: inline-block;
                    font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
                    font-size: 14px;
                    line-height: 1.42857143;
                    box-sizing: border-box;
                    border: 3px dashed red;
                    padding: 20px;
                    color: #333;
                }

                .img {
                    width: 300px;
                    height: 200px;
                }
    
            </style>
            <div class='container'>
                <h1>##hb_native_asset_id_4##</h1>
                <h3>Rating: ##hb_native_asset_id_3##</h3>
                <img class='img' src='##hb_native_asset_id_1##' alt='bg' />
            </div>
        </template>
"
      #+NIL
      (:template
       :id "native-template"
       (:style (cl-who:str (cl-css:css '((body
					  :display inline-block)
					 (.container
					  :display inline-block
					  :font-family :Helvetica
					  :font-size 14px
					  :line-height 1.42857143
					  :box-sizing border-box
					  :border "3px dashed red"
					  :padding 20px
					  :color \#333
					  )
					 (.img
					  :width 300px
					  :height 200px)))))
       (:div
	:class "container"
	(:h1 "##hb_native_asset_id_4##")
	(:h3 "Rating: ##hb_native_asset_id_3##")
	(:img :class "img" :src "##hb_native_asset_id_1##" :alt "bg")))
#|
      (:h2 "Prebid Test Bidder Example")
      (:h5 "Native ad")
      (:div :id "native")       
|#
"
        <h2>Prebid Test Bidder Example</h2>
        <h5>Native ad</h5>
        <div id='native'></div>
    <script src='//localhost:35729/livereload.js?snipver=1' async='' defer=''></script>
"
      )
     )
    )
  )

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
      (:script (str (cl-prebid::slurp-javascript-file "prebid10.2.0.js")))
      )
     (:body
      (:div
       :id "div-0" :style "min-height:250px"
       (:p (:a "landing page div-0"))
       (:p (:a :href "./test-bidder-banner-example" (:b "testBidderBannerExample")))
       (:p (:a :href "./test-bidder-banner-example2" (:b "testBidderBannerExample2")))
       (:p (:a :href "./test-bidder-banner-example3" (:b "testBidderBannerExample3")))
       (:p (:a :href "./test-bidder-native-example" (:b "testBidderNativeExample")))
       (:p (:a :href "./foo" (:b "foo")))
       (:p (:a :href "./test-bidder-video-example" (:b "testBidderVideoExample"))))))))


;;
;;
;;



