;;; -*- Mode: Lisp; Package: cl-prebid/hunchentoot -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(in-package :cl-prebid/hunchentoot)

(defvar *normal-acceptor* (make-instance
			   #-NIL 'hunchentoot:easy-acceptor
			   #+NIL 'acceptor-which-logs
			   :name 'normal-acceptor
			   ;; 'hunchentoot:easy-ssl-acceptor
			   ;; :ssl-certificate-file #P"symbology.crt"
			   ;; :ssl-privatekey-file #P"symbology.key"
			   :port 4242
			   :document-root (cl-fad:merge-pathnames-as-directory
					   (uiop/os:getcwd)
					   #P"www/")))

(defparameter *test-bidder-banner-example-script*
  "
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

(defparameter *test-bidder-native-example-script*
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
                /**
                 * BID RESPONSE SIMULATION SECTION END
                 */

                pbjs.addAdUnits(adUnits);
                pbjs.requestBids({
                    adUnitCodes: [adUnitCode],
                    bidsBackHandler: function() {
                        const bids = pbjs.getHighestCpmBids(adUnitCode);
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
                        iframe.onload = () => pbjs.renderAd(iframe.contentDocument, bid.adId);
                    }
                });
            });
")

(defparameter *test-bidder-video-example-script*
  "
            var pbjs = pbjs || {};
            pbjs.que = pbjs.que || [];

            const adUnitCode = 'adUnit-0000';

            const adUnits = [{
                mediaTypes: {
                    video: {
                        playerSize: [640, 360],
                        context: 'outstream'
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
                        pbjs.renderAd(document, winningBid.adId);
                    }
                });
            });
")

(defun run (&key (acceptor *normal-acceptor* acceptor-supplied-p))
  (hunchentoot:start acceptor))
