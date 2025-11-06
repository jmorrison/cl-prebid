;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: cl-user; -*-                                 
           
;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(defpackage #:cl-prebid/reblocks
  (:use
   :cl
   :assoc-utils
   :cl-prebid
   :cl-who
   :cl-fad
   )
  #+NIL
  (:import-from #:reblocks/actions
                #:make-js-form-action
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/request
		#:get-parameters)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/routes
                #:page)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:reblocks-ui2/themes/api
                #:*current-theme*)
  (:import-from #:reblocks-ui2/widget
                #:render
		#:ui-widget)
  #+NIL
  (:import-from #:serapeum
                #:soft-list-of)
  #+NIL
  (:import-from #:40ants-routes/route-url
                #:route-url)
  #+NIL
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get)
  (:export
   #:run
   )
  )
