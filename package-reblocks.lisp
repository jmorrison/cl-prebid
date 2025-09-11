;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: cl-user; -*-                                 
           
;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(defpackage #:cl-prebid/reblocks
  (:use
   :cl
   :cl-prebid
   :cl-who
   :cl-fad
;;   :reblocks
   )
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-form-action
                #:make-js-action)
  (:import-from #:reblocks/app
                #:defapp)
  #+NIL
  (:import-from #:reblocks/routes
                #:page)
  (:import-from #:serapeum
                #:soft-list-of)
  #+NIL
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  #+NIL
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get)
  
  (:export
   #:run
   )
  )
