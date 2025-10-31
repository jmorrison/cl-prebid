;;; -*- Mode: Lisp; Package: cl-prebid/reblocks -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2025.
;;;

(defpackage #:todo
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-form-action)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/routes
                #:page)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:40ants-routes/route-url
                #:route-url)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui2/themes/api
                #:*current-theme*)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:make-tailwind-theme)
  (:shadowing-import-from #:40ants-routes/defroutes
                          #:get))
(in-package #:todo)


;(trace reblocks/widget:render :break t)
;(trace reblocks-ui2/widget:render :break t)

(defclass task ()
  ((id :initarg :id
       :initform (error "Field ID is required")
       :accessor id
       :type integer)
   (title :initarg :title
          :initform ""
          :accessor title)
   (description :initarg :description
                :initform ""
                :accessor description)
   (done :initarg :done
         :initform nil
         :accessor done)))


(defvar *store* (make-hash-table)
  "Dummy store for tasks: id -> task.")


(defvar *counter* 0
  "Simple counter for the hash table store.")


(defun make-task (title &key done)
  "Create a task and store it by its id."
  (let* ((id (incf *counter*))
         (task (make-instance 'task :title title :done done :id id)))
    (setf (gethash id *store*) task)
    task))


(defun get-task (id)
  (gethash id *store*))


;;;;;; Task list item

(defwidget list-item ()
  ((task :initarg :task
         :type task
         :reader task)))


(defun make-list-item (task)
  (make-instance 'list-item
                 :task task))


(defun toggle (list-item)
  (let ((task (task list-item)))
    (setf (done task)
          (if (done task)
              nil
              t))
    (update list-item)))


(defmethod reblocks/widget:render ((list-item list-item))
  (let ((task (task list-item)))
    (with-html ()
      (:p (:input :type "checkbox"
                  :checked (done task)
                  :onclick (make-js-form-action
                            (lambda (&key &allow-other-keys)
                              (toggle list-item))))
          (if (done task)
              (:s (title task))
              (title task))))))


;;;;;; Index page

(defwidget task-list ()
  ((items :initarg :items
          :type (soft-list-of list-item)
          :accessor list-items)))


(defun make-task-list (&rest task-titles)
  (let ((items
          (loop for title in task-titles
                for task = (make-task title)
                collect (make-list-item task))))
    (make-instance 'task-list
                   :items items)))


(defun add-task (task-list  title)
  (serapeum:push-end (make-list-item (make-task title))
                     (list-items task-list))
  (update task-list))


;; Alternative version for sending only a new item's HTML to the frontend
;; (defun add-task (task-list  title)
;;   (let ((last-item (alexandria:last-elt
;;                     (list-items task-list)))
;;         (new-item (make-list-item (make-task title))))
;;     (serapeum:push-end new-item
;;                        (list-items task-list))

;;     ;; This time we are calling update on a new list item:
;;     (update new-item
;;             ;; And providing to the frontend
;;             ;; a hint that we've inserted this new-item
;;             ;; after the some other item:
;;             :inserted-after last-item)))


(defmethod reblocks/widget:render ((task-list task-list))
  (with-html ()
    (:h1 "Tasks")
    
    (loop for item in (list-items task-list) do
      (reblocks/widget:render item))

    ;; Form for adding a new task
    (flet ((on-submit (&key title &allow-other-keys)
             (add-task task-list title)))
      (:form :onsubmit (make-js-form-action #'on-submit)
             (:input :type "text"
                     :name "title"
                     :placeholder "Task's title")
             (:input :type "submit"
                     :class "button"
                     :value "Add")))))


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

(defapp tasks
  :prefix "/"
  ;; :name "tasks"
  :page-constructor 'wrap-with-frame
  :routes ((page ("/foo" :name "tasks-list")
             (make-task-list "First"
                             "Second"
                             "Third"))
	   (page ("/" :name "/")
		 ;; (make-instance 'cl-prebid-container)
		 (reblocks/widgets/string-widget:make-string-widget "root-widget")
		 )))


(defmethod body-classes ((app tasks))
  ;; (clouseau:inspect (list :body-classes app))
  (join-css-classes *current-theme* (colors-bg-normal *current-theme*)))

(defun start (&key (port 8080))
  (setf reblocks-ui2/themes/api::*current-theme*
        (reblocks-ui2/themes/tailwind:make-tailwind-theme))
  ;; (setf REBLOCKS/SERVER:*DEFAULT-SAMESITE-POLICY* :lax)
  (reblocks/server:start :port port
                         :apps '(tasks)))
