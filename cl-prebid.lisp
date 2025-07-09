;;; -*- Mode: Lisp; Package: cl-prebid -*-

;;;
;;; Copyright Symbolic Simulation, LLC, 2022.
;;;

(in-package :cl-prebid)

(defun run (&key (acceptor *normal-acceptor* acceptor-supplied-p))
  (hunchentoot:start acceptor))
