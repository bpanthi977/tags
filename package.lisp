;;;; package.lisp

(defpackage #:tags/utils
  (:use :cl)
  (:export #:difference
	   #:join-vector))

(defpackage #:tags
  (:use #:cl)
  (:local-nicknames (#:u #:tags/utils)
		    (#:a #:alexandria)))
