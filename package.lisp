;;;; package.lisp

(defpackage #:tags/utils
  (:use :cl)
  (:export #:difference
           #:join-vector
           #:equal-set))

(defpackage #:tags
  (:use #:cl)
  (:local-nicknames (#:u #:tags/utils)
                    (#:a #:alexandria))
  (:export #:existing-tags*
           #:existing-tags
           #:update-tags*
           #:update-tags
           #:with-tag-directory
           #:find-tags-directory))
