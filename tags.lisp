;;;; tags.lisp

(in-package #:tags)

(defparameter *$tag-directory* nil)
(defparameter *.tag-file* nil)
(defparameter *entries* nil)

;;;
;;; Data Structure 
;;; 

(defstruct details
  (size nil :type fixnum))

(defstruct entry
  "`tagstring' is a string of tags separated by comma (,)"
  (file nil :type string)
  (tags nil :type (vector string))
  (%tags-string nil :type (or null string))
  (details nil :type details))

(defun tags-string (tags)
  "Converts vector of tags to tag-string"
  (declare (type (vector string) tags))
  (u:join-vector tags #\,))

(defun entry-tags-string (entry)
  (declare (type entry entry))
  (let ((string (entry-%tags-string entry)))
    (if string
	string
	(setf (entry-%tags-string entry) (tags-string (entry-tags entry))))))

(defun %tags (tags-string)
  (declare (type string tags-string))
  (mapcar (lambda (str)
	    (string-trim " " str))
	  (uiop:split-string tags-string :separator ";,")))

(defun tags (tags-string)
  (declare (type string tags-string))
  (let ((tags (%tags tags-string)))
    (the (vector string)
	 (make-array (length tags)
		     :element-type 'string
		     :initial-contents tags))))

(defun read-entries (.tag-file)
  (declare (type pathname .tag-file))
  (when (probe-file .tag-file)
    (loop for entry in (uiop:with-safe-io-syntax ()
			(uiop:read-file-forms .tag-file))
	  for (file tags-list details) = entry
	  for tags = (coerce tags-list '(vector string))
	  collect (make-entry
		   :file file
		   :tags (tags tags-string)
		   :details (make-details :size (car details))))))

(defun create-entry (file tags)
  (make-entry
   :file (namestring (truename file))
   :tags tags
   :%tags-string (tags-string tags)
   :details (make-details :size (file-size file))))

(defun find-entry (file)
  "Find entry for `file' in *entries*"
  (declare (type pathname file))
  (the (or entry null)
       (find (namestring (truename file)) *entries* :test #'string-equal :key #'entry-file)))

;;;
;;; Tagging System
;;;

(defmacro with-$tag-directory (($tag-directory &key (read-tags nil)) &body body)
  `(let* ((*$tag-directory* ,$tag-directory)
	  (*.tag-file* (.tag-file *$tag-directory*))
	  (*entries* (if ,read-tags (read-entries *.tag-file*) *entries*)))
     ,@body))

(defun tags->directories (tags)
  (declare (type (vector string) tags))
  (loop with dirs = ()
	for tag across tags do
	(loop for dir in (uiop:split-string tag :separator "/") do
	      (push dir dirs))
	finally (return (reverse dirs))))

(defun update-tags (file tagstring)
  "Changes tags associated with `file'"
  (declare (optimize (debug 3)))
  (with-$tag-directory ((find-tags-directory file) :read-tags t)
    (let* ((file (truename file))
	   (old (find-entry file))
	   (new (create-entry file (tags tagstring)))
	   
	   (additions (if old
			  (u:difference (entry-tags new) (entry-tags old) :test #'string-equal)
			  (entry-tags new)))
	   (deletions (when old
			(u:difference (entry-tags old) (entry-tags new) :test #'string-equal))))


      (unless (= (length deletions) 0)
	(a:map-permutations (lambda (tags)
			      (declare (type (vector string) tags))
			      (let ((path (link-pathname file (tags->directories tags))))
				(when (probe-file path)
				  (delete-file path))))
			    (entry-tags old)))
      (unless (= (length additions) 0)
	(a:map-permutations (lambda (tags)
			      (declare (type (vector string) tags))
			      (create-link (link-pathname file (tags->directories tags)) file))
			    (entry-tags new))))))


(defun existing-tags* (file)
  (let ((entry (find-entry file)))
    (when entry
      (entry-tags-string entry))))

    



