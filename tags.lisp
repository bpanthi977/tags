;;;; tags.lisp

(in-package #:tags)

(defparameter *$tag-directory* nil)
(defparameter *.tag-file* nil)
(defparameter *entries* nil)
(defparameter *dirty-entries* nil)

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

(defun details= (d1 d2)
  (= (details-size d1) (details-size d2)))

(defun entry= (e1 e2)
  (and (string= (entry-file e1) (entry-file e2))
       (= (length (entry-tags e1)) (length (entry-tags e2)))
       (every #'string= (entry-tags e1) (entry-tags e2))
       (details= (entry-details e1) (entry-details e2))))

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
		   :tags (make-array (length tags-list) :element-type 'string
							:initial-contents tags-list)
		   :details (make-details :size (car details))))))

(defun write-entries (entries .tag-file)
  (with-open-file (stream .tag-file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (loop for entry in entries do
      (write (list (entry-file entry)
		   (coerce (entry-tags entry) 'list)
		   (list (details-size (entry-details entry))))
	     :stream stream))))


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

(defun save-changes ()
  (when *dirty-entries*
    (write-entries *entries* *.tag-file*)))

(defmacro with-$tag-directory (($tag-directory &key (read-tags nil)) &body body)
  `(let* ((*$tag-directory* ,$tag-directory)
	  (*.tag-file* (.tag-file *$tag-directory*))
	  (*entries* (if ,read-tags (read-entries *.tag-file*) *entries*))
	  (*dirty-entries* (progn (when *dirty-entries* 
				    (print "Dirty entries exist: ")
				    (print *dirty-entries*))
				  ())))
     (prog1 (progn ,@body)
       (save-changes))))

(defun push-change-in-entry (entry)
  (let ((e (find (entry-file entry) *entries* :test #'string-equal :key #'entry-file)))
    (cond ((not e)
	   (push entry *entries*)
	   (push entry *dirty-entries*))
	  ((not (entry= entry e))
	   (setf (nth (position e *entries*) *entries*) entry)
	   (push entry *dirty-entries*)))))

(defun tags->directories (tags)
  (declare (type (vector string) tags))
  (loop with dirs = ()
	for tag across tags do
	  (loop for dir in (uiop:split-string tag :separator "/") do
	    (push dir dirs))
	finally (return (reverse dirs))))

(defun update-tags* (file tagstring)
  (let* ((file (truename file))
	 (old (find-entry file))
	 (new (create-entry file (tags tagstring)))
	 
	 (additions (if old
			(u:difference (entry-tags new) (entry-tags old) :test #'string-equal)
			(entry-tags new)))
	 (deletions (when old
		      (u:difference (entry-tags old) (entry-tags new) :test #'string-equal))))

    (when (or (not (= (length deletions) 0))
	      (not (= (length additions) 0)))
      (push-change-in-entry new)
      
      (when old
	(a:map-permutations (lambda (tags)
			      (declare (type (vector string) tags))
			      (let ((path (link-pathname file (tags->directories tags))))
				(print path)
				(when (probe-file path)
				  (delete-file path))))
			    (entry-tags old)))

      (a:map-permutations (lambda (tags)
			    (declare (type (vector string) tags))
			    (let ((path (link-pathname file (tags->directories tags))))
			      (print path)
			      (unless (probe-file path) ; TODO: check if it points to same target
				(create-link path file))))
			  (entry-tags new)))
    new))

(defun update-tags (file tagstring)
  "Changes tags associated with `file'; first in link structure and then in .tags file"
  (declare (optimize (debug 3)))
  (with-$tag-directory ((find-tags-directory file) :read-tags t)
    (update-tags* file tagstring)))


(defun existing-tags* (file)
  (let ((entry (find-entry file)))
    (when entry
      (entry-tags-string entry))))

(defun existing-tags (file)
  (with-$tag-directory ((find-tags-directory file) :read-tags t)
    (existing-tags* file)))





