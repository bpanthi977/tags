;;;; tags.lisp

(in-package #:tags)

(defun find-tags-directory% (dir)
  (declare (optimize (debug 3) (safety 3)))
  (let* ((dir (truename dir))
	 ($tags-directory (uiop:directory-exists-p (merge-pathnames #p"./$tags" dir))))
    (if $tags-directory
	$tags-directory
	(let ((parent (uiop:pathname-parent-directory-pathname dir)))
	  (unless (uiop:pathname-equal parent dir)
	    ;; because parent of #p"/" is #p"/" which could result to infinite recursion
	    (find-tags-directory parent))))))

(defun find-tags-directory (file-or-dir)
  (find-tags-directory% (if (uiop:directory-pathname-p file-or-dir)
			   (uiop:pathname-parent-directory-pathname file-or-dir)
			   (uiop:pathname-directory-pathname file-or-dir))))

(defun create-link (file target)
  (osicat:make-link ))

(defun read-tag-details (.tag-file)
  (when (probe-file .tag-file)
    (uiop:with-safe-io-syntax  ()
      (uiop:read-file-forms .tag-file))))

(defun .tag-file ($tag-directory)
  (merge-pathnames "./.tags" $tag-directory))

(defun pathname-name.type (pathname)
  (let ((name (pathname-name pathname))
	(ext (pathname-type pathname)))
    (if ext
	(format nil "~a.~a" name ext)
	(format nil "~a" name))))

(defun id (tag file)
  (format nil "~a/~a" tag (if (uiop:directory-pathname-p file)
			      (car (last (pathname-directory file)))
			      (pathname-name.type file))))

(defun save-tag-details ($tag-directory file tag &optional existing-tags)
  "Save `file' details in .tags file; so that if link breaks it may later be restored"
  (declare (optimize (debug 3)))
  (let* ((.tag-file (.tag-file $tag-directory))
	 (existing-tags (or existing-tags (read-tag-details .tag-file)))
	 (rewrite nil)
	 (nochange nil)

	 ;; record 
	 (id (id tag file))
	 (file-namestring (namestring file))
	 (size (osicat-posix:stat-size (osicat-posix:stat file)))
	 (new-record (list id file-namestring size)))

    (loop for (id* file* size*) in existing-tags
	  for i from 0 do
	    (when (string-equal id id*)
	      (if (and (string-equal file* file-namestring)
		       (= size size*))
		  (setf nochange t)
		  (setf rewrite i))))
    (cond (rewrite
	   (setf (nth rewrite existing-tags) new-record)
	   (with-open-file (stream .tag-file :direction :output
					     :if-exists :supersede)
	     (write (namestring $tag-directory) :stream stream)
	     (loop for record in existing-tags do
	       (write record :stream stream)
	       (write-char #\Newline stream ))))
	  ((not nochange)
	   (setf existing-tags (cons new-record existing-tags))
	   (with-open-file (stream .tag-file :direction :output
					     :if-exists :append
					     :if-does-not-exist :create)
	     (write new-record :stream stream)
	     (write-char #\Newline stream))))
    ;; updated tags
    existing-tags))

(defun remove-tag-details ($tag-directory file tag &optional existing-tags)
  "remove tag from .tags file"
  (let* ((.tag-file (.tag-file $tag-directory))
	 (id (id tag file))
	 (existing-tags (or existing-tags (read-tag-details .tag-file))))
    (setf existing-tags
	  (remove-if (lambda (record)
		       (string-equal (car record) id))
		     existing-tags))
    (with-open-file (stream .tag-file :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
      (loop for record in existing-tags do
	(write record :stream stream)
	(write-char #\Newline stream)))
    existing-tags))

(defun add-tag (file tag)
  "add `file' to `tag' folder"
  (let* ((file (truename file))
	 (tag (string-downcase tag))
	 ($tags-directory (find-tags-directory file))
	 (tag-directory (ensure-directories-exist
			 (merge-pathnames (make-pathname :directory (cons :relative (uiop:split-string tag :separator "/")))
					  $tags-directory)))
	 (link-file (if (uiop:directory-pathname-p file)
			(make-pathname :name (car (last (pathname-directory file)))
				       :defaults tag-directory)
			(make-pathname :name (pathname-name file)
				   :type (pathname-type file)
				   :defaults tag-directory))))
    (when (probe-file link-file)
      (delete-file link-file))
    (osicat:make-link link-file :target file :hard nil)
    (save-tag-details $tags-directory file tag)))

(defun remove-tag (file tag)
  (let* ((file (truename file))
	 (tag (string-downcase tag))
	 ($tags-directory (find-tags-directory file))
	 (tag-directory (ensure-directories-exist
			 (merge-pathnames (make-pathname :directory (list :relative tag))
					  $tags-directory)))
	 (link-file (make-pathname :name (pathname-name file)
				   :type (pathname-type file)
				   :defaults tag-directory)))
    (when (probe-file link-file)
      (delete-file link-file))
    (remove-tag-details $tags-directory file tag)))

(defun existing-tags (file &optional $tags-directory)
  (let* ((file (truename file))
	 ($tags-directory (or $tags-directory (find-tags-directory file)))
	 (file-namestring (namestring file))
	 (all-tags (read-tag-details (.tag-file $tags-directory))))
    (loop for (id* file-namestring* nil) in all-tags
	  when (string= file-namestring* file-namestring)
	    collect (subseq id* 0 (position #\/ id* :from-end t)))))

(defun update-tags (file tagstring)
  "`tagstring' is a string of tags separated by comma (,)"
  (let* ((file (truename file))
	 (tags (mapcar (lambda (s) (string-trim " " s)) (uiop:split-string (string-downcase tagstring) :separator ",")))
	 ($tags-directory (find-tags-directory file))
	 (previous-tags (existing-tags file $tags-directory))
	 (additions (set-difference tags previous-tags :test #'string-equal))
	 (deletions (set-difference previous-tags tags :test #'string-equal)))
    (print additions)
    (loop for tag in additions do
      (add-tag file tag))
    (loop for tag in deletions do
      (remove-tag file tag))))

(defun existing-tags* (file)
  (format nil "~{~a~^,~}" (existing-tags file)))

    



