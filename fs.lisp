;;;; file system traversing and other utilites

(in-package :tags)

;;; finds #tags directory

(defun find-tags-directory% (dir)
  (declare (type pathname dir))
  (let* ((dir (truename dir))
         (tags-directory (uiop:directory-exists-p (merge-pathnames #p"./#tags" dir))))
    (if tags-directory
        tags-directory
        (let ((parent (uiop:pathname-parent-directory-pathname dir)))
          (unless (uiop:pathname-equal parent dir)
            ;; because parent of #p"/" is #p"/" which could result to infinite recursion
            (find-tags-directory% parent))))))

(defun find-tags-directory (file-or-dir)
  "Finds the #tags directory for given file or directory"
  (declare (type pathname file-or-dir))
  (find-tags-directory% (if (uiop:directory-pathname-p file-or-dir)
                            (uiop:pathname-parent-directory-pathname file-or-dir)
                            (uiop:pathname-directory-pathname file-or-dir))))

(defun make-relative-pathname (directories base)
  (merge-pathnames (make-pathname :directory (cons :relative directories))
                   base))

(defun link-pathname (file tags-directories)
  "Returns the pathname to the `link' file that needs to be created under a series of `tags' for the `file'
e.g. for file = 'abc.txt'; tags=#('p/a', 'm', 'n')
link-file should return '#tags/p/a/m/n"
  (declare (type pathname file)
           (type list tags-directories)
           (optimize (debug 3)))
  (let ((tag-directory (ensure-directories-exist
                        (make-relative-pathname tags-directories *tag-directory*))))
    (if (uiop:directory-pathname-p file)
        (make-pathname :name (car (last (pathname-directory file)))
                       :defaults *tag-directory*)
        (make-pathname :name (pathname-name file)
                       :type (pathname-type file)
                       :defaults tag-directory))))

(defun file-size (file)
  (osicat-posix:stat-size (osicat-posix:stat file)))

(defun .tag-file (tag-directory)
  (merge-pathnames "./.tags" tag-directory))

(defun relative-pathname (from to)
  "Relative pathname `from' to `to'.
Note: directories of `from' and `to' must exist, since `truename' is used to compute the relative pathnames"
  (let ((to-directory (pathname-directory (truename (uiop:pathname-directory-pathname to))))
        (from-directory (pathname-directory (truename (uiop:pathname-directory-pathname from)))))

    (loop for (dt . tailt) on to-directory
          for (df . tailf) on from-directory
          while (or (eql dt df)
                    (string= dt df))
          finally
             (return (make-pathname :defaults to
                                    :directory `(:relative ,@(make-list (1+ (length tailf)) :initial-element :up)
                                                           ,dt ,@tailt))))))
(defun create-link (link target &key (relative nil relative-provided?))
  (if (and relative-provided?
             (eql relative t))
      (osicat:make-link link :target (namestring (relative-pathname link target)))
      (osicat:make-link link :target target)))

(defun relativize-link (link)
  (let ((target (osicat:read-link link)))
    (unless (uiop:relative-pathname-p target)
      (delete-file link)
      (create-link link target :relative t))))
