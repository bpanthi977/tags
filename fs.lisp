;;;; file system traversing and other utilites

(in-package :tags)

;;; finds $tags directory

(defun find-tags-directory% (dir)
  (declare (type pathname dir))
  (let* ((dir (truename dir))
         ($tags-directory (uiop:directory-exists-p (merge-pathnames #p"./$tags" dir))))
    (if $tags-directory
        $tags-directory
        (let ((parent (uiop:pathname-parent-directory-pathname dir)))
          (unless (uiop:pathname-equal parent dir)
            ;; because parent of #p"/" is #p"/" which could result to infinite recursion
            (find-tags-directory% parent))))))

(defun find-tags-directory (file-or-dir)
  "Finds the $tags directory for given file or directory"
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
link-file should return '$tags/p/a/m/n"
  (declare (type pathname file)
           (type list tags-directories)
           (optimize (debug 3)))
  (let ((tag-directory (ensure-directories-exist
                        (make-relative-pathname tags-directories *$tag-directory*))))
    (if (uiop:directory-pathname-p file)
        (make-pathname :name (car (last (pathname-directory file)))
                       :defaults *$tag-directory*)
        (make-pathname :name (pathname-name file)
                       :type (pathname-type file)
                       :defaults tag-directory))))

(defun file-size (file)
  (osicat-posix:stat-size (osicat-posix:stat file)))

(defun .tag-file ($tag-directory)
  (merge-pathnames "./.tags" $tag-directory))

(defun create-link (link target &optional (relative nil relative-provided? ))
  (when (and relative-provided?
             (eql relative t))
    (print "creating relative links not supported yet"))
  (osicat:make-link link :target target))
