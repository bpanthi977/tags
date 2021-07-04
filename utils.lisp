(in-package #:tags/utils)

(defun difference (vector1 vector2 &key key test)
  "Set difference between elements of vector1 and vector2; TODO: write efficient implementation"
  (set-difference (coerce vector1 'list)
		  (coerce vector2 'list)
		  :test test
		  :key key))

(defun equal-set (sequence1 sequence2 &key test)
  "Do sequence1 and sequence2 contain same elements? (order doesn't matter)"
  (and (= (length sequence1) (length sequence2))
       (every #'(lambda (el1)
                  (find el1 sequence2 :test test))
              sequence1)))

(defun join-vector (vector separator)
  (declare (type (vector string) vector)
	   (type character separator))
  (with-output-to-string (stream)
    (loop for i from 0
	  with lasti = (1- (length vector))
	  for str across vector do
	    (write-string str stream)
	    (unless (= i lasti)
	      (write-char separator stream)))))
