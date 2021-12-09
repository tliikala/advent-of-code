
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day5.txt")))
    (loop for line in lines
          as split = (split-sequence '(#\, #\Space #\- #\>) line)
          as x1 = (first split) and y1 = (second split) and x2 = (sixth split) and y2 = (seventh split)
          collect (list (parse-integer x1) (parse-integer y1) (parse-integer x2) (parse-integer y2)))))

(defun day-5-part-1 ()
  (let* ((input (read-input-data))
         (ht (make-hash-table :test 'equal)))
    (loop for (x1 y1 x2 y2) in input
          when (or (= x1 x2) (= y1 y2))
          do (loop for x from (min x1 x2) to (max x1 x2)
                   do (loop for y from (min y1 y2) to (max y1 y2)
                            as k = (format nil "~D-~D" x y)
                            do (if (gethash k ht) (incf (gethash k ht))
                                 (setf (gethash k ht) 1)))))
    (loop for v being each hash-value of ht
          count (> v 1))))
