
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day1.txt")))
    (loop for x in lines
          collect (parse-integer x))))

(defun day-1-part-1 ()
  (let ((depths (read-input-data)))
    (loop for x1 in depths
          for x2 in (cdr depths)
          when (< x1 x2)
          count it)))

(defun day-1-part-2 ()
  (let ((depths (read-input-data)))
    (loop with prev = nil
          for a in depths
          for b in (cdr depths)
          for c in (cddr depths)
          as abc = (+ a b c)
          when prev
          count (< prev abc)
          do (setq prev abc))))
