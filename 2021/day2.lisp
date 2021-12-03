
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day2.txt")))
    (loop for line in lines
          collect (lw:split-sequence '(#\Space) line))))

(defun day-2-part-1 ()
  (let ((movements (read-input-data)))
    (loop with h = 0 and d = 0
          for movement in movements
          do (cond ((equalp (car movement) "forward") (incf h (parse-integer (cadr movement))))
                   ((equalp (car movement) "up") (decf d (parse-integer (cadr movement))))
                   ((equalp (car movement) "down") (incf d (parse-integer (cadr movement)))))
          finally (return (* h d)))))

(defun day-2-part-2 ()
  (let ((movements (read-input-data)))
    (loop with h = 0 and d = 0 and aim = 0
          for movement in movements
          as x = (parse-integer (cadr movement))
          do (cond ((equalp (car movement) "forward")
                    (incf h x)
                    (incf d (* aim x)))
                   ((equalp (car movement) "up") (decf aim x))
                   ((equalp (car movement) "down") (incf aim x)))
          finally (return (* h d)))))
