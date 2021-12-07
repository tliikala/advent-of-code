
;;; https://github.com/tliikala/advent-of-code/2021/

;(format nil "~B" 121212)

;(loop for x across "11101100101111100" for n downfrom (1- (length "11101100101111100")) summing (* (parse-integer (string x)) (expt 2 n)))

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day3.txt")))
    (loop for line in lines
          collect (loop for c across line
                        collect (if (char= c #\1) 1 0)))))

(defun binary-to-decimal (b)
  (loop for x in b
        for n downfrom (1- (length b))
        summing (* x (expt 2 n))))

(defun day-3-part-1 ()
  (let* ((data (read-input-data))
         (how-many (length data))
         (info (loop for i from 0 to (1- (length (car data)))
                     collect (loop for x in data
                                   count (plusp (nth i x)))))
         gamma epsilon)
    (setq gamma (mapcar #'(lambda (x) (if (> x (/ how-many 2)) 1 0)) info)
          epsilon (mapcar #'(lambda (x) (if (< x (/ how-many 2)) 1 0)) info))
    (* (binary-to-decimal gamma) (binary-to-decimal epsilon))))

;;; Part 2

(defun more (data)
  (let* ((more-ones (loop for x in data
                          count (plusp (car x)) into ones
                          count (zerop (car x)) into zeros
                          finally (return (>= ones zeros))))
         (next (if more-ones (loop for x in data
                                   when (plusp (car x))
                                   collect (cdr x))
                 (loop for x in data
                       when (zerop (car x))
                       collect (cdr x)))))
    (cons (if more-ones 1 0) (if (cadr next) (more next) (car next)))))

(defun less (data)
  (let* ((less-ones (loop for x in data
                          count (plusp (car x)) into ones
                          count (zerop (car x)) into zeros
                          finally (return (< ones zeros))))
         (next (if less-ones (loop for x in data
                                   when (plusp (car x))
                                   collect (cdr x))
                 (loop for x in data
                       when (zerop (car x))
                       collect (cdr x)))))
    (cons (if less-ones 1 0) (if (cadr next) (less next) (car next)))))

(defun day-3-part-2 ()
  (let* ((data (read-input-data)))
    (* (binary-to-decimal (more data)) (binary-to-decimal (less data)))))
