
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass adapter ()
  ((joltage :initform nil :initarg :joltage :accessor joltage)))

(defmethod print-object ((object adapter) stream)
  (format stream "Adapter ~D" (joltage object)))

(defvar *adapters* nil)

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day10.txt")))
    (setq *adapters* (loop for x in lines collect (make-instance 'adapter :joltage (parse-integer x))))))

;;; Part 1

(defun count-differences-and-multiply (joltages)
  (loop for x on joltages
        count (and (cadr x) (= (- (car x) (cadr x)) 3)) into diff3
        count (and (cadr x) (= (- (car x) (cadr x)) 1)) into diff1
        finally (return (* diff1 diff3))))

(defun day-10-part-1 ()
  (setq *adapters* (cons (make-instance 'adapter :joltage 0) (sort (read-input-data) #'< :key #'joltage)))
  (let* ((result (go-through *adapters* nil))
         (joltages (and result (cons (+ 3 (joltage (car result))) (mapcar #'joltage result)))))
    (count-differences-and-multiply joltages)))

(defmethod ok-next-adapters ((self adapter) rest-adapters)
  (loop for x on rest-adapters
        as diff = (- (joltage (car x)) (joltage self))
        when (< 0 diff 4)
        collect x
        until (> diff 3)))

(defun go-through2 (choices curr gone-through)
  (or (go-through (car choices) (cons curr gone-through))
      (go-through2 (cdr choices) curr gone-through)))

(defun go-through (adapters gone-through)
  (if adapters
      (lw:when-let (choices (if (cdr adapters) (ok-next-adapters (car adapters) (cdr adapters))
                              ;;; Would the final adapter be ok?
                              (let ((c (car adapters)) (p (car gone-through)))
                                (and (< 0 (- (joltage c) (joltage p)) 4) (list (list c))))))
        (go-through2 choices (car adapters) gone-through))
    gone-through))

;;; (day-10-part-1)
