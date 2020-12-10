
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

(defun ok-next-adapters (joltage &optional (not-these nil))
  (loop for x in *adapters*
        as diff = (- (joltage x) joltage)
        when (and (< 0 diff 4) (not (member x not-these)))
        collect x))

;;; (go-through 0 nil)

(defun go-through (joltage gone-through)
  (if (= (length gone-through) (length *adapters*)) gone-through
    (let ((adapters (ok-next-adapters joltage gone-through)))
      (loop for a in adapters
            append (go-through (joltage a) (cons a gone-through))))))

(defun day-10-part-1 ()
  (read-input-data)
  (let* ((result (go-through 0 nil))
         (joltages (cons (+ (joltage (car result)) 3) (mapcar #'joltage result))))
    (setf (cdr (last joltages)) (list 0))
    (count-differences-and-multiply joltages)))

(defun count-differences-and-multiply (joltages)
  (loop for x on joltages
        count (and (cadr x) (= (- (car x) (cadr x)) 3)) into diff3
        count (and (cadr x) (= (- (car x) (cadr x)) 1)) into diff1
        finally (return (* diff1 diff3))))

;;;(day-10-part-1)
