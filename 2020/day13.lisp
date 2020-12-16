
;;; https://github.com/tliikala/advent-of-code/2020/

;;; Well, this time the input is in this file directly.

(defvar *buses* '(19 X X X X X X X X X X X X 37 X X X X X 883 X X X X X X X 23 X X X X 13 X X X 17 X X X X X X X X X X X X X 797 X X X X X X X X X 41 X X X X X X X X X X X X X X X X X X 29))

;;; Part 1

(defvar *earliest-depart* 1006605)

(defun get-non-x-buses ()
  (loop for x in *buses*
        when (numberp x)
        collect x))

(defun day-13-part-1 ()
  (let ((earliest (car (sort (mapcar #'(lambda (x) (cons (+ (- x (rem *earliest-depart* x)) *earliest-depart*) x)) (get-non-x-buses))
                             #'< :key #'car))))
    (* (- (car earliest) *earliest-depart*) (cdr earliest))))

;;; (day-13-part-1)

;;; Part 2

;;; Help: https://youtu.be/Pe6nZtTsfUM

(defun day-13-part-2 ()
  (loop with timestamp = 0 and lcm = (car *buses*)
        for x in (cdr *buses*)
        for i upfrom 1
        when (integerp x)
        do
        (loop while (not (zerop (rem (+ timestamp i) x)))
              do (incf timestamp lcm)
              finally do (setq lcm (* lcm x)))
        finally
        (return timestamp)))

;;; (day-13-part-2)
