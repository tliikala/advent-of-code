
;;; https://github.com/tliikala/advent-of-code/2020/

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day9.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

;;; (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day9.txt")

(defvar *numbers* nil)

(defvar *preamble-length* 25)

(defun read-numbers ()
  (setq *numbers* nil)
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day9.txt")))
    (setq *numbers* (loop for x in lines collect (parse-integer x)))))

;;; Part 1

(defun test-number ()
  (read-numbers)
  (loop with nums = nil
        for x in *numbers*
        for i upfrom 0
        if (< i *preamble-length*)
        do
        (if nums (setf (cdr (last nums)) (list x)) (setq nums (list x)))
        else
        do
        (unless (test-number2 x nums) (return x))
        (setq nums (cdr nums))
        (setf (cdr (last nums)) (list x))))

(defun test-number2 (needed nums)
  (when (cdr nums)
    (or (loop with x1 = (car nums)
              for x2 in (cdr nums)
              when (= (+ x1 x2) needed)
              do (return (cons x1 x2)))
        (test-number2 needed (cdr nums)))))

;;; (test-number)

;;; Part 2

;;; 507622668

(defun test-number-part-2 (needed nums)
  (when (cdr nums)
    (let ((res (loop with x1 = (car nums)
                     with end = nil
                     with sums = x1
                     with numne = (list x1)
                     for x2 in (cdr nums)
                     until end
                     do (push x2 numne)
                     when (prog1 (setq end (= (incf sums x2) needed))
                            (if (> sums needed) (setq end t)))
                     append numne)))
      (if res (cons res (test-number-part-2 needed (cdr nums)))
        (test-number-part-2 needed (cdr nums))))))

(defun do-day-9-part-2 (needed)
  (read-numbers)
  (lw:when-let (result (car (test-number-part-2 needed *numbers*))) ;;; Only one list is assumed to be in the result.
    (+ (apply #'min result) (apply #'max result))))

;;; (do-day-9-part-2 507622668)
