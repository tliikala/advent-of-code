
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let* ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day6.txt"))
         (data (car lines))
         (info (mapcar #'parse-integer (lw:split-sequence '(#\,) data))))
    info))

(defvar *lanternfish* nil)

(defclass lanternfish ()
  ((timer :initarg :timer)))

(defmethod initialize-instance :after ((self lanternfish) &key)
  (push self *lanternfish*))

(defmethod sim ((self lanternfish))
  (with-slots (timer) self
    (if (zerop timer)
        (progn (setq timer 6)
          (make-instance 'lanternfish :timer 8))
      (decf timer))))

(defun day-6-part-1 ()
  (let ((info (read-input-data)))
    (setq *lanternfish* (loop for x in info
                              collect (make-instance 'lanternfish :timer x))))
  (loop repeat 80
        do (loop for fish in *lanternfish*
                 do (sim fish)))
  (length *lanternfish*))

;;; Part 2

(defun day-6-part-2 ()
  (let ((info (read-input-data))
        (curr (list 0 0 0 0 0 0 0 0 0)))
    ;;; Initialize
    (loop for x in info
          do (incf (nth x curr)))
    ;;; Sim
    (loop repeat 256
          do (loop with new = (list 0 0 0 0 0 0 0 0 0)
                   for i downfrom 8 to 0
                   as amount = (nth i curr)
                   do
                   (if (zerop i) (progn (incf (nth 6 new) amount) (setf (nth 8 new) amount))
                     (setf (nth (1- i) new) amount))
                   finally do (setq curr new)))
    (apply #'+ curr)))
