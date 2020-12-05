
;;; https://github.com/tliikala/advent-of-code/2020/

(defclass alue ()
  ((area :initform nil :initarg :area :accessor area)
   (height :initform nil :initarg :height :accessor height)
   (width :initform nil :initarg :width :accessor width)))

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun initialize-game-area (height width)
  (make-array (list height width)))

(defun set-value-in-area (area x y value)
  (setf (aref area x y) value))

(defmethod get-value-from-area ((self alue) x y)
  (with-slots (area width) self
    (aref area y (rem x width))))

(defvar *alue-here* nil)

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent of code\\2020\\day3\\test-input.txt"

;;; Well, there's definitely room for improvement here!

(defun initialize-area ()
  (let* ((path "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day3\\input.txt")
         (lines (read-lines-from-file path))
         (width (length (car lines)))
         (height (length lines)))
    (setq *alue-here* (make-instance 'alue :area (initialize-game-area height width) :height height :width width))
    (loop for y in lines for j from 0
          do (loop for x across y for i from 0
                   do (set-value-in-area (area *alue-here*) j i (if (char-equal x #\#) 'tree 'open-square))))
    *alue-here*))

;;; Part 1

(defmethod do-day-3 ((self alue))
  (with-slots (area height width) self
    (loop with w = 0
          for h from 1 to (1- height)
          do (setq w (rem (+ w 3) width))
          count (eql (get-value-from-area self w h) 'tree))))

;;; (initialize-area)
;;; (do-day-3 *alue-here*)

;;; Part 2

(defmethod do-day-3-part-2-helper ((self alue) &optional (movement-right 3) (movement-down 1))
  (with-slots (area height width) self
    (loop with w = 0
          for h from movement-down to (1- height) by movement-down
          do (setq w (rem (+ w movement-right) width))
          count (eql (get-value-from-area self w h) 'tree))))

(defmethod do-day-3-part-2 ((self alue))
  (* (do-day-3-part-2-helper self 1 1)
     (do-day-3-part-2-helper self 3 1)
     (do-day-3-part-2-helper self 5 1)
     (do-day-3-part-2-helper self 7 1)
     (do-day-3-part-2-helper self 1 2)))

;;; (do-day-3-part-2 *alue-here*)
