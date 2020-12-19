
;;; https://github.com/tliikala/advent-of-code/2020/

(defvar *input*)

(setq *input* '(0 13 16 17 1 10 6))

(defun get-input ()
  (loop for x in (reverse *input*)
        for turn downfrom (length *input*)
        collect (cons x turn)))

(defun replacing-numbers (i rounds &optional (max-i 2020))
  (if (> i max-i) (car (car rounds))
    (replacing-numbers (1+ i) (list* (cons (let* ((last-num (caar rounds))
                                                  (num-info (number-info last-num rounds)))
                                             (or num-info 0)) i)
                                     rounds)
                       max-i)))

(defun number-info (number rounds)
  (lw:when-let* ((pos1 (position number rounds :test #'= :key #'car))
                 (pos2 (position number (cdr (nthcdr pos1 rounds)) :test #'= :key #'car)))
    (- (cdr (nth pos1 rounds)) (cdr (nth pos2 (cdr (nthcdr pos1 rounds)))))))

;;; Part 1

(defun day-15-part-1 ()
  (let ((input (get-input)))
    (replacing-numbers (1+ (cdr (car input))) input)))

;;; (day-15-part-1)

;;; Part 2
;;; (Part 1 solution is too "heavy".)

#| Again, too "heavy"

(defvar *numbers* nil)

(defun get-from-numbers (turn)
  (aref *numbers* (1- turn)))

(defun set-to-numbers (value turn)
  (setf (aref *numbers* (1- turn)) value))

(defun day-15-part-2 (&optional (max-turn 30000000))
  ;;; Initializations
  (setq *numbers* (make-array max-turn :element-type 'integer))
  (loop for x in *input* for i from 0 do (setf (aref *numbers* i) x))
  ;;;
  (loop for turn from (1+ (length *input*)) to max-turn
        do (set-to-numbers (or (update-number-part-2 (get-from-numbers (1- turn)) turn) 0) turn)
        finally (return (get-from-numbers max-turn))))

;(setq *input* '(0 3 6))

(defun update-number-part-2 (number turn)
  (lw:when-let* ((trn1 (loop for trn downfrom (1- turn) to 1 when (= (get-from-numbers trn) number) do (return trn)))
                 (trn2 (and (> trn1 1) (loop for trn downfrom (1- trn1) to 1 when (= (get-from-numbers trn) number) do (return trn)))))
    (- trn1 trn2)))

|#

(defvar *spoken-numbers* (make-hash-table))

(defun set-index-to-numbers (last-turn-spoken number)
  (setf (gethash number *spoken-numbers*) last-turn-spoken))

(defun get-index-from-spoken-numbers (number)
  (gethash number *spoken-numbers*))

(defun day-15-part-2 (&optional (max-turn 30000000))
  (clrhash *spoken-numbers*)
  (loop for x in (butlast *input*) for i upfrom 1 do (set-index-to-numbers i x))
  (loop with latest-spoken-number = (car (last *input*)) and f-ind = nil and new-number = nil
        for turn from (1+ (length *input*)) to max-turn
        if (setq f-ind (get-index-from-spoken-numbers latest-spoken-number))
        do (setq new-number (- (1- turn) f-ind))
        else
        do (setq new-number 0)
        do
        (set-index-to-numbers (1- turn) latest-spoken-number)
        (setq latest-spoken-number new-number)
        finally (return latest-spoken-number)))

;;; (day-15-part-2) ;;; SBCL was used due to heap size limit of LispWorks Personal Edition.
