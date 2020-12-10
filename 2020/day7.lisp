
;;; https://github.com/tliikala/advent-of-code/2020/

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day7.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun get-details (txt)
  (let* ((i1 (find-regexp-in-string " bags contain" txt))
         (colour (subseq txt 0 i1))
         (rest (subseq txt (+ i1 14))))
    (list* colour (unless (string-equal "no other bags." rest)
                    (get-details2 rest)))))

(defun get-details2 (txt)
  (lw:when-let (i-first-space (find-regexp-in-string " " txt))
    (list* (cons (parse-integer (subseq txt 0 (find-regexp-in-string " " txt)))
                 (subseq txt (1+ i-first-space) (find-regexp-in-string " bag" txt)))
           (lw:when-let (i-comma (find-regexp-in-string "," txt))
             (get-details2 (subseq txt (+ i-comma 2)))))))

(defvar *bags* nil)

(defun update-details ()
  (setq *bags* nil)
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day7.txt")))
    (dolist (line lines)
      (let ((details (get-details line)))
        (let ((actual-bag (find-bag (car details))))
          (loop for d in (cdr details)
                do (update-cont actual-bag d)))))))

(defclass bag ()
  ((colour :initform nil :initarg :colour :accessor colour)
   (cont :initform nil :initarg :cont :accessor cont
         :documentation "list of conses where car is a bag object and cdr how many of those bags this can carry")))

(defmethod initialize-instance :after ((self bag) &key)
  (push self *bags*))

(defmethod print-object ((object bag) stream)
  (format stream "BAG~A ~A" (if (zerop (length (cont object))) "(ends)" "")  (colour object)))

(defun find-bag (colour)
  (or (find colour *bags* :test #'string-equal :key #'colour)
      (make-instance 'bag :colour colour)))

(defmethod update-cont ((self bag) details-cons)
  (with-slots (cont) self
    (let ((new-bag (find-bag (cdr details-cons))))
      (push (cons new-bag (car details-cons)) cont))))

;;; Part 1

(defmethod bag-colour-test ((self bag) colour)
  (string-equal (colour self) colour))

(defmethod run-bag-test ((self bag) colour)
  (with-slots (cont) self
    (run-bag-test-helper cont colour)))

(defun run-bag-test-helper (det col)
  (when (car det)
    (let ((bag (caar det)))
      (or (bag-colour-test bag col)
          (run-bag-test-helper (cont bag) col)
          (run-bag-test-helper (cdr det) col)))))

(defun count-ba ()
  (update-details)
  (loop for bag in *bags*
        count (run-bag-test bag "shiny gold")))

;;; (count-ba)

;;; Part 2

(defmethod run-bag-count ((self bag))
  (with-slots (cont) self
    (1- (run-bag-count-helper cont))))

(defun run-bag-count-helper (det)
  (if (car det)
      (let ((bag (caar det)))
        (+ (* (cdar det) (run-bag-count-helper (cont bag)))
           (if (cdr det)
               (run-bag-count-helper (cdr det))
             1)
           ))
    1))

(defun count-bags-part-2 ()
  (update-details)
  (run-bag-count (find-bag "shiny gold")))

;;; (count-bags-part-2)
