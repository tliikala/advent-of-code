
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass rule ()
  ((name :initform "" :initarg :name :accessor name)
   (range1 :initform nil :initarg :range1 :documentation "inclusive '(min . max)")
   (range2 :initform nil :initarg :range2 :documentation "inclusive '(min . max)")))

(defvar *rules* nil)

(defun read-rules ()
  (flet ((make-range-cons (input)
           (let ((i (find-regexp-in-string "-" input)))
             (cons (parse-integer (subseq input 0 i)) (parse-integer (subseq input (1+ i)))))))
    (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\day-16-input-rules.txt")))
      (setq *rules*
            (loop for line in lines
                  collect (let* ((i1 (find-regexp-in-string ":" line))
                                 (rule-nm (subseq line 0 i1))
                                 (i2 (find-regexp-in-string " or " line))
                                 (r1 (subseq line (+ i1 2) i2))
                                 (r2 (subseq line (+ 4 i2))))
                            (make-instance 'rule :name rule-nm :range1 (make-range-cons r1) :range2 (make-range-cons r2))))))))

(defclass ticket ()
  ((field-numbers :initform nil :initarg :field-numbers :accessor field-numbers)
   (valid-rules :initform nil :accessor valid-rules)
   (invalid-fields :initform nil :accessor invalid-fields)))

;;; Part 1

(defmethod process-ticket ((self ticket))
  (with-slots (field-numbers invalid-fields) self
    (loop for field in field-numbers
          unless (rule-valid-for-ticket self field)
          do (push field invalid-fields))))

(defmethod rule-valid-for-ticket ((self ticket) field)
  (with-slots (valid-rules) self
    (loop for rule in *rules*
          when (with-slots (range1 range2 name) rule
                 (when (or (num-in-rng field range1)
                           (num-in-rng field range2))
                   (if (member name valid-rules :test #'string-equal) nil
                     (push name valid-rules))))
          do (return t))))

(defun num-in-rng (num rng)
  (<= (car rng) num (cdr rng)))

(defun day-16-part-1 ()
  (read-rules)
  (let* ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\day-16-nearby-tickets.txt"))
         (tickets (loop for line in lines
                        collect (make-instance 'ticket :field-numbers (mapcar #'parse-integer (lw:split-sequence '(#\,) line))))))
    (loop for ticket in tickets
          do (process-ticket ticket))
    (loop for ticket in tickets
          summing (loop for invalid in (invalid-fields ticket) summing invalid))))

;;; (day-16-part-1)
