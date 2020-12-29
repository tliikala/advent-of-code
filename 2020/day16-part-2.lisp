
;;; https://github.com/tliikala/advent-of-code/2020/

;;; Doing this differently (when compared to the day's part 1).

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

(defmethod print-object ((object rule) stream)
  (format stream "~A" (name object)))

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
  ((fields :initform nil :initarg :fields :accessor fields :documentation "list of field objects")
   (invalid :initform nil :accessor invalid)))

(defmethod print-object ((object ticket) stream)
  (format stream "TICKET ~{~D ~}" (mapcar #'(lambda (f) (length (valid-rules f))) (fields object))))

(defclass field ()
  ((field-index :initform nil :initarg :field-index :accessor field-index)
   (field-number :initform nil :initarg :field-number :accessor field-number)
   (valid-rules :initform nil :initarg :valid-rules :accessor valid-rules)))

(defmethod print-object ((object field) stream)
  (format stream "F~D (~D)" (field-index object) (length (valid-rules object))))

(defun num-in-rng (num rng)
  (<= (car rng) num (cdr rng)))

(defmethod initialize-instance :after ((self ticket) &key field-numbers)
  (with-slots (fields invalid) self
    (loop for field-number in field-numbers
          for i upfrom 0
          as rule-valid-etc-result = (rule-valid-etc self field-number)
          if rule-valid-etc-result
          do (push (make-instance 'field :field-index i :field-number field-number :valid-rules rule-valid-etc-result) fields)
          else do
          (setq invalid t))
    (setq fields (reverse fields))))

(defmethod rule-valid-etc ((self ticket) field-number)
  (loop with valid-rules = nil
        for rule in *rules*
        do (with-slots (range1 range2) rule
             (when (or (num-in-rng field-number range1)
                       (num-in-rng field-number range2))
               (push rule valid-rules)))
        finally do (return (reverse valid-rules))))

(defvar *my-ticket-info* "53,101,83,151,127,131,103,61,73,71,97,89,113,67,149,163,139,59,79,137") ;;; Input data!

(defclass column () ;;; of common fields
  ((index :initform nil :initarg :index :accessor index)
   (ok-rules :initform nil :initarg :ok-rules :accessor ok-rules)))

(defmethod no-ok-rules-left ((self column))
  (with-slots (ok-rules) self
    (null ok-rules)))

(defmethod print-object ((object column) stream)
  (format stream "Col ~D (~D)" (index object) (length (ok-rules object))))

(defun day-16-part-2 ()
  (let* ((rules (read-rules))
         (lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\day-16-nearby-tickets.txt"))
         (my-ticket (make-instance 'ticket :field-numbers (mapcar #'parse-integer (lw:split-sequence '(#\,) *my-ticket-info*))))
         (tickets (loop for line in lines
                        collect (make-instance 'ticket :field-numbers (mapcar #'parse-integer (lw:split-sequence '(#\,) line))))))
    (let ((ok-tickets (loop for ticket in tickets
                            unless (invalid ticket)
                            collect ticket)))
      ;;; Next making the columns.
      (let ((columns (loop for i upfrom 0 to (1- (length rules))
                           collect (make-instance 'column :index i :ok-rules rules))))
        ;;; Some fields of the tickets don't fulfill all the rules. So, then one can assume that that particular column doesn't represent that rule.
        (loop for ticket in ok-tickets
              do (loop for field in (fields ticket)
                       for i upfrom 0
                       as column = (nth i columns)
                       do (setf (ok-rules column) (remove (car (set-difference rules (valid-rules field))) (ok-rules column)))))
        ;;; Going through the columns one-by-one. Columns that have only one "ok-rule" are sure to represent the field in question.
        ;;;   Then that rule no longer needs to be considered.
        (let* ((sure-fields (loop with sure-fields = nil
                                  until (every #'no-ok-rules-left columns)
                                  do (loop with rule-to-remove = nil
                                           for column in columns
                                           until rule-to-remove
                                           when (= (length (ok-rules column)) 1)
                                           do
                                           (push (cons (index column) (car (ok-rules column))) sure-fields)
                                           (setq rule-to-remove (car (ok-rules column)))
                                           finally do (loop for column in columns
                                                            do (setf (ok-rules column) (remove rule-to-remove (ok-rules column)))))
                                  finally (return sure-fields)))
               (departure-info (loop for c in sure-fields
                                     when (and (> (length (name (cdr c))) 9) (string-equal (subseq (name (cdr c)) 0 9) "departure"))
                                     collect c)))
          (apply #'* (loop for c in departure-info collect (field-number (nth (car c) (fields my-ticket))))))))))

;;; (day-16-part-2)
