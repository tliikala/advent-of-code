
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass rule ()
  ((index :initform nil :initarg :index :accessor index)
   (and-rules-1 :initform nil :initarg :and-rules-1 :accessor and-rules-1)
   (and-rules-2 :initform nil :initarg :and-rules-2 :accessor and-rules-2)
   (final-cond :initform nil :initarg :final-cond :accessor final-cond)))

(defmethod print-object ((object rule) stream)
  (format stream "RULE ~D~A" (index object) (if (final-cond object) "*" "")))

(defvar *rules* nil)

(defun make-rule (line)
  (let* ((i1 (find-regexp-in-string ":" line))
         (ind (parse-integer (subseq line 0 i1))))
    (or
     ;;; FINAL-COND
     (lw:when-let (dq-pos (find-regexp-in-string "\"" line))
       (make-instance 'rule :index ind :final-cond (aref (subseq line dq-pos) 1)))
     ;;; MULTI-OR (2)
     (when (find-regexp-in-string "|" line)
       (setq line (subseq line (+ i1 2)))
       (let* ((i2 (find-regexp-in-string " " line))
              (m1 (parse-integer (subseq line 0 i2)))
              (i3 (find-regexp-in-string " | " line))
              (m2 (ignore-errors (parse-integer (subseq line (1+ i2) i3)))))
         (setq line (subseq line (+ i3 3)))
         (let* ((i4 (find-regexp-in-string " " line))
                (m3 (parse-integer (subseq line 0 i4)))
                (m4 (ignore-errors (parse-integer (subseq line (1+ i4))))))
           (make-instance 'rule :index ind :and-rules-1 (list* m1 (and m2 (list m2))) :and-rules-2 (list* m3 (and m4 (list m4)))))))
     ;;; MULTI-OR (1)
     (progn
       (setq line (subseq line (+ i1 2)))
       (let* ((i2 (find-regexp-in-string " " line))
              (m1 (parse-integer (subseq line 0 i2)))
              m2 m3)
         (when i2
           (setq line (subseq line (1+ i2)))
           (let ((i3 (find-regexp-in-string " " line)))
             (setq m2 (ignore-errors (parse-integer (subseq line 0 i3))))
             (when i3
               (setq line (subseq line (1+ i3)))
               (let ((i4 (find-regexp-in-string " " line)))
                 (setq m3 (parse-integer (subseq line 0 i4)))))))
         (make-instance 'rule :index ind :and-rules-1 (cons m1 (and m2 (cons m2 (and m3 (cons m3 nil)))))))))))

(defun read-rules ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day19-rules.txt")))
    (setq *rules* (loop for line in lines
                        as new-rule = (make-rule line)
                        when new-rule
                        collect new-rule))
    (connect-rules) ;;; !!!
    ))

(defun find-rule (index)
  (find index *rules* :key #'index :test #'=))

(defun connect-rules ()
  (loop for rule in *rules*
        do
        (setf (and-rules-1 rule) (loop for x in (and-rules-1 rule) collect (find-rule x))
              (and-rules-2 rule) (loop for x in (and-rules-2 rule) collect (find-rule x)))))

(defvar *no-excess-chars* nil)

(defun check-rule-matches (txt)
  (progv '(*no-excess-chars*) (list nil)
    (and (rule-matches (find-rule 0) txt 0) *no-excess-chars*)))

(defmethod rule-matches ((self rule) txt index)
  (with-slots (and-rules-1 and-rules-2 final-cond) self
    (if final-cond
        (when (char-equal (aref txt index) final-cond)
          (when (= (length txt) (1+ index))
            (setq *no-excess-chars* t))
          index)
      (or
       (loop with okp = t
             and i = index
             for rule in and-rules-1
             when okp
             do
             (setq okp (lw:when-let (new-i (rule-matches rule txt i))
                         (setq i (1+ new-i))))
             finally (return (and okp (1- i))))
       (when and-rules-2
         (loop with okp = t
               and i = index
               for rule in and-rules-2
               when okp
               do
               (setq okp (lw:when-let (new-i (rule-matches rule txt i))
                           (setq i (1+ new-i))))
               finally (return (and okp (1- i)))))
       ))))

(defun day19-part-1 ()
  (read-rules)
  (let ((input-lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day19-texts.txt")))
    (loop for input-line in input-lines
          counting (check-rule-matches input-line))))

;;; (day19-part-1)
