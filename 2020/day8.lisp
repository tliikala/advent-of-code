
;;; https://github.com/tliikala/advent-of-code/2020/

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day8.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

;;; (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day8.txt")

(defvar *accumulator* 0)

(defclass instruction ()
  ((operation :initform nil :initarg :operation :accessor operation :documentation "acc jmp nop")
   (argument :initform nil :initarg :argument :accessor argument)))

(defmethod print-object ((object instruction) stream)
  (format stream "~A ~A" (operation object) (argument object)))

(defvar *instructions* nil)

(defun read-instructions ()
  (setq *instructions* nil)
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day8.txt")))
    (setq *instructions* (loop for x in lines
                               collect (make-instruction x)))))

(defvar *cross-operations* '(("acc" . acc)
                             ("jmp" . jmp)
                             ("nop" . nop)))

(defun get-init-keyw (txt)
  (cdr (assoc txt *cross-operations* :test #'string-equal)))

(defun make-instruction (txt)
  (let* ((i1 (find-regexp-in-string " " txt))
         (oper (subseq txt 0 i1))
         (sign (subseq txt (+ i1 1) (+ i1 2)))
         (value (subseq txt (+ i1 2))))
    (make-instance 'instruction
                   :operation (get-init-keyw oper)
                   :argument (* (if (string= sign "-") -1 1)
                                (parse-integer value)))))

;;; Part 1

(defun do-day8-part-1 ()
  (setq *accumulator* 0)
  (read-instructions)
  (day8-part-1 0 nil))

(defun day8-part-1 (pos thus-far)
  (let ((instruction (nth pos *instructions*)))
    (if (member instruction thus-far) *accumulator*
      (progn
        (case (operation instruction)
          (acc (incf *accumulator* (argument instruction)) (incf pos))
          (jmp (incf pos (argument instruction)))
          (nop (incf pos)))
        (push instruction thus-far)
        (day8-part-1 pos thus-far)))))

;;; (do-day8-part-1)

;;; Part 2

(defun do-day8-part-2 ()
  (read-instructions)
  (loop with end-pos = (length *instructions*)
        for x in *instructions*
        as result = nil
        do (setq *accumulator* 0)
        ;;;
        when (eql (operation x) 'jmp)
        do
        (setf (operation x) 'nop)
        (setq result (day8-part-2-ok-p 0 nil end-pos))
        (setf (operation x) 'jmp)
        if result do (return result)
        ;;;
        when (eql (operation x) 'nop)
        do
        (setf (operation x) 'jmp)
        (setq result (day8-part-2-ok-p 0 nil end-pos))
        (setf (operation x) 'nop)
        if result do (return result)))

(defun day8-part-2-ok-p (pos thus-far end-pos)
  (let ((instruction (nth pos *instructions*)))
    (if (= pos end-pos) *accumulator*
      (if (member instruction thus-far) nil
        (progn
          (case (operation instruction)
            (acc (incf *accumulator* (argument instruction)) (incf pos))
            (jmp (incf pos (argument instruction)))
            (nop (incf pos)))
          (push instruction thus-far)
          (day8-part-2-ok-p pos thus-far end-pos))))))

;;; (do-day8-part-2)
