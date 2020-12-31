
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun extract (txt)
  (when (plusp (length txt))
    (let ((x (aref txt 0)))
      (let ((out1 (cond ((char= x #\Space) (extract (subseq txt 1)))
                        ((char= x #\() (cons 'open-bracket (subseq txt 1)))
                        ((char= x #\)) (cons 'close-bracket (subseq txt 1)))
                        ((char= x #\+) (cons 'plus (subseq txt 1)))
                        ((char= x #\*) (cons 'multiply (subseq txt 1)))
                        (t (let ((end-i (position-if #'(lambda (c) (not (alphanumericp c))) txt)))
                             (cons (parse-integer (subseq txt 0 end-i)) (and end-i (subseq txt end-i))))))))
        out1))))

(defun calc (txt value operator)
  (if (or (not txt) (zerop (length txt))) value
    (let ((ext (extract txt)))
      (when (eql (car ext) 'open-bracket)
        (setq ext (calc (cdr ext) 0 nil)))
      (cond 
       ((eql (car ext) 'close-bracket)
        (cons value (cdr ext)))
       ((integerp (car ext))
        (case operator
          (plus (incf value (car ext)))
          (multiply (setq value (* (car ext) value)))
          (otherwise (setq value (car ext))))
        (calc (cdr ext) value nil))
       ((member (car ext) '(plus minus multiply))
        (calc (cdr ext) value (car ext)))))))

;;; https://en.wikipedia.org/wiki/Operator-precedence_parser ("Alternative methods")
(defun modify (in out)
  (if (zerop (length in)) (format nil "~A))" out)
    (let ((x (aref in 0)))
      (when (zerop (length out)) (setq out "(("))
      (cond ((char= x #\() (modify (subseq in 1) (format nil "~A(((" out)))
            ((char= x #\)) (modify (subseq in 1) (format nil "~A)))" out)))
            ((char= x #\*) (modify (subseq in 1) (format nil "~A)*(" out)))
            (t (modify (subseq in 1) (format nil "~A~C" out x)))))))

(defun day18-part-2 ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day18.txt")))
    (loop for line in lines
          summing (calc (modify line "") 0 nil))))

;;; (day18-part-2)
