
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defvar *input* nil)

(defvar *mem* (make-hash-table))

(defun collect-input ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day14.txt")))
    (setq *input* (loop for line in lines
                        collect (let ((how (subseq line 0 2)))
                                  (if (string-equal how "ma")
                                      (cons 'mask (subseq line 7))
                                    (cons 'mem (let* ((i1 (find-regexp-in-string "\\[" line))
                                                      (i2 (find-regexp-in-string "\\]" line))
                                                      (what (subseq line (1+ i1) i2))
                                                      (addr (parse-integer what))
                                                      (i3 (find-regexp-in-string "=" line))
                                                      (val (subseq line (+ 1 i3)))
                                                      (value (parse-integer val)))
                                                 (cons addr value)))))))))

(defun to-mem (index value)
  (setf (gethash index *mem*) value))

(defun sum-of-mem ()
  (loop for x being the hash-values of *mem*
        summing x))

(defun use-mask (value mask)
  (let* ((value2 (format nil "~36,'0B" value))
         (new-value (loop for x across value2
                          for m across mask
                          if (char-equal m #\0) collect #\0
                          else if (char-equal m #\1) collect #\1
                          else collect x)))
    (parse-integer (format nil "~{~A~}" new-value) :radix 2)))

;;; Part 1

(defun day-14-part-1 ()
  (collect-input)
  (clrhash *mem*)
  (loop with mask = nil
        for c in *input*
        if (eql (car c) 'mask)
        do (setq mask (cdr c))
        else
        do (let ((new-val (if mask (use-mask (cddr c) mask) (cddr c))))
             (to-mem (cadr c) new-val)))
  (sum-of-mem))

;;; (day-14-part-1)

;;; Part 2

(defun use-mask-2-helper (value mask all)
  (cond ((endp value) all)
        ((char-equal (car mask) #\0) (use-mask-2-helper (cdr value) (cdr mask) (cons (car value) all)))
        ((char-equal (car mask) #\1) (use-mask-2-helper (cdr value) (cdr mask) (cons (car mask) all)))
        ((char-equal (car mask) #\X) (append (use-mask-2-helper (cdr value) (cdr mask) (cons #\0 all))
                                             (use-mask-2-helper (cdr value) (cdr mask) (cons #\1 all))))))

(defun use-mask-2 (value mask)
  (let ((helper (use-mask-2-helper (loop for c across (reverse (format nil "~36,'0B" value)) collect c)
                                   (loop for c across (reverse mask) collect c)
                                   nil)))
    (loop for x from 0 to (1- (length helper)) by 36
          collect (parse-integer (format nil "~{~D~}" (loop for xx upfrom x to (+ x 35)
                                                            collect (nth xx helper)))
                                 :radix 2))))

(defun day-14-part-2 ()
  (collect-input)
  (clrhash *mem*)
  (loop with mask = nil
        for c in *input*
        if (eql (car c) 'mask) do (setq mask (cdr c))
        else
        do (let ((addresses (if mask (use-mask-2 (cadr c) mask) (list (cadr c)))))
             (loop for addr in addresses
                   do (to-mem addr (cddr c)))))
  (sum-of-mem))

;;; (day-14-part-2)
