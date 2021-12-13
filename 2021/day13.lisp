
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let* ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day13.txt"))
         (data (loop for line in lines
                     until (zerop (length line))
                     as s = (lw:split-sequence '(#\,) line)
                     collect (cons (parse-integer (car s)) (parse-integer (cadr s)))))
         (folds (loop with here = nil
                      for line in lines
                      when here
                      collect (let* ((x (lw:split-sequence '(#\Space) line))
                                     (f (third x)))
                                (cons (if (char= (aref f 0) #\x) 'x 'y) (parse-integer (subseq f 2))))
                      when (zerop (length line))
                      do (setq here t))))
    (list data folds)))


(defun x-fold (dot fld dots)
  (setf (car dot) (- fld (- (car dot) fld)))
  (remove-duplicates dots :test #'(lambda (c1 c2) (and (= (car c1) (car c2)) (= (cdr c1) (cdr c2))))))

(defun y-fold (dot fld dots)
  (setf (cdr dot) (- fld (- (cdr dot) fld)))
  (remove-duplicates dots :test #'(lambda (c1 c2) (and (= (car c1) (car c2)) (= (cdr c1) (cdr c2))))))

(defun day-13-part-1 ()
  (let* ((inp (read-input-data))
         (data (car inp))
         (folds (list (caadr inp))))
    (loop for (dir . amount) in folds
          do (loop for dot in data
                   do (case dir
                        (x (when (> (car dot) amount) (setq data (x-fold dot amount data))))
                        (y (when (> (cdr dot) amount) (setq data (y-fold dot amount data)))))))
    (length data)))

;;; Part 2

(defun day-13-part-2 ()
  (let* ((inp (read-input-data))
         (data (car inp))
         (folds (cadr inp)))
    (loop for (dir . amount) in folds
          do (loop for dot in data
                   do (case dir
                        (x (when (> (car dot) amount) (setq data (x-fold dot amount data))))
                        (y (when (> (cdr dot) amount) (setq data (y-fold dot amount data)))))))
    (capi:display (make-instance 'folding-interf :data data))))

(capi:define-interface folding-interf ()
  ((data :initarg :data))
  (:panes
   (piirr‰
    capi:output-pane
    :display-callback 
    #'(lambda (pane x y w h)
        (declare (ignore x y w h))
        (gp:with-graphics-translation (pane 100 100)
          (gp:with-graphics-scale (pane 10 10)
            (dolist (dot data)
              (gp:draw-circle pane (car dot) (cdr dot) 0.1)))))))
  (:layouts
   (p‰‰1
    capi:column-layout
    '(piirr‰)))
  (:default-initargs
   :layout 'p‰‰1
   :auto-menus nil
   :title "Folding"
   :best-width 1000
   :best-height 400))
