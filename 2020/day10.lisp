
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass adapter ()
  ((joltage :initform nil :initarg :joltage :accessor joltage)))

(defmethod print-object ((object adapter) stream)
  (format stream "Adapter ~D" (joltage object)))

(defvar *adapters* nil)

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day10-test1.txt")))
    (setq *adapters* (loop for x in lines collect (make-instance 'adapter :joltage (parse-integer x))))))

(defun ok-next-adapters (adapters)

  ;(if (cdr adapters)

  ;(print adapters)

      (loop with here = (car adapters)
            for x on (cdr adapters)
            as diff = (- (joltage (car x)) (joltage here))
            when (< 0 diff 4)
            collect x
            until (> diff 3))

   ; (and adapters
    ;     (let ((diff (- (joltage (car adapters)) latest-joltage)))
     ;      (when (< 0 diff 4)
      ;       (list (car adapters)))))

    )


(defun go-through (adapters gone-through)
      



  (let ((ok-adapters (ok-next-adapters adapters)))
    


    
    (if ok-adapters (loop for x in ok-adapters
                          as tulos = (go-through x (cons (car adapters) gone-through))
                          when (when tulos ;(break (format nil "~A" gone-through))
                               ;  (print gone-through)
                                 t
                                    ;(+ 1 2)
                                    )
                          append tulos)
      (values (and (= (length gone-through) (length *adapters*)) (progn (break "ff") gone-through))))))
      



  
;  (if (= (length gone-through) (length *adapters*)) gone-through
 ;   (let ((adapters (ok-next-adapters joltage gone-through)))
  ;    (loop for a in adapters
   ;         append (go-through (joltage a) (cons a gone-through))))))

(defun day-10-part-1 ()
  
  ;(setq *adapters* (cons (make-instance 'adapter :joltage 0) (sort (read-input-data) #'< :key #'joltage)))

  (setq *adapters* (sort (read-input-data) #'< :key #'joltage))


  (let ((built-in-adapter (make-instance 'adapter :joltage (+ 3 (loop for x in *adapters* maximizing (joltage x))))))
    
    ;(setf (cdr (last *adapters*)) (list built-in-adapter))
  
  (let* (
         
         (result (go-through *adapters* nil))
         (joltages (and result (cons (joltage built-in-adapter) (mapcar #'joltage result)))))
    (count-differences-and-multiply joltages))))

(defun count-differences-and-multiply (joltages)
  (loop for x on joltages
        count (and (cadr x) (= (- (car x) (cadr x)) 3)) into diff3
        count (and (cadr x) (= (- (car x) (cadr x)) 1)) into diff1
        finally (return (* diff1 diff3))))

;;; (day-10-part-1)

;;; 2400 wrong too low