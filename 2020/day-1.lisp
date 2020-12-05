
;;; https://github.com/tliikala/advent-of-code/2020/

;;; Well, here *input* is the input...

;;; 1. part
(defun tee1 ()
  (declare (special *input*))
  (loop for x in *input*
        as output = (loop for y in *input*
                          when (= (+ x y) 2020)
                          do (return (cons x y)))
        when output
        do (return (* (car output) (cdr output)))))

;;; 2. part
(defun tee2 ()
  (declare (special *input*))
  (apply #'* (loop for x1 in *input*
                   as output = (loop for x2 in *input*
                                     as output = (loop for x3 in *input*
                                                       when (= (+ x1 x2 x3) 2020)
                                                       do (return (list x1 x2 x3)))
                                     when output do (return output))
                   when output do (return output))))
