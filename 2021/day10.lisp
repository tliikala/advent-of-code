
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day10.txt")))
    lines))

(defun day-10-part-1 ()
  (let ((results (loop with lines = (read-input-data)
                       for line in lines
                       collect (loop with latest = nil
                                     for c across line
                                     do (cond ((char= c #\() (push c latest))
                                              ((char= c #\[) (push c latest))
                                              ((char= c #\{) (push c latest))
                                              ((char= c #\<) (push c latest))
                                              ((char= c #\)) (if (equal (car latest) #\() (pop latest)
                                                               (return #\))))
                                              ((char= c #\]) (if (equal (car latest) #\[) (pop latest)
                                                               (return #\])))
                                              ((char= c #\}) (if (equal (car latest) #\{) (pop latest)
                                                               (return #\})))
                                              ((char= c #\>) (if (equal (car latest) #\<) (pop latest)
                                                               (return #\>)))
                                              )))))
    (loop for c in (remove nil results)
          summing (cond ((char= c #\)) 3)
                        ((char= c #\]) 57)
                        ((char= c #\}) 1197)
                        ((char= c #\>) 25137)))))

;;; Part 2

(defun day-10-part-2 ()
  (let* ((info (remove nil (loop with lines = (read-input-data)
                                 for line in lines
                                 collect (loop with latest = nil
                                               for c across line
                                               do (cond ((char= c #\() (push c latest))
                                                        ((char= c #\[) (push c latest))
                                                        ((char= c #\{) (push c latest))
                                                        ((char= c #\<) (push c latest))
                                                        ((char= c #\)) (if (equal (car latest) #\() (pop latest)
                                                                         (return nil)))
                                                        ((char= c #\]) (if (equal (car latest) #\[) (pop latest)
                                                                         (return nil)))
                                                        ((char= c #\}) (if (equal (car latest) #\{) (pop latest)
                                                                         (return nil)))
                                                        ((char= c #\>) (if (equal (car latest) #\<) (pop latest)
                                                                         (return nil))))
                                               finally (return (if (null latest) nil
                                                                 latest))))))
         (info2 (loop for cs in info
                      collect (loop for c in cs
                                    collect (cond ((char= c #\() #\))
                                                  ((char= c #\[) #\])
                                                  ((char= c #\{) #\})
                                                  ((char= c #\<) #\>)))))
         (info3 (loop for cs in info2
                      collect (loop with score = 0
                                    for c in cs
                                    do (setq score (* 5 score))
                                    do (incf score (cond ((char= c #\)) 1)
                                                         ((char= c #\]) 2)
                                                         ((char= c #\}) 3)
                                                         ((char= c #\>) 4)))
                                    finally (return score)))))
    (nth (floor (/ (length info3) 2)) (sort info3 #'<))))
