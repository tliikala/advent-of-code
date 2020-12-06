
;;; https://github.com/tliikala/advent-of-code/2020/

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day6.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun put-together2 (lines)
  (let ((tog (loop with together = nil
                   with current = nil
                   for x in lines
                   if (zerop (length x)) do (push current together) (setq current nil)
                   else do (push x current)
                   finally do (push current together) (return together))))
    (loop for x in tog
          collect (format nil "~{~A~}" x))))

;;; (put-together2 (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day6.txt"))

;;; Part 1

(defun day6-part1 ()
  (let ((groups (put-together2 (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day6.txt"))))
    (loop for g in groups
          summing (length (remove-duplicates g :test #'char-equal)))))

;;; (day6-part1)

;;; Part 2

(defun put-together3 (lines)
  (let ((tog (loop with together = nil
                   with current = nil
                   for x in lines
                   if (zerop (length x)) do (push current together) (setq current nil)
                   else do (push (remove-duplicates x :test #'char-equal) current) ;;; Remove-duplicates!
                   finally do (push current together) (return together))))
    tog))

(defun day6-part2-helper (input-list)
  (let ((how-many-times-needed (length input-list))
        (how-many-times (loop with char-info = nil
                              for c across (format nil "~{~A~}" input-list)
                              if (getf char-info c)
                              do (incf (getf char-info c))
                              else
                              do (setf (getf char-info c) 1)
                              finally (return char-info))))
    (labels ((remove-not-ok (input-plist)
               (when (cdr input-plist) ;;; Need to be CDR here.
                 (if (= (cadr input-plist) how-many-times-needed)
                     (list* (car input-plist) (cadr input-plist) (remove-not-ok (cddr input-plist)))
                   (remove-not-ok (cddr input-plist))))))
      (remove-not-ok how-many-times))))

(defun day6-part2 ()
  (let ((groups (put-together3 (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day6.txt"))))
    (loop for g in groups
          summing (/ (length (day6-part2-helper g)) 2))))

;;; (day6-part2)
