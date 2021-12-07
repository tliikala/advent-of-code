
;;; https://github.com/tliikala/advent-of-code/2021/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun read-input-data ()
  (let* ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2021\\input-day4.txt"))
         (numbers (mapcar #'parse-integer (lw:split-sequence '(#\,) (car lines)))))
    (values
     numbers
     ;;; Boards
     (reverse
      (loop with board = nil and boards = nil
            for x in (cddr lines)
            as row = (mapcar #'(lambda (val) (cons (parse-integer val) nil))
                             (remove-if #'(lambda (x) (zerop (length x))) (lw:split-sequence '(#\Space) x)))
            if (plusp (length x)) do (push row board)
            else do (progn (push (reverse board) boards) (setq board nil))
            finally do (push (reverse board) boards) (return boards))))))

(defun populate-boards (number boards)
  (loop for b in boards
        do (loop for y in b
                 until (loop for x in y
                             until (and (= (car x) number) (setf (cdr x) t))))))

(defun check-boards (boards)
  (loop for board in boards
        when (or (check-board-horizontally board)
                 (check-board-horizontally (transpose-board board)))
        do (return board)))

(defun check-board-row-single (c)
  (cdr c))

(defun check-board-row (row)
  (every #'check-board-row-single row))

(defun check-board-horizontally (board)
  (loop for y in board
        when (check-board-row y)
        do (return t)))

(defun transpose-board (board)
  (loop for i upfrom 0 to (1- (length (car board)))
        collect (loop for y in board
                      collect (nth i y))))

(defun day-4-part-1 ()
  (multiple-value-bind (numbers boards) (read-input-data)
    (let ((info (loop with winning-board = nil
                      for n in numbers
                      for i upfrom 0
                      do (populate-boards n boards)
                      until (and (> i 3) (setq winning-board (check-boards boards)))
                      finally (return (cons n winning-board)))))
      (when (cdr info)
        (* (car info)
           (loop for y in (cdr info)
                 summing (loop for x in y
                               unless (cdr x)
                               summing (car x))))))))

;;; Part 2

(defun day-4-part-2 ()
  (multiple-value-bind (numbers boards) (read-input-data)
    (let* ((how-many-boards (length boards))
           (winning-boards (make-list how-many-boards :initial-element nil)))
      (let ((info (loop with last-board = nil
                        for n in numbers
                        for i upfrom 0
                        do (populate-boards n boards)
                        when (> i 3)
                        do
                        (progn
                          (loop for b in boards
                                for w on winning-boards
                                when (and (not (car w))
                                          (or (check-board-horizontally b)
                                              (check-board-horizontally (transpose-board b))))
                                do (progn (setf (car w) t)
                                     (setq last-board b)))
                          (when (= (count t winning-boards) how-many-boards)
                            (loop-finish)))
                        finally (return (cons n last-board)))))
        (when (cdr info)
          (* (car info)
             (loop for y in (cdr info)
                   summing (loop for x in y
                                 unless (cdr x)
                                 summing (car x)))))))))
