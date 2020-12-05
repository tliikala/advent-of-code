
;;; https://github.com/tliikala/advent-of-code/2020/

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day5\\input.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defvar *txt-lines* (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day5\\input.txt"))

;;; (get-row-from-txt "FFFBFFFRRL" 0 127)

(defun get-row-from-txt (txt begin end)
  (if (= begin end) begin
    (if (char-equal (aref txt 0) #\F) (get-row-from-txt (subseq txt 1) begin (- end (/ (1+ (- end begin)) 2)))
      (get-row-from-txt (subseq txt 1) (+ begin (/ (1+ (- end begin)) 2)) end))))

;;; (get-column-from-txt "RLR" 0 7)

(defun get-column-from-txt (txt begin end)
  (if (= begin end) begin
    (if (char-equal (aref txt 0) #\L) (get-column-from-txt (subseq txt 1) begin (- end (/ (1+ (- end begin)) 2)))
      (get-column-from-txt (subseq txt 1) (+ begin (/ (1+ (- end begin)) 2)) end))))

(defun get-seat-id (txt)
  (let ((row (get-row-from-txt txt 0 127))
        (column (get-column-from-txt (subseq txt 7) 0 7)))
    (values
     (+ (* row 8) column)
     row column)))

;;; Part 1

(defun highest-id (txt-lines)
  (loop for x in txt-lines
        maximize (get-seat-id x)))

;;; (highest-id *txt-lines*)

;;; Part 1

(defun my-seat (txt-lines)
  (let ((ids (sort (loop for x in txt-lines collect (get-seat-id x)) #'<)))
    (find-my-seat ids)))

(defun find-my-seat (ids)
  (when (cadr ids)
    (if (= (car ids) (- (cadr ids) 2)) (1+ (car ids))
      (find-my-seat (cdr ids)))))

;;; (my-seat *txt-lines*)
