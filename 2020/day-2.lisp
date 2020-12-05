
;;; https://github.com/tliikala/advent-of-code/2020/

;;; (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day2\\input.txt")

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defvar *input* (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day2\\input.txt"))

;;; PART 1

(defun test-line-part-1 (str)
  (let* ((i-of-hyphen (find-regexp-in-string "[-]" str))
         (i-of-space (find-regexp-in-string "[ ]" str))
         (i-of-colon (find-regexp-in-string "[:]" str))
         (min-amount (parse-integer (subseq str 0 i-of-hyphen)))
         (max-amount (parse-integer (subseq str (1+ i-of-hyphen) i-of-space)))
         (char-to-count (aref (subseq str (1+ i-of-space) (+ i-of-space 2)) 0))
         (text (subseq str (+ i-of-colon 2))))
    (<= min-amount (loop for c across text count (char= c char-to-count)) max-amount)))

(defun suorita-part-1 ()
  (loop for x in (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day2\\input.txt")
        count (test-line-part-1 x)))

;;; PART 2

(defun test-line-part-2 (str)
  (let* ((i-of-hyphen (find-regexp-in-string "[-]" str))
         (i-of-space (find-regexp-in-string "[ ]" str))
         (i-of-colon (find-regexp-in-string "[:]" str))
         (match-pos (parse-integer (subseq str 0 i-of-hyphen)))
         (non-match-pos (parse-integer (subseq str (1+ i-of-hyphen) i-of-space)))
         (char-to-examine (aref (subseq str (1+ i-of-space) (+ i-of-space 2)) 0))
         (text (subseq str (+ i-of-colon 2))))
    (= 1 (+ (if (char= (aref text (1- match-pos)) char-to-examine) 1 0)
            (if (char= (aref text (1- non-match-pos)) char-to-examine) 1 0)))))

(defun suorita-part-2 ()
  (loop for x in (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day2\\input.txt")
        count (test-line-part-2 x)))
