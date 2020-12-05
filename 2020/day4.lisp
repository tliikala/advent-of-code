
;;; https://github.com/tliikala/advent-of-code/2020/

#|
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
|#

(defclass passport ()
  ((byr :initform nil :initarg :byr :documentation "birth year")
   (iyr :initform nil :initarg :iyr :documentation "issue year")
   (eyr :initform nil :initarg :eyr :documentation "expiration year")
   (hgt :initform nil :initarg :hgt :documentation "height")
   (hcl :initform nil :initarg :hcl :documentation "hair color")
   (ecl :initform nil :initarg :ecl :documentation "eye color")
   (pid :initform nil :initarg :pid :documentation "passport id")
   (cid :initform nil :initarg :cid :documentation "country id")))

(defmethod passport-ok-p ((self passport))
  (with-slots (byr iyr eyr hgt hcl ecl pid) self
    (and byr iyr eyr hgt hcl ecl pid)))

(defvar *cross-connections* '(("byr" . :byr)
                              ("iyr" . :iyr)
                              ("eyr" . :eyr)
                              ("hgt" . :hgt)
                              ("hcl" . :hcl)
                              ("ecl" . :ecl)
                              ("pid" . :pid)
                              ("cid" . :cid)))

(defun get-init-keyw (txt)
  (cdr (assoc txt *cross-connections* :test #'string-equal)))

;;; "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent of code\\2020\\day4\\input.txt"

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun put-together (lines)
  (let ((tog (loop with together = nil
                   with current = nil
                   for x in lines
                   if (zerop (length x)) do (push current together) (setq current nil)
                   else do (push x current)
                   finally do (push current together) (return together))))
    (loop for x in tog
          collect (format nil "~{~A~^ ~}" x))))

(defun get-details (txt)
  (let* ((i-of-colon (find-regexp-in-string "[:]" txt))
         (i-of-space (find-regexp-in-string "[ ]" txt))
         (what (subseq txt 0 i-of-colon))
         (value (subseq txt (1+ i-of-colon) i-of-space))
         (rest (and i-of-space (subseq txt (1+ i-of-space)))))
    (cons (get-init-keyw what) (cons value (and (plusp (length rest)) (get-details rest))))))

(defun make-passports (&optional (path "C:\\Users\\tliik\\Documents\\Ohjelmointi\\AoC\\2020\\day4\\input.txt"))
  (let* ((lines (read-lines-from-file path))
         (tog (put-together lines)))
    (loop for x in tog
          collect (make-a-passport (get-details x)))))

(defun make-a-passport (details)
  (apply #'make-instance 'passport details))

;;; Part 1

(defun count-ok-passports (passports)
  (count-if #'passport-ok-p passports))

;;; (count-ok-passports (make-passports))

;;; Part 2

#|
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
|#

(defmethod passport-ok-strict-p ((self passport))
  (with-slots (byr iyr eyr hgt hcl ecl pid) self
    (and
     ;;; Birth year
     (lw:when-let (y (parse-integer byr :junk-allowed t))
       (<= 1920 y 2002))
     ;;; Issue year
     (lw:when-let (y (parse-integer iyr :junk-allowed t))
       (<= 2010 y 2020))
     ;;; Expiration year
     (lw:when-let (y (parse-integer eyr :junk-allowed t))
       (<= 2020 y 2030))
     ;;; Height
     (lw:when-let (i (or (find-regexp-in-string "[cm]" hgt) (find-regexp-in-string "[in]" hgt)))
       (let ((len (subseq hgt 0 i))
             (un (subseq hgt i)))
         (when (setq len (parse-integer len :junk-allowed t))
           (cond ((string-equal un "cm") (<= 150 len 193))
                 ((string-equal un "in") (<= 59 len 76))))))
     ;;; Hair color
     (when (= (length hcl) 7)
       (and (char= (aref hcl 0) #\#)
            (loop for c across (subseq hcl 1)
                  always (find c "0123456789abcdef"))))
     ;;; Eye color
     (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
     ;;; Passport id
     (when (= (length pid) 9)
       (parse-integer pid :junk-allowed t)))))

(defun count-ok-passports-strict (passports)
  (count-if #'(lambda (pp) (and (passport-ok-p pp) (passport-ok-strict-p pp))) passports))

;;; (count-ok-passports-strict (make-passports))
