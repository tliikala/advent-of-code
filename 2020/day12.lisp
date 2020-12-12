
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass ship ()
  ((direction :initform 0 :accessor direction :documentation "radians")
   (east/west-pos :initform 0 :accessor east/west-pos :documentation "west negative, east postive")
   (north/south-pos :initform 0 :accessor north/south-pos :documentation "south negative, north postive")))

(defvar *ship* nil)

(defvar *commands* nil "conses '(command . amount)")

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day12.txt")))
    (setq *commands* (loop for str in lines
                           collect (cons (let ((command (subseq str 0 1)))
                                           (cond ((string= command "N") :n)
                                                 ((string= command "S") :s)
                                                 ((string= command "E") :e)
                                                 ((string= command "W") :w)
                                                 ((string= command "L") :l)
                                                 ((string= command "R") :r)
                                                 ((string= command "F") :f)))
                                         (parse-integer (subseq str 1)))))))

(defmethod nav ((self ship) command)
  (with-slots (north/south-pos east/west-pos) self
    (case (car command)
      (:n (incf north/south-pos (cdr command)))
      (:s (decf north/south-pos (cdr command)))
      (:e (incf east/west-pos (cdr command)))
      (:w (decf east/west-pos (cdr command)))
      (:l (turn self (cdr command)))
      (:r (turn self (- (cdr command))))
      (:f (move-forward self (cdr command))))))

(defmethod turn ((self ship) degrees)
  (with-slots (direction) self
    (incf direction (* (/ pi 180) degrees))))

(defmethod move-forward ((self ship) amount)
  (with-slots (direction) self
    (nav self (cons (let ((dir-sin (round (sin direction)))
                          (dir-cos (round (cos direction))))
                      (cond ((and (zerop dir-sin) (plusp dir-cos)) :e)
                            ((and (plusp dir-sin) (zerop dir-cos)) :n)
                            ((and (zerop dir-sin) (minusp dir-cos)) :w)
                            ((and (minusp dir-sin) (zerop dir-cos)) :s)
                            (t nil) ;;; ???
                            )) amount))))

(defmethod calc-manhattan-distance ((self ship))
  (with-slots (north/south-pos east/west-pos) self
    (+ (abs north/south-pos) (abs east/west-pos))))

(defun navigate ()
  (loop for c in *commands*
        do (nav *ship* c)))

;;; Part 1

(defun do-day-12-part-1 ()
  (read-input-data)
  (setq *ship* (make-instance 'ship))
  (navigate)
  (values
   (calc-manhattan-distance *ship*)
   *ship*))

;;; (do-day-12-part-1)
