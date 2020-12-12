
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass ship ()
  ((direction :initform 0 :accessor direction :documentation "radians") ;;; Not needed in part 2
   (east/west-pos :initform 0 :initarg :east/west-pos :accessor east/west-pos :documentation "west negative, east positive")
   (north/south-pos :initform 0 :initarg :north/south-pos :accessor north/south-pos :documentation "south negative, north positive")))

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

(defmethod calc-manhattan-distance-from-origin ((self ship))
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
   (calc-manhattan-distance-from-origin *ship*)
   *ship*))

;;; (do-day-12-part-1)

;;; Part 2

(defvar *waypoint* nil)

(defmethod rotate-around-ship ((self ship) (wayp ship) degrees)
  (let ((center-x (east/west-pos self))
        (center-y (north/south-pos self))
        (surr-x (east/west-pos wayp))
        (surr-y (north/south-pos wayp))
        (rotation (* (/ pi 180) degrees)))
    ;;; https://stackoverflow.com/questions/2259476/rotating-a-point-about-another-point-2d
    (setf (east/west-pos wayp) (round (+ (- (* (cos rotation) (- surr-x center-x)) (* (sin rotation) (- surr-y center-y))) center-x))    ;;; new x
          (north/south-pos wayp) (round (+ (+ (* (sin rotation) (- surr-x center-x)) (* (cos rotation) (- surr-y center-y))) center-y))) ;;; new y
    ))

(defmethod move-part-2 ((self ship) (wayp ship) gain)
  (let (distx going-east disty going-north)
    (multiple-value-bind (dist to-east-p) (calc-manhattan-distance-east/west self wayp)
      (unless (zerop dist)
        (setq distx dist going-east to-east-p)
        (nav self (cons (if to-east-p :e :w) (* gain dist)))))
    (multiple-value-bind (dist to-north-p) (calc-manhattan-distance-north/south self wayp)
      (unless (zerop dist)
        (setq disty dist going-north to-north-p)
        (nav self (cons (if to-north-p :n :s) (* gain dist)))))
    (and distx (nav wayp (cons (if going-east :e :w) (* gain distx))))
    (and disty (nav wayp (cons (if going-north :n :s) (* gain disty))))))

(defmethod calc-manhattan-distance-east/west ((self ship) (wayp ship))
  (let ((ship-x (east/west-pos self))
        (wayp-x (east/west-pos wayp)))
    (values
     (abs (- ship-x wayp-x))
     (> wayp-x ship-x)) ;;; waypoint to the east
    ))

(defmethod calc-manhattan-distance-north/south ((self ship) (wayp ship))
  (let ((ship-y (north/south-pos self))
        (wayp-y (north/south-pos wayp)))
    (values
     (abs (- ship-y wayp-y))
     (> wayp-y ship-y))  ;;; waypoint to the north
    ))

(defun navigate-part-2 ()
  (loop for c in *commands*
        do (nav-part-2 *ship* *waypoint* c)))

(defmethod nav-part-2 ((self ship) (wayp ship) command)
  (case (car command)
    (:n (nav wayp command))
    (:s (nav wayp command))
    (:e (nav wayp command))
    (:w (nav wayp command))
    (:l (rotate-around-ship self wayp (cdr command)))
    (:r (rotate-around-ship self wayp (- (cdr command))))
    (:f (move-part-2 self wayp (cdr command)))))

(defun do-day-12-part-2 ()
  (read-input-data)
  (setq *ship* (make-instance 'ship) *waypoint* (make-instance 'ship :east/west-pos 10 :north/south-pos 1))
  (navigate-part-2)
  (values
   (calc-manhattan-distance-from-origin *ship*)
   *ship* *waypoint*))

;;; (do-day-12-part-2)
