
;;; https://github.com/tliikala/advent-of-code/2020/

(defvar *cubes* nil)

(defclass cube ()
  ((x :initform nil :initarg :x :accessor x)
   (y :initform nil :initarg :y :accessor y)
   (z :initform nil :initarg :z :accessor z)
   (state :initform 'inactive :initarg :state :accessor state)
   (new-state :initform nil :initarg :new-state :accessor new-state)
   (neighbours :initform nil :accessor neighbours)))

(defmethod initialize-instance :after ((self cube) &key)
  (push self *cubes*))

(defmethod print-object ((object cube) stream)
  (with-slots (x y z state) object
    (format stream "~D ~D ~D ~C" x y z (if (eql state 'active) #\# #\.))))

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defun init-cubes ()
  (setq *cubes* nil)
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day17.txt")))
    (loop for line in lines
          for xi upfrom 0
          do (loop for s across line
                   for yi upfrom 0
                   do (make-instance 'cube :x xi :y yi :z 0 :state (if (char= s #\#) 'active 'inactive))))))

(defmethod find-neighbour ((self cube) x y z)
  (with-slots (neighbours) self
    (find-if #'(lambda (n) (and (= (x n) x) (= (y n) y) (= (z n) z))) neighbours)))

(defun find-cube (x y z)
  (find-if #'(lambda (n) (and (= (x n) x) (= (y n) y) (= (z n) z))) *cubes*))

(defmethod active-neighbours ((self cube))
  (with-slots (neighbours) self
    (count-if #'(lambda (c) (eql (state c) 'active)) neighbours)))

(defun active-cubes ()
  (count-if #'(lambda (c) (eql (state c) 'active)) *cubes*))

(defmethod prepare-neighbours ((self cube) &key (add-new-layer nil))
  (with-slots (x y z neighbours) self
    (loop for xi from (1- x) to (1+ x)
          do (loop for yi from (1- y) to (1+ y)
                   do (loop for zi from (1- z) to (1+ z)
                            unless (and (= xi x) (= yi y) (= zi z))
                            do (unless (find-neighbour self xi yi zi)
                                 (lw:when-let (cube (or (find-cube xi yi zi)
                                                        (when add-new-layer
                                                          (make-instance 'cube :x xi :y yi :z zi))))
                                   (pushnew cube neighbours)
                                   (pushnew self (neighbours cube))
                                   )))))))

(defun simulate-cycle ()
  (loop for cube in *cubes*
        as how-many-active-neighbours = (active-neighbours cube)
        if (eql (state cube) 'active)
        do (setf (new-state cube) (if (<= 2 how-many-active-neighbours 3)
                                      'active
                                    'inactive))
        else
        do (setf (new-state cube) (if (= how-many-active-neighbours 3)
                                      'active
                                    'inactive)))
  (loop for cube in *cubes*
        do (setf (state cube) (new-state cube))))

(defun day-17-part-1 (&optional (no-of-cycles 6))
  (init-cubes)
  (loop for cube in *cubes*
        do (prepare-neighbours cube :add-new-layer nil))
  (loop repeat no-of-cycles
        do
        (loop for cube in *cubes* do (prepare-neighbours cube :add-new-layer t))
        (simulate-cycle))
  (active-cubes))

;;; (day-17-part-1)
