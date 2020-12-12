
;;; https://github.com/tliikala/advent-of-code/2020/

(defun read-lines-from-file (path)
  (with-open-file (stream path :direction :input)
    (loop with line = nil
          do (setq line (read-line stream nil))
          while line
          collect line)))

(defclass tile ()
  ((state :initform nil :initarg :state :accessor state :documentation "e empty o occupied f floor")
   (x :initform nil :initarg :x :accessor x)
   (y :initform nil :initarg :y :accessor y)
   (new-state :initform nil :initarg :new-state :accessor new-state)
   (latest-step-change-p :initform nil :accessor latest-step-change-p :documentation "state has changed the last step")
   (adjacents :initform nil :accessor adjacents :documentation "the surrounding tiles")
   (highlight-p :initform nil :accessor highlight-p :documentation "true after clicking a tile center to this")
   ))

(defmethod print-object ((object tile) stream)
  (format stream "~A" (case (state object) (e "L") (o "#") (f "."))))

(defvar *tiles* nil)

(defun read-input-data ()
  (let ((lines (read-lines-from-file "C:\\Users\\tliik\\Documents\\Ohjelmointi\\advent-of-code\\2020\\input-day11.txt")))
    (setq *tiles* (loop for str in lines
                        for y upfrom 0
                        collect (loop for c across str
                                      for x upfrom 0
                                      collect (make-instance 'tile
                                                             :state (cond ((char= c #\L) 'e)
                                                                          ((char= c #\#) 'o)
                                                                          ((char= c #\.) 'f))
                                                             :x x :y y))))
    (set-adjacents)
    (values *tiles*)))

(defun set-adjacents ()
  (loop for y in *tiles*
        for y-1 upfrom -1
        for y0 upfrom 0
        for y+1 upfrom 1
        do (loop for x in y
                 for x-1 upfrom -1
                 for x0 upfrom 0
                 for x+1 upfrom 1
                 as adjacents = (remove nil (list
                                             ;;; "Above"
                                             (ignore-errors (nth x-1 (nth y-1 *tiles*)))
                                             (ignore-errors (nth x0 (nth y-1 *tiles*)))
                                             (ignore-errors (nth x+1 (nth y-1 *tiles*)))
                                             ;;; "Same"
                                             (ignore-errors (nth x-1 (nth y0 *tiles*)))
                                             (ignore-errors (nth x+1 (nth y0 *tiles*)))
                                             ;;; "Below"
                                             (ignore-errors (nth x-1 (nth y+1 *tiles*)))
                                             (ignore-errors (nth x0 (nth y+1 *tiles*)))
                                             (ignore-errors (nth x+1 (nth y+1 *tiles*)))))
                 do (setf (adjacents x) adjacents))))

;;; Part 1

(capi:define-interface tiles-interf ()
  ((tiles :initform nil :accessor game-tiles)
   (stop-pushed :initform nil))
  (:panes
   (stop-p
    capi:push-button
    :text "Stop"
    :callback-type :none
    :selection-callback #'(lambda () (setq stop-pushed t))
    :enabled t))
  (:layouts
   (pää
    capi:pinboard-layout
    nil
    :draw-with-buffer t
    :fit-size-to-children nil
    :input-model '(((:button-1 :press) locate-tile)))
   (pää1
    capi:column-layout
    '(pää stop-p)))
  (:default-initargs
   :layout 'pää1
   :auto-menus nil
   :title "Tiles"
   :best-width 800
   :best-height 800
   ))

(defmethod initialize-instance :after ((self tiles-interf) &key)
  (with-slots (tiles pää) self
    (setq tiles (loop for row in (read-input-data) append row))
    (setf (capi:layout-description pää) (loop for tile in tiles
                                              collect (make-instance 'capi:drawn-pinboard-object
                                                                     :plist (list :tile tile)
                                                                     :x (+ (* 8 (x tile)) 5)
                                                                     :y (+ (* 8 (y tile)) 5)
                                                                     :visible-min-width 8
                                                                     :visible-min-height 8
                                                                     :display-callback 'draw-a-tile)))
    (mp:process-run-function "simulate" () #'simulate-tiles self)))

(defun locate-tile (pinboard-layout x y)
  (lw:when-let (tilepb (capi:pinboard-object-at-position pinboard-layout x y))
    (highlight-tile (capi:capi-object-property tilepb :tile) pinboard-layout)))

(defmethod highlight-tile ((self tile) pinboard-layout)
  ;;; Removes all highlighting
  (loop for tile in (game-tiles (capi:element-interface pinboard-layout))
        do (setf (highlight-p tile) nil))
  ;;; Highlights the adjacent tiles
  (dolist (tile (adjacents self))
    (setf (highlight-p tile) t))
  ;;; "Redraw"
  (gp:invalidate-rectangle pinboard-layout))

(defun draw-a-tile (output-pane self x y width height)
  (declare (ignore height))
  (gp:with-graphics-translation (output-pane 0 0)
    (let ((tile (capi:capi-object-property self :tile)))
      (gp:draw-circle output-pane x y (/ width 2)
                      :filled (if (highlight-p tile) t
                                (case (state tile) (e nil) (o t) (f t)))
                      :foreground (case (state tile) (e :white) (o :blue) (f :black))))))

(defmethod simulate-tiles ((self tiles-interf))
  (with-slots (tiles pää stop-pushed) self
    (sleep 2) ;;; A little wait
    (loop with step-changes-p = nil and end-sim = nil
          while (and (not end-sim) (not stop-pushed))
          do
          ;;; Simulate all the tiles.
          (loop for tile in tiles
                do (simulate-tile tile))
          ;;; Has some state changed?
          (setq step-changes-p (some #'latest-step-change-p tiles))
          ;;; Update new-state values to states.
          (loop for tile in tiles
                do (update-tile tile))
          ;;; Redraw
          (when step-changes-p
            (capi:apply-in-pane-process pää #'(lambda (pbl)
                                                ;(loop for pbo in (capi:layout-description pbl)
                                                ;      do (capi:redraw-pinboard-object pbo))
                                                (capi:redraw-pinboard-layout pbl 0 0 800 800)) pää)
            (sleep 0.1))
          ;;; Changes?
          (if step-changes-p (setq step-changes-p nil)
            (progn
              (capi:display-message "Ready. ~A occupied seat(s)" (count-if #'tile-occupied tiles))
              (setq end-sim t))))))

(defmethod simulate-tile ((self tile))
  (with-slots (new-state state latest-step-change-p) self
    (unless (tile-is-a-floor self)
      (let ((o-adjacents (count-occupied-adjacents self)))
        (cond ((and (tile-empty self) (zerop o-adjacents))
               (setf (new-state self) 'o))
              ((and (tile-occupied self) (>= o-adjacents 4))
               (setf (new-state self) 'e))))
      (unless (eql new-state state)
        (setq latest-step-change-p t)))))

(defmethod update-tile ((self tile))
  (with-slots (state new-state latest-step-change-p) self
    (unless (tile-is-a-floor self)
      (setq state new-state latest-step-change-p nil))))

(defmethod tile-empty ((self tile))
  (with-slots (state) self
    (eql state 'e)))

(defmethod tile-occupied ((self tile))
  (with-slots (state) self
    (eql state 'o)))

(defmethod tile-is-a-floor ((self tile))
  (with-slots (state) self
    (eql state 'f)))

(defmethod count-occupied-adjacents ((self tile))
  (with-slots (adjacents) self
    (count-if #'tile-occupied adjacents)))

(defun day-11-part-1 ()
  (capi:display (make-instance 'tiles-interf)))

;;; (day-11-part-1)
