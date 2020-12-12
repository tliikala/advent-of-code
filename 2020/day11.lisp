
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
   (old-state :initform nil :accessor old-state)
   (latest-step-change-p :initform nil :accessor latest-step-change-p)
   (adjacents :initform nil :accessor adjacents :documentation "the surrounding tiles")
   (highlight-p :initform nil :accessor highlight-p)
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
                                      collect (make-instance 'tile :state (cond ((char= c #\L) 'e)
                                                                                ((char= c #\#) 'o)
                                                                                ((char= c #\.) 'f))
                                                             :x x :y y
                                                             

                                                             ))))
    (set-adjacents)
    *tiles*

    ))

;;; Part 1


(defun set-adjacents ()
  (loop for y in *tiles*
        for y-1 upfrom -1
        for y0 upfrom 0
        for y+1 upfrom 1
        do (loop for x in y
                 for x-1 upfrom -1
                 for x0 upfrom 0
                 for x+1 upfrom 1
                 as adjacents = (remove nil (list (ignore-errors (nth x-1 (nth y-1 *tiles*)))
                                                  (ignore-errors (nth x0 (nth y-1 *tiles*)))
                                                  (ignore-errors (nth x+1 (nth y-1 *tiles*)))

                                                  (ignore-errors (nth x-1 (nth y0 *tiles*)))
                                                  (ignore-errors (nth x+1 (nth y0 *tiles*)))
                                                  
                                                  (ignore-errors (nth x-1 (nth y+1 *tiles*)))
                                                  (ignore-errors (nth x0 (nth y+1 *tiles*)))
                                                  (ignore-errors (nth x+1 (nth y+1 *tiles*)))))
                 do (setf (adjacents x) adjacents))))


                                    


;(defmethod simulate ((self tile))
;  (with-slots (state old-state latest-step-change-p) self))

#|
(capi:define-interface tiles-interf ()
  ((tiles :initform nil :accessor game-tiles))
  (:panes
   (game-disp
    capi:output-pane
    :display-callback 'draw-tiles
    :background :white
    :draw-with-buffer t
    :drawing-mode :quality
    :initial-constraints '(:min-width 700 :min-height 650)

    :input-model `(

                   
                   ((:button-1 :press)
                    button-input-callback
                    "Button-1 press" t)

                   ))

   )
  (:layouts
   (p‰‰
    capi:column-layout
    '(game-disp)
    ))
  (:default-initargs
   :layout 'p‰‰
   :auto-menus nil
   :title "Tiles"))

(defmethod initialize-instance :after ((self tiles-interf) &key)
  (with-slots (tiles) self
    (setq tiles (read-input-data))
    ))


(defun draw-tiles (pane &rest x)
  (declare (ignore x))

  (let ((tiles (game-tiles (capi:element-interface pane))))
    (gp:with-graphics-translation (pane 0 0)
      (gp:with-graphics-scale (pane 0.4 0.4)
      
      
      (loop for row in tiles
            for j upfrom 0
            do (loop for tile in row
                     for i upfrom 0
                     do (case (state tile)
                          (e (gp:draw-rectangle pane (* i 16) (* j 16) 16 16 :filled t :foreground :white))
                          (o (gp:draw-circle pane (* i 16) (* j 16) 10 :filled t :foreground :blue))
                          (f (gp:draw-rectangle pane (* i 16) (* j 16) 16 16 :filled t :foreground :black))
                          )))))))

(defun button-input-callback (self x y callback-name press-p)
  

  
  (gp:draw-character self (if press-p #\P #\R) x y)


  

  )
 
;(capi:display (make-instance 'tiles-interf))
|#

(capi:define-interface tiles-interf ()
  ((tiles :initform nil :accessor game-tiles))
  (:layouts
   (p‰‰
    capi:pinboard-layout
    nil
    :draw-with-buffer t
    :fit-size-to-children nil
    :input-model '(((:button-1 :press) locate-tile))))
  (:default-initargs
   :layout 'p‰‰
   :auto-menus nil
   :title "Tiles"
   :best-width 800
   :best-height 800

   ))

(defmethod initialize-instance :after ((self tiles-interf) &key)
  (with-slots (tiles p‰‰) self
    
   ; (setq tiles (read-input-data))
    
    (setq tiles (loop for row in (read-input-data)
                      append row))

    
    (setf (capi:layout-description p‰‰)
          
          (loop for tile in tiles
          
                             collect (make-instance 'capi:drawn-pinboard-object :plist (list :tile tile)

                                                    :x (+ (* 8 (x tile)) 5)
                                                    :y (+ (* 8 (y tile)) 5)
                                                    :visible-min-width 8
                                                    :visible-min-height 8
                                                    
                                                    :display-callback 'draw-a-tile
                                                    )))))

(defun locate-tile (pinboard-layout x y)


  (lw:when-let (tilepb (capi:pinboard-object-at-position pinboard-layout x y))




    (highlight-tile (capi:capi-object-property tilepb :tile) pinboard-layout)))

(defmethod highlight-tile ((self tile) pinboard-layout)
  
  (loop for tile in (game-tiles (capi:element-interface pinboard-layout))
        do (setf (highlight-p tile) nil))

  
  (dolist (tile (adjacents self))
    (setf (highlight-p tile) t))
  
  
  (gp:invalidate-rectangle pinboard-layout)
  )


(defun draw-a-tile (output-pane self x y width height)
  (declare (ignore height))
  (gp:with-graphics-translation (output-pane 0 0)
    

    (let ((tile (capi:capi-object-property self :tile)))
      
      (gp:draw-circle output-pane x y (/ width 2)
                      :filled (if (highlight-p tile) t
                                (case (state tile) (e nil) (o t) (f t)))
                       
                      :foreground (case (state tile) (e :white) (o :blue) (f :black)))
      )))


(defun day-11-part-1 ()
  (capi:display (make-instance 'tiles-interf)))
