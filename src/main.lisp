(defpackage game-of-life
  (:use :cl
        :raylib))
(in-package :game-of-life)

(defconstant +dead+ 0)
(defconstant +alive+ 1)

(defvar *board-rows* 64)
(defvar *board-columns* 64)
(defvar *cell-size* 8)
(defvar *screen-width* (* (1- *board-columns*) *cell-size*))
(defvar *screen-height* (* (1- *board-rows*) *cell-size*))
(defvar *title* "GOL - CL")

(declaim (inline make-board))
(defun make-board (width height)
  (make-array (list width height) :element-type 'bit :initial-element +dead+))

(defvar *board* (make-board *board-columns* *board-rows*))

(defmacro iterate-with-board (board &body body)
  (let ((rows (gensym "rows"))
        (columns (gensym "columns")))
    `(destructuring-bind (,rows ,columns) (array-dimensions ,board)
       (loop :for row :from 0 :below ,rows
             :do (loop :for column :from 0 :below ,columns
                       :do (progn
                             ,@body))))))

(defun alive-p (coordinates &optional (board *board*))
  (= (apply #'aref (cons board coordinates)) +alive+))

(defun dead-p (coordinates &optional (board *board*))
  (not (alive-p coordinates board)))

(defun randomize-board (&optional (board *board*))
  (iterate-with-board board
    (when (zerop (mod (random 2) 2))
      (setf (aref board row column) +alive+))))

(defun clear-board (&optional (board *board*))
  (iterate-with-board board
    (setf (aref board row column) +dead+)))

(defun in-board-p (coordinates dimensions)
  (destructuring-bind (rows cols x y) (append dimensions coordinates)
    (and (<= 0 x)
         (< x cols)
         (<= 0 y)
         (< y rows))))

(defun apply-offset (coordinates offset)
  (destructuring-bind (x y dx dy) (append coordinates offset)
    (list (+ x dx)
          (+ y dy))))

(defun neighbour-count (coordinates board)
  (loop :with neighbour-offsets := '((-1 -1)
                                     (0 -1)
                                     (1 -1)
                                     (-1 0)
                                     (1 0)
                                     (-1 1)
                                     (0 1)
                                     (1 1))
        :with dimensions := (array-dimensions board)
        :for neighbour-offset :in neighbour-offsets
        :for neighbour := (apply-offset coordinates neighbour-offset)
        :count (and (in-board-p neighbour dimensions)
                    (alive-p neighbour board))))

(defun survives-p (coordinates board)
  (let ((neighbour-count (neighbour-count coordinates board))
        (old-cell (apply #'aref (cons board coordinates))))
    (cond
      ((and
        (= +alive+ old-cell)
        (or (< neighbour-count 2)
            (> neighbour-count 3)))
       nil)
      ((= neighbour-count 3)
       t)
      (t (= +alive+ old-cell)))))

(defun alive-cells (&optional (board *board*))
  (let ((count 0))
    (iterate-with-board board
      (when (alive-p (list column row) board)
        (incf count)))
    count))

(defun advance-generation (&optional (board *board*))
  (let ((new-board (apply #'make-board (array-dimensions board))))
    (iterate-with-board board
      (when (survives-p (list row column) board)
        (setf (aref new-board row column) +alive+)))
    new-board))

(defun draw-board (&optional (board *board*))
  (with-drawing
    (clear-background +black+)
    (iterate-with-board board
      (when (alive-p (list column row) board)
        (draw-rectangle (* column *cell-size*) (* row *cell-size*) *cell-size* *cell-size* +white+)))))

(defun main()
  (with-window (*screen-width* *screen-height* *title*)
    (set-target-fps 30)
    (loop :initially (randomize-board)
          :until (window-should-close)
          :do (progn
                (when (is-key-down +key-r+)
                  (clear-board)
                  (randomize-board))
                (draw-board)
                (setf *board* (advance-generation))))))
