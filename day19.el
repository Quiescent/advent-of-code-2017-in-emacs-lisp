;;; day19 --- a solution to day 19 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1

(defun find-start (map)
  "Find the starting square on MAP.

The starting square is the `|' on the top row."
  (cl-position ?| (aref map 0)))

(defun is-letter (char)
  "Produce t if CHAR is a letter."
  (and (>= char ?A)
       (<= char ?Z)))

(defun safe-get (map x y)
  "Produce the value in MAP at (X, Y).

Produces nil if the coordinate is out of bounds."
  (when (and (<  y (length map))
             (>= y 0)
             (<  x (length (aref map y)))
             (>= x 0))
    (aref (aref map y) x)))

(defun new-direction (map x y previous-direction)
  "Produce the direction which we should turn in on MAP at (X, Y).

We use the PREVIOUS-DIRECTION to prevent back tracking."
  (let ((left  (safe-get map (1- x) y))
        (right (safe-get map (1+ x) y))
        (up    (safe-get map x      (1- y)))
        (down  (safe-get map x      (1+ y))))
    (when (eq left  ?\ ) (setq left  nil))
    (when (eq right ?\ ) (setq right nil))
    (when (eq up    ?\ ) (setq up    nil))
    (when (eq down  ?\ ) (setq down  nil))
    (cond
     ((eq previous-direction 'DOWN) (if left 'LEFT (if right 'RIGHT nil)))
     ((eq previous-direction 'UP) (if left 'LEFT (if right 'RIGHT nil)))
     ((eq previous-direction 'LEFT) (if up 'UP (if down 'DOWN nil)))
     ((eq previous-direction 'RIGHT) (if up 'UP (if down 'DOWN nil))))))

;;     |
;;     |  +--+
;;     A  |  C
;; F---|----E|--+
;;     |  |  |  D
;;     +B-+  +--+

(defun search (map)
  "Produce the order in which letters are encountered in MAP."
  (let ((x (find-start map))
        (y 0)
        (direction 'DOWN)
        (current-block ?|)
        found)
    (cl-labels ((move ()
                      (cond
                       ((eq direction 'DOWN) (cl-incf y))
                       ((eq direction 'UP) (cl-decf y))
                       ((eq direction 'LEFT) (cl-decf x))
                       ((eq direction 'RIGHT) (cl-incf x))))
                (change-direction ()
                                  (cond
                                   ((eq current-block ?+) (setq direction
                                                                (new-direction map x y direction)))
                                   (t direction))))
      (while (and (not (eq current-block ?\ ))
                  (not (eq direction nil)))
        (move)
        (setq current-block (aref (aref map y) x))
        (when (is-letter current-block)
          (push current-block found))
        (change-direction))
      (nreverse found))))

(defun day19-part-1 (input)
  "Solve day 19 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (map (apply #'vector lines))
         (found (search map)))
    (message "%s" (apply #'string found))))

;; Solution: ITSZCJNMUO

;; Part 2:

(defun count-steps (map)
  "Count the number of movements made by following MAP to it's end."
  (let ((x (find-start map))
        (y 0)
        (direction 'DOWN)
        (current-block ?|)
        (steps 0))
    (cl-labels ((move ()
                      (cond
                       ((eq direction 'DOWN) (cl-incf y))
                       ((eq direction 'UP) (cl-decf y))
                       ((eq direction 'LEFT) (cl-decf x))
                       ((eq direction 'RIGHT) (cl-incf x))))
                (change-direction ()
                                  (cond
                                   ((eq current-block ?+) (setq direction
                                                                (new-direction map x y direction)))
                                   (t direction))))
      (while (and (not (eq current-block ?\ ))
                  (not (eq direction nil)))
        (move)
        (setq current-block (aref (aref map y) x))
        (incf steps)
        (change-direction))
      steps)))


(defun day19-part-2 (input)
  "Solv part 2e day 19 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (map (apply #'vector lines))
         (steps (count-steps map)))
    (message "%s" steps)))

;; Solution: 17420

(provide 'day19)
;;; day19 ends here
