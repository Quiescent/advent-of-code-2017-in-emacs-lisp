;;; day11 --- a solution to day 11 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(require 'seq)

;; Part 1:

(defun final-pos (moves)
  "Make MOVES and produce the final position after having made them."
  (let ((x 0)
        (y 0)
        (z 0)n)
    (dolist (move moves `(,x ,y ,z))
      (cond
       ((string-equal move "se")
        (progn
          (setq z (1- z))
          (setq x (1- x))))
       ((string-equal move "s")
        (progn
          (setq z (1- z))
          (setq y (1+ y))))
       ((string-equal move "sw")
        (progn
          (setq x (1+ x))
          (setq y (1+ y))))
       ((string-equal move "nw")
        (progn
          (setq z (1+ z))
          (setq x (1+ x))))
       ((string-equal move "n")
        (progn
          (setq y (1- y))
          (setq z (1+ z))))
       ((string-equal move "ne")
        (progn
          (setq x (1- x))
          (setq y (1- y))))))))

(final-pos (split-string "ne,ne,ne,n" "," t))
(final-pos (split-string "ne,ne,nw" "," t))

(defun day11-part-1 (input)
  "Solve day 11 for INPUT."
  (interactive "sInput: ")
  (let* ((moves   (split-string input "," t))
         (pos     (final-pos moves))
         (abs-pos (sort (seq-map #'identity (mapcar #'abs pos)) #'>)))
    (message "%s"
             (+ (cadr abs-pos)
                (abs (- (car  abs-pos)
                        (cadr abs-pos)))))))

(day11-part-1 "se,sw,se,sw,sw")
(day11-part-1 "ne,ne,ne")
(day11-part-1 "ne,ne,nw")
(day11-part-1 "ne,ne,nw,n,ne,nw,n")
(day11-part-1 "ne,ne,sw,sw")
(day11-part-1 "ne,ne,s,s")

;; Solution:

;; Part 2:

(defun greatest-distance (moves)
  "Produce the furthest we were from (0, 0, 0) when making MOVES."
  (let ((x                0)
        (y                0)
        (z                0)
        (furthest         0)
        (current-distance 0))
    (dolist (move moves)
      (cond
       ((string-equal move "se")
        (progn
          (setq z (1- z))
          (setq x (1- x))))
       ((string-equal move "s")
        (progn
          (setq z (1- z))
          (setq y (1+ y))))
       ((string-equal move "sw")
        (progn
          (setq x (1+ x))
          (setq y (1+ y))))
       ((string-equal move "nw")
        (progn
          (setq z (1+ z))
          (setq x (1+ x))))
       ((string-equal move "n")
        (progn
          (setq y (1- y))
          (setq z (1+ z))))
       ((string-equal move "ne")
        (progn
          (setq x (1- x))
          (setq y (1- y)))))
      (setq current-distance (distance-from-start `(,x ,y ,z)))
      (when (> current-distance furthest)
        (setq furthest current-distance)))
    furthest))

(defun distance-from-start (pos)
  "Produce the distance which POS is from start.

POS is a coord in x, y and z."
  (let* ((abs-pos (sort (seq-map #'identity (mapcar #'abs pos)) #'>)))
    (+ (cadr abs-pos)
       (abs (- (car  abs-pos)
               (cadr abs-pos))))))

(defun day11-part-2 (input)
  "Solv part 2e day 11 for INPUT."
  (interactive "sInput: ")
  (let* ((moves (split-string input ",")))
    (message "%s" (greatest-distance moves))))

;; Solution

(provide 'day11)
;;; day11 ends here
