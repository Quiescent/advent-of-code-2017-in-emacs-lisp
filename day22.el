;;; day22 --- a solution to day 22 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1:

(defun parse-map (lines)
  "Parse LINES to a vector of vectors with t for hashes and nil for dots."
  (cl-map 'vector
          (lambda (line)
            (cl-map 'vector
                    (lambda (node) (eq node ?#))
                    line))
          lines))

(parse-map (split-string "..##.##.######...#.######
##...#...###....##.#.#.##
###.#.#.#..#.##.####.#.#.
..##.##...#..#.##.....##.
##.##...#.....#.#..#.####
.###...#.........###.####
#..##....###...#######..#
###..#.####.###.#.#......
.#....##..##...###..###.#
###.#..#.##.###.#..###...
####.#..##.#.#.#.#.#...##
##.#####.#......#.#.#.#.#
..##..####...#..#.#.####.
.####.####.####...##.#.##
#####....#...#.####.#..#.
.#..###..........#..#.#..
.#.##.#.#.##.##.#..#.#...
..##...#..#.....##.####..
..#.#...######..##..##.#.
.####.###....##...####.#.
.#####..#####....####.#..
###..#..##.#......##.###.
.########...#.#...###....
...##.#.##.#####.###.####
.....##.#.#....#..#....#." "\n"))

(defun to-sparse-map (map)
  "Produce a sparse map from MAP where tuples index into the map."
  (let ((sparse-map (make-hash-table :test #'equal)))
    (dotimes (i (length map) sparse-map)
      (dotimes (j (length (aref map 0)))
        (puthash `(,j . ,i) (aref (aref map i) j) sparse-map)))))

(format "%s" (to-sparse-map (parse-map (split-string "..#
#..
..." "\n"))))

(defun turn-left (direction)
  "Produce the direction one would face when turning left from DIRECTION."
  (cond
   ((eq direction 'UP)    'LEFT)
   ((eq direction 'LEFT)  'DOWN)
   ((eq direction 'DOWN)  'RIGHT)
   ((eq direction 'RIGHT) 'UP)))

(defun turn-right (direction)
  "Produce the direction one would face when turning right from DIRECTION."
  (cond
   ((eq direction 'UP)    'RIGHT)
   ((eq direction 'LEFT)  'UP)
   ((eq direction 'DOWN)  'LEFT)
   ((eq direction 'RIGHT) 'DOWN)))

(defun move-forward (direction coord)
  "Produce the new position after moving in DIRECTION from COORD."
  (pcase coord
    (`(,x .,y)
     (cond
      ((eq direction 'UP)    `(,x      . ,(1- y)))
      ((eq direction 'LEFT)  `(,(1- x) . ,y))
      ((eq direction 'DOWN)  `(,x      . ,(1+ y)))
      ((eq direction 'RIGHT) `(,(1+ x) . ,y))))))

(move-forward 'DOWN  '(3 . 4))
(move-forward 'LEFT  '(3 . 4))
(move-forward 'UP    '(3 . 4))
(move-forward 'RIGHT '(3 . 4))

(defun burst-times (map times)
  "Wake the virus up to do its work on MAP TIMES many times and count infections."
  (let* ((infections-caused 0)
         (current-direction 'UP)
         (x                 (/ (length (aref map 0)) 2))
         (y                 (/ (length map) 2))
         (current-coord     `(,x . ,y))
         (sparse-map        (to-sparse-map map)))
    (dotimes (_ times infections-caused)
      (let ((current-node  (gethash current-coord sparse-map nil)))
        (if current-node
            (progn
              (setq current-direction (turn-right current-direction))
              (puthash current-coord nil sparse-map))
          (progn
            (cl-incf infections-caused)
            (setq current-direction (turn-left current-direction))
            (puthash current-coord t sparse-map)))
        (setq current-coord (move-forward current-direction current-coord))))))

(defun day22-part-1 (input)
  "Solve day 22 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (map   (parse-map   lines)))
    (message "%s" (burst-times map 10000))))

;; Solution: 5339

;; Part 2:

(defun reverse-direction (direction)
  "Produce the direction one would be facing if you turned back while facing DIRECTION."
  (cond
   ((eq direction 'UP)    'DOWN)
   ((eq direction 'LEFT)  'RIGHT)
   ((eq direction 'DOWN)  'UP)
   ((eq direction 'RIGHT) 'LEFT)))

(defun burst-times-enhanced (map times)
  "Wake the virus up to do its work on MAP TIMES many times & count infections.

This time, we weaken clean nodes and infect the weakened ones, we
also flag the infected nodes instead of cleaning and only clean
flagged nodes."
  (let* ((infections-caused 0)
         (current-direction 'UP)
         (x                 (/ (length (aref map 0)) 2))
         (y                 (/ (length map) 2))
         (current-coord     `(,x . ,y))
         (sparse-map        (to-sparse-map map)))
    (dotimes (_ times infections-caused)
      (let ((current-node  (gethash current-coord sparse-map nil)))
        (cond
         ((eq current-node t)
          (progn
            (setq current-direction (turn-right current-direction))
            (puthash current-coord 'FLAGGED sparse-map)))
         ((eq current-node nil)
          (progn
            (setq current-direction (turn-left current-direction))
            (puthash current-coord 'WEAKENED sparse-map)))
         ((eq current-node 'WEAKENED)
          (progn
            (puthash current-coord t sparse-map)
            (cl-incf infections-caused)))
         ((eq current-node 'FLAGGED)
          (progn
            (setq current-direction (reverse-direction current-direction))
            (puthash current-coord nil sparse-map))))
        (setq current-coord (move-forward current-direction current-coord))))))


(defun day22-part-2 (input)
  "Solve part 2 day 22 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (map   (parse-map   lines)))
    (message "%s" (burst-times-enhanced map 10000000))))

;; Solution: 2512380

(provide 'day22)
;;; day22 ends here
