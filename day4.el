;;; day4 --- a solution to day 4 -*- lexical-binding: t; -*-

;;; Commentary:

;; First Part

;; Input:

;; Solution:

;; Second Part

;; Input:

;; Solution:

;;; Code:

(require 'parse)
(require 'seq)
(eval-when-compile (require 'cl))
(require 'calc-arith)

;; Part 1

;; 1 3 5 7 9

(defun which-ring (length)
  "Given LENGTH, determine `ring-number', `ring-length' and `cummulative-length-so-far'."
  (let ((ring-number 0)
        (previous-length 0)
        (previous-cumm-length 0)
        (current-length 1)
        (cumm-length 1))
    (while (< cumm-length length)
      (setq previous-cumm-length cumm-length)
      (setq previous-length current-length)
      (incf cumm-length (- (* 4 current-length) 4))
      (incf current-length 2)
      (incf ring-number))
    `(,(1- ring-number) ,previous-length ,cumm-length ,previous-cumm-length)))

(defun distance-from-middle (length)
  "Produce the distance from the middle that LENGTH puts you at."
  (pcase (which-ring length)
    (`(,ring-number ,ring-length ,cumm-length ,previous-cumm-length)
     (let* ((distance-from-corner (mod (- length previous-cumm-length)
                                       (1- ring-length)))
            (distance-from-zero-point (math-abs (- distance-from-corner
                                                   (/ ring-length 2)))))
       (+ ring-number distance-from-zero-point)))))

(defun day4-part-1 (input)
  "Solve day 4 for INPUT."
  (interactive "sInput: ")
  (distance-from-middle (string-to-number input)))

;; Scratch pad

;; 37 36  35  34  33  32  31
;; 38 17  16  15  14  13  30
;; 39 18   5   4   3  12  29
;; 40 19   6   1   2  11  28
;; 41 20   7   8   9  10  27
;; 42 21  22  23  24  25  26
;; 43 44  45  46  47  48  49

(day4-part-1 "26")
(day4-part-1 "23")
(day4-part-1 "49")
(day4-part-1 "46")
(day4-part-1 "48")
(day4-part-1 "45")
(day4-part-1 "1")
(day4-part-1 "1024")
(day4-part-1 "12")
(day4-part-1 "14")
(day4-part-1 "13")

;; input

(day4-part-1 "368078")

;; solution 371

;; Part 2

(defun relative-coord (length)
  "Produce the radial coord which LENGTH puts you at."
  (pcase (which-ring length)
    (`(,ring-number ,ring-length ,cumm-length ,previous-cumm-length)
     `(,ring-number ,(- length previous-cumm-length)))))

(relative-coord 13)
(relative-coord 17)

(defun make-grid (length)
  "Produce which can fit memory of length LENGTH.

All values initialised to zero."
  (let* ((outer-ring-length (car (which-ring length)))
         (square-side       (1+ (* 2 outer-ring-length)))
         (grid              (make-vector square-side nil)))
    (dotimes (i square-side grid)
      (aset grid i (make-vector square-side 0)))))

(make-grid 23)

(defun x-y-coord (length mid-point)
  "Produce the x and y coord after moving to LENGTH.

Coord is corrected for target memory grid with the MID-POINT of
that grid."
  (pcase (which-ring length)
    (`(,ring-number ,ring-length ,cumm-length ,previous-cumm-length)
     (let ((offset (- mid-point ring-number)))
       (pcase (relative-coord length)
         (`(,ring ,distance)
          (let* ((ring-disp   (1- ring-length))
                 (num-lengths (/ distance ring-disp)))
            `(,(+ offset
                  (cond
                   ((eq num-lengths 0) ring-disp)
                   ((eq num-lengths 1) (- ring-disp (mod distance ring-disp)))
                   ((eq num-lengths 2) 0)
                   ((eq num-lengths 3) (mod distance ring-disp))
                   (t                  ring-disp)))
              ,(+ offset
                  (cond
                   ((eq num-lengths 0) distance)
                   ((eq num-lengths 1) ring-disp)
                   ((eq num-lengths 2) (- ring-disp (mod distance ring-disp)))
                   (t                  0)))))))))))

(x-y-coord 15 4)
(x-y-coord 10 10)
;; (x-y-coord 1 2) Doesn't work because the base case of which ring
;; doesn't work.
(x-y-coord 2  2)
(x-y-coord 3  2)
(x-y-coord 4  2)
(x-y-coord 5  2)

(defun grid-get (grid x y)
  "Produce the value of GRID at X, Y.

Produces zero if X and Y describe an out of bounds position."
  (let ((grid-length (length grid)))
    (if (or (>= x grid-length)
            (>= y grid-length)
            (< x 0)
            (< y 0))
        0
      (aref (aref grid x) y))))

(defun sum-adjacent (grid x y)
  "Sum all of the squares in GRID adjacent to X, Y."
  (+ (grid-get grid (1- x) (1- y))
     (grid-get grid x      (1- y))
     (grid-get grid (1+ x) (1- y))
     (grid-get grid (1+ x) y)
     (grid-get grid (1+ x) (1+ y))
     (grid-get grid x      (1+ y))
     (grid-get grid (1- x) (1+ y))
     (grid-get grid (1- x) y)))

(defun generate-memory (length)
  "Generate a memory block up to LENGTH.

Stress test is performed on the block inserting the sum of the
available adjacent cells as we go."
  (let* ((grid (make-grid length))
         (mid-point (/ (length grid) 2)))
    (aset (aref grid mid-point) mid-point 1)
    (dotimes (distance (1- length))
      (pcase (x-y-coord (+ 2 distance) mid-point)
        (`(,x ,y)
         (aset (aref grid x) y (sum-adjacent grid x y)))))
    grid))

(x-y-coord 9 2)

(cl-reduce (lambda (a x) (concat a "\n" x)) (seq-map (lambda (x) (format "%s" x)) (generate-memory 23)))

[0 0  0  0 0]
[0 34 10 5 0]
[0 23 1  4 0]
[0 0  1  2 0]
[0 1  0  0 0]

[13 4 2 0 0]
[9  0 2 0 0]
[0  1 1 0 0]
[1  0 0 0 0]
[0  0 0 0 0]

(defun day4-part-2 (input)
  "Solve day 4 part 2 for INPUT."
  (interactive "sInput: ")
  (let* ((number       (string-to-number input))
         (bound-approx (floor (sqrt number))) ; Doesn't work for low values (but they're visible in the input.)
         (memory       (generate-memory bound-approx)))
    (cl-find-if (apply-partially #'< number)
                (sort (apply (apply-partially #'seq-concatenate 'list) (seq-map #'identity memory)) #'<))))

(day4-part-2 "747")

;; input: 368078

(day4-part-2 "368078")

;;(insert (format "\n;; solution: %s" (day4-part-2 "368078")))
;; solution: 369601

(provide 'day4)
;;; day4 ends here
