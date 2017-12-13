;;; day13 --- a solution to day 13 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; 0: 3
;; 1: 2
;; 2: 6
;; 4: 4
;; 6: 4
;; 8: 10
;; 10: 6
;; 12: 8
;; 14: 5
;; 16: 6
;; 18: 8
;; 20: 8
;; 22: 12
;; 24: 6
;; 26: 9
;; 28: 8
;; 30: 8
;; 32: 10
;; 34: 12
;; 36: 12
;; 38: 8
;; 40: 12
;; 42: 12
;; 44: 14
;; 46: 12
;; 48: 12
;; 50: 12
;; 52: 12
;; 54: 14
;; 56: 14
;; 58: 14
;; 60: 12
;; 62: 14
;; 64: 14
;; 66: 17
;; 68: 14
;; 72: 18
;; 74: 14
;; 76: 20
;; 78: 14
;; 82: 18
;; 86: 14
;; 90: 18
;; 92: 14

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1:

(defun parse-layer (line)
  "Parse LINE into a layer and range."
  (pcase (split-string line ": " t)
    (`(,layer ,range)
     `(,(string-to-number layer)
       ,(string-to-number range)))))

(defun parse-layers (lines)
  "Parse LINES into a list of layer-range tuples."
  (mapcar #'parse-layer lines))

(defun position-of (position system time)
  "Produce the position of the scanner in POSITION in SYSTEM at TIME."
  (let* ((range (1- (cadr (nth position system))))
         (trips (/ time range)))
    (if (cl-evenp trips)
        (mod time range)
      (- range (mod time range)))))

(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 1)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 2)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 3)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 4)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 5)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 6)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 7)
(position-of 2 '((0 3) (1 2) (4 4) (6 4)) 8)

(defun move-through (system)
  "Produce the cost of moving through SYSTEM at t=0."
  (let ((total-severity 0)
        (current-position 0))
    (dolist (layer system total-severity)
      (let ((current-layer (car layer)))
        (when (eq 0 (position-of current-position system current-layer))
          (incf total-severity (* current-layer (cadr layer))))
        (incf current-position)))))

(defun day13-part-1 (input)
  "Solve day 13 for INPUT."
  (interactive "sInput: ")
  (let* ((lines  (parse-lines input))
         (system (parse-layers lines)))
    (message "%s" (move-through system))))

;; Solution: 3184

(defun position-of-car (layer time)
  "Produce the position that the scanner in LAYER will be after TIME."
  (let* ((range (1- (cadr layer)))
         (trips (/ time range)))
    (if (cl-evenp trips)
        (mod time range)
      (- range (mod time range)))))

(defun move-through-at-time (system delay)
  "Move through SYSTEM only starting after DELAY.

Produce t if we were found by a scanner."
  (let ((found nil))
    (while (and system
                (not found))
      (let* ((layer (pop system))
             (current-layer (car layer)))
        (when (eq 0 (position-of-car layer (+ delay current-layer)))
          (setq found t))))
    found))

(defun day13-part-2 (input)
  "Solv part 2e day 13 for INPUT."
  (interactive "sInput: ")
  (let* ((lines  (parse-lines input))
         (system (parse-layers lines))
         (time   0))
    (while (move-through-at-time system time)
      (incf time))
    (message "%s" time)))

;; Solution: 3878062

(provide 'day13)
;;; day13 ends here
