;;; day5 --- a solution to day 5 -*- lexical-binding: t; -*-

;;; Commentary:

;; First Part

;; Input:

;; Solution:

;; Second Part

;; Input:

;; Solution:

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; part 1:

(cl-map 'vector #'string-to-number (apply #'vector (parse-lines "0
3
0
1
-3")))

(defun follow-instruction (instructions idx)
  "Follow the jump at IDX in INSTRUCTIONS.
Increment instruction at IDX afterwards."
  (if (>= (+ idx (aref instructions idx))
          (length instructions))
      nil
    (let ((move (aref instructions idx)))
      (aset instructions idx (1+ move))
      (+ idx move))))

(defun day5-part-1 (input)
  "Solve day 5 for INPUT."
  (interactive "sInput: ")
  (message "%s"
           (let ((cnt 0)
                 (idx 0)
                 (instructions (cl-map 'vector #'string-to-number (apply #'vector (parse-lines input)))))
             (while (not (null idx))
               (incf cnt)
               (setq idx (follow-instruction instructions idx)))
             cnt)))

;; solution: 372139

;; part 2:


(defun follow-instruction-2 (instructions idx)
  "Follow the jump at IDX in INSTRUCTIONS.
If the jump was greater than two then subtract from the
instruction otherwise increment it."
  (let* ((new-idx (+ idx (aref instructions idx))))
    (if (or (>= new-idx
                (length instructions))
            (< new-idx
               0))
        nil
      (let ((move (aref instructions idx)))
        (if (>= move 3)
            (aset instructions idx (1- move))
          (aset instructions idx (1+ move)))
        new-idx))))

(defun day5-part-2 (input)
  "Solve day 5 for INPUT."
  (interactive "sInput: ")
  (message "%s"
           (let ((cnt 0)
                 (idx 0)
                 (instructions (cl-map 'vector #'string-to-number (apply #'vector (parse-lines input)))))
             (while (not (null idx))
               (incf cnt)
               (setq idx (follow-instruction-2 instructions idx)))
             cnt)))

;; solution: 29629538

(provide 'day5)
;;; day5 ends here
