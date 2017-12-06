;;; day6 --- a solution to day 6 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1

(defun reallocate (memory idx)
  "Perform a reallocation cycle starting at IDX in MEMORY.

Redistribute the value in IDX evenly among the slots in MEMORY by
iterating, one by one, through them."
  (let ((remaining (aref memory idx)))
    (aset memory idx 0)
    (dotimes (i remaining memory)
      (setq idx (mod (1+ idx) (length memory)))
      (aset memory idx (1+ (aref memory idx))))))

(defun idx-of-highest (memory)
  "Find the index of the highest value in MEMORY."
  (let ((idx-highest 0)
        (val-highest (aref memory 0)))
    (dotimes (idx (length memory) idx-highest)
      (when (> (aref memory idx)
               val-highest)
        (setq idx-highest idx
              val-highest (aref memory idx))))))

(idx-of-highest [0 2 7 0])

(defun reallocate-until-seen (memory)
  "Keep reallocating until the given state of MEMORY is encountered again."
  (let ((count 0)
        (idx   (idx-of-highest memory))
        (seen  `(,(format "%s" memory)))
        found)
    (while (null found)
      (reallocate memory idx)
      (incf count)
      (let ((memory-key (format "%s" memory)))
        (if (member memory-key seen)
            (setq found count)
          (progn
            (push memory-key seen)
            (setq idx (idx-of-highest memory))))))
    `(,found ,memory)))

(defun day6-part-1 (input)
  "Solve day 6 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (memory (map #'vector #'string-to-number (split-string input "\t" t))))
    (message "%s" (car (reallocate-until-seen memory)))))

;; (day6-part-1 "5\t1\t10\t0\t1\t7\t13\t14\t3\t12\t8\t10\t7\t12\t0\t6")

;; solution: 5042

;; Part 2

(defun day6-part-2 (input)
  "Solve day 6 part 2 for INPUT."
  (interactive "sInput: ")
  (let ((lines (parse-lines input)))
    (let* ((lines (parse-lines input))
           (memory (map #'vector #'string-to-number (split-string input "\t" t))))
      (message "%s" (car (reallocate-until-seen (cadr (reallocate-until-seen memory))))))))

;; (day6-part-2 "5\t1\t10\t0\t1\t7\t13\t14\t3\t12\t8\t10\t7\t12\t0\t6")

;; solution: 1086


(provide 'day6)
;;; day6 ends here
