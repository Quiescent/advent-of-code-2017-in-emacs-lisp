;;; day2-loop --- My solution to day2-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day2-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(require 'seq)

(defun day2-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     for numbers = (mapcar #'string-to-number (split-string line "\t" t " "))
     sum (- (seq-max numbers)
            (seq-min numbers))))

;; # PART 2:

(defun day2-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     for numbers = (mapcar #'string-to-number (split-string line "\t" t " "))
     sum (cl-loop for x in numbers
            for result = (cl-loop for y in numbers
                            when (and (not (eq x y))
                                      (eq (mod y x) 0))
                              return (cons x y))
            when result
              return (/ (cdr result) (car result)))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day2-loop-part-1 input-1))
    (message "Part 2: %s\n" (day2-loop-part-2 input-2))))

(provide 'day2-loop)
;;; day2-loop ends here
