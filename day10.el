;;; day10 --- a solution to day 9 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: 106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))
(require 'seq)

;; Part 1

(defun reverse-length (xs rev-len idx)
  "Reverse the subseq of XS starting at IDX with REV-LEN length.

XS is circular and REV-LEN can wrap."
  (let* ((end        (if (> (+ rev-len idx) (length xs))
                         (length xs)
                       (+ rev-len idx)))
         (from-start (if (> (+ rev-len idx) (length xs))
                         (- rev-len (- (length xs) idx))
                       0))
         (rev-segment (reverse (seq-concatenate 'list
                                                (seq-subseq xs idx end)
                                                (seq-subseq xs 0 from-start)))))
    (seq-concatenate 'list
                     (seq-subseq rev-segment (- (length rev-segment) from-start))
                     (seq-subseq xs from-start idx)
                     (seq-subseq rev-segment 0 (- (length rev-segment) from-start))
                     (seq-subseq xs end))))

(reverse-length "this is a test" 5 2)

(defun day10-part-1 (input)
  "Solve day 9 for INPUT."
  (interactive "sInput: ")
  (let* ((lengths (mapcar #'string-to-number (split-string input "," t)))
         (idx 0)
         (skip 0)
         (numbers (let (xs)
                    (dotimes (i 256 (reverse xs))
                      (push i xs)))))
    (message "%s"
             (dolist (length lengths (apply #'* (seq-take numbers 2)))
               (setq numbers (reverse-length numbers length idx))
               (setq idx  (mod (+ idx (+ length skip)) (length numbers)))
               (incf skip)))))

;; (day10-part-1 "3,4,1,5") Set 256 to 5 above to test this

;; Solution: 6909

;; Part 2

(defconst hex-numbers '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")
  "The numbers in hexadecimal radix.")

(defun to-hex (number)
  "Convert NUMBER to hexadecimal."
  (concat (nth (/ number 16) hex-numbers)
          (nth (mod number 16) hex-numbers)))

(defun knot-hash (input)
  (let* ((lengths (seq-concatenate 'list (string-to-list input) '(17 31 73 47 23)))
         (idx 0)
         (skip 0)
         (numbers (let (xs)
                    (dotimes (i 256 (reverse xs))
                      (push i xs)))))
    (mapconcat #'to-hex
               (mapcar (lambda (sub-list) (apply #'logxor sub-list))
                       (seq-partition
                        (dotimes (_ 64 numbers)
                          (dolist (length lengths)
                            (setq numbers (reverse-length numbers length idx))
                            (setq idx  (mod (+ idx (+ length skip)) (length numbers)))
                            (incf skip)))
                        16))
               "")))

(defun day10-part-2 (input)
  "Solve day 9 part 2 for INPUT."
  (interactive "sInput: ")
  (message "%s" (knot-hash input)))

;; Solution: 9d5f4561367d379cfbf04f8c471c0095

(provide 'day10)
;;; day10 ends here
