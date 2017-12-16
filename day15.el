;;; day15 --- a solution to day 15 -*- lexical-binding: t; -*-

;;; Commentary:

;; When I was writing this I was fairly certain that I was coding the
;; fastest way of solving the problem.  It only occured to me
;; afterwards that the slowest operation here (by a very long way) is
;; the modulo by 4, 8 and 2147483647.
;;
;; For the modulo 4 and 8 we're only interested in whether the numbers
;; are evenly divisible.  I'm sure that this check can also be done
;; with some fancy bit twiddling.
;;
;; Consider 4 (100) any remainder after division by four must have a
;; value in the last two bits (the 2's and 1's) so if you mask a
;; number by 3 then you should be able to detect divisibility by 4.
;; Likewise, for 8 you should be able to mask it by 7 (111) to check
;; whether it's evenly divisible.  This is, ofcourse, a consequence of
;; the fact that 4 and 8 are powers of 2.
;;
;; It turns out that 2147483647 is a power of 2 minus one
;; (1111111111111111111111111111111); to speed up that operation you
;; can simply mask the number by it.
;;
;; I don't see any pattern in either 16807 or 48271.  Perhaps I need
;; to stare at their binary representations for longer.

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1:

(defun generate (a b)
  "Find the number of times that A and B have matching 16 first bits.

The generator function is quite elaborate.  Please look at the
source rather than expecting me to describe it here."
  (let ((current-a a)
        (current-b b)
        (matches   0))
    (dotimes (_ 40000000 matches)
      (setq current-a (mod (* current-a 16807) 2147483647))
      (setq current-b (mod (* current-b 48271) 2147483647))
      (when (eq (logand current-a 65535)
                (logand current-b 65535))
        (incf matches)))))

(defun day15-part-1 (input)
  "Solve day 15 for INPUT."
  (interactive "sInput: ")
  (let* ((lines  (parse-lines input))
         (starts (mapcar (lambda (line) (string-to-number (nth 4 (split-string line " ")))) lines)))
    (message "%s" (apply #'generate starts))))

;; Input:
;; Generator A starts with 783
;; Generator B starts with 325

;; Solution: 650

;; Part 2:


(defun generate-multiples-of-4-and-8 (a b)
  "Count the times that A and B have matching 16 first bits in a new gen func.

Once again the generator function is quite elaborate and I'd
rather that you just look at the source instead  :) ."
  (let ((current-a a)
        (current-b b)
        (matches   0))
    (dotimes (_ 5000000 matches)
      (setq current-a (mod (* current-a 16807) 2147483647))
      (setq current-b (mod (* current-b 48271) 2147483647))
      (while (/= (mod current-a 4) 0)
        (setq current-a (mod (* current-a 16807) 2147483647)))
      (while (/= (mod current-b 8) 0)
        (setq current-b (mod (* current-b 48271) 2147483647)))
      (when (eq (logand current-a 65535)
                (logand current-b 65535))
        (incf matches)))))



(defun generate-multiples-of-4-and-8-faster (a b)
  "Count the times that A and B have matching 16 first bits in a new gen func.

Once again the generator function is quite elaborate and I'd
rather that you just look at the source instead  :) .

This is a faster version which encorporates everything which I
described in the commentary section of the source file for this
function.

It doesn't work and I'm fairly certain that it's the replacement
for module which I wrote which is causing me head aches.  I'll
work on it from home."
  (let ((current-a a)
        (current-b b)
        (matches   0))
    (dotimes (_ 5000000 matches)
      (setq current-a (mod (* current-a 16807) 2147483647))
      (setq current-b (mod (* current-b 48271) 2147483647))
      (while (/= (logand current-a 3) 0)
        (setq current-a (mod (* current-a 16807) 2147483647)))
      (while (/= (logand current-b 7) 0)
        (setq current-b (mod (* current-b 48271) 2147483647)))
      (when (eq (logand current-a 65535)
                (logand current-b 65535))
        (incf matches)))))

(defun day15-part-2 (input)
  "Solv part 2e day 15 for INPUT."
  (interactive "sInput: ")
  (let* ((lines  (parse-lines input))
         (starts (mapcar (lambda (line) (string-to-number (nth 4 (split-string line " ")))) lines)))
    (message "%s" (apply #'generate-multiples-of-4-and-8-faster starts))))

;; Solution: 336

(provide 'day15)
;;; day15 ends here
