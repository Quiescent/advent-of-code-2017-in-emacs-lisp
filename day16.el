;; day16 --- a solution to day 16 -*- lexical-binding: t; -*-

;;; Commentary:

;; A cool idea which didn't work:
;;
;; What I thought was that a round of the dance might always have the
;; same effect.  Therefore you can determine the swaps to go from init
;; to the next string.  Going two forward is then from init to current
;; after doing the swaps you just figured out.  You can then go four
;; forward etc because it won't end up being evenly divisible with one
;; billion you have to reset the jump when it gets to big.
;; Unfortunately my idea was wrong and doing swaps consecutive times
;; doesn't work.
;;
;; (defun determine-swaps (original after)
;;   (cl-mapcar (lambda (char) (cl-position char original)) after))
;;
;; (defun do-swaps (prev-string next-string swaps)
;;   (progn
;;     (let ((idx 0))
;;       (mapc (lambda (from)
;;               (progn
;;                 (aset next-string idx (aref prev-string from))
;;                 (incf idx)))
;;             swaps))
;;     (cl-replace prev-string next-string)))
;;
;; (setq swaps (determine-swaps init-string prev-string))
;;        (while (> remaining 0)
;;          (setq jump-length 1)
;;          (while (>= remaining jump-length)
;;            (let ((prev-string   (substring prev-string 0))
;;                  (next-string   (substring next-string 0)))
;;              (do-dance)
;;              (message "%s" prev-string))
;;            (do-swaps prev-string next-string swaps)
;;            (decf remaining jump-length)
;;            (setq swaps (determine-swaps init-string prev-string))
;;            (setq jump-length (* 2 jump-length))))
;;
;; The real solution was to note that there is a cycle here.  It
;; repeats itself every n times.  I only thought of this when I
;; started to print the stream (phew!)  Then it's a case of taking one
;; billion modulo cycle length to find the number of times it should
;; be done before exiting.

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1:

(defvar prev-string ""
  "The previous dance string.")
(defvar next-string ""
  "The current dance string.")
(defvar string-length nil
  "The length of the dance string.")
(defvar steps nil
  "The steps which should be taken in the dance.")

(defun parse-moves (string-moves)
  "Parse STRING-MOVES into a stream of movements with structure."
  (let (moves)
    (dolist (step (split-string string-moves ",") (reverse moves))
      (push
       (pcase (aref step 0)
         ('?s `(?s ,(string-to-number (substring step 1))))
         ('?x (cons ?x (mapcar #'string-to-number (split-string (substring step 1) "/" t))))
         ('?p (cons ?p (mapcar (lambda (x) (aref x 0)) (split-string (substring step 1) "/" t)))))
       moves))))

(defun do-dance ()
  "Do dance given in comma separated movements in `steps'."
  (dolist (step steps prev-string)
    (pcase step
      (`(?s ,end)
       (progn
         (cl-replace next-string
                     prev-string
                     :start1 0
                     :end1   end
                     :start2 (- string-length end))
         (cl-replace next-string
                     prev-string
                     :start1 end
                     :start2 0
                     :end2   (- string-length end))))
      (`(?x ,pos-1 ,pos-2)
       (progn
         (aset next-string pos-1 (aref prev-string pos-2))
         (aset next-string pos-2 (aref prev-string pos-1))))
      (`(?p ,char-1 ,char-2)
       (let ((pos-1 (cl-position char-1 prev-string))
             (pos-2 (cl-position char-2 prev-string)))
         (progn
           (aset next-string pos-1 (aref prev-string pos-2))
           (aset next-string pos-2 (aref prev-string pos-1))))))
    (cl-replace prev-string next-string)))

(defun day16-part-1 (input)
  "Solve day 16 for INPUT."
  (interactive "sInput: ")
  (let* ((init-string  "abcdefghijklmnop")
         (prev-string  (substring init-string 0))
         (next-string   (substring init-string 0))
         (string-length (length    init-string))
         (steps         (parse-moves input)))
    (message "%s" (do-dance))))

;; Solution: lbdiomkhgcjanefp

;; Part 2:

(defun day16-part-2 (input)
  "Solv part 2e day 16 for INPUT."
  (interactive "sInput: ")
  (let* ((init-string  "abcdefghijklmnop")
         (prev-string   (substring init-string 0))
         (next-string   (substring init-string 0))
         (string-length (length init-string))
         (steps         (parse-moves input))
         (cycle-length  (progn
                          (do-dance)
                          1)))
    (message
     "%s"
     (progn
       (while (not (string-equal next-string init-string))
         (incf cycle-length)
         (do-dance))
       (cl-replace prev-string init-string)
       (cl-replace next-string init-string)
       (dotimes (_ (mod 1000000000 cycle-length) next-string)
         (do-dance))))))

;; Solution:

(provide 'day16)
;;; day16 ends here
