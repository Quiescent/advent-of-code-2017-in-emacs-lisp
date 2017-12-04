;;; day4 --- a solution to day 4 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(require 'seq)

(defun parse-input (file)
  "Parse FILE into lines."
  (split-string file "\n" t))

(defun is-valid (pass-phrase)
  "Produce true if PASS-PHRASE has no duplicate words."
  (let ((tokens (split-string pass-phrase " " t)))
    (eq (length (cl-remove-duplicates tokens :test #'string-equal))
        (length tokens))))

(defun day4-part-1 (input)
  "Solve day 3 for INPUT."
  (interactive "sInput: ")
  (message "%s"
           (length (cl-remove-if-not #'is-valid (parse-input input)))))

(is-valid "aa bb cc dd aaa")
(is-valid "aa bb cc dd aa")

;; 451

;; part 2:

(defun letter-content (string)
  "Produce vectors representing the counts of letters in STRING.

`a' is in index 0."
  (let ((letters (string-to-list string))
        (counts  (make-vector 26 0)))
    (dolist (letter letters counts)
      (aset counts (- letter ?a) (1+ (aref counts (- letter ?a)))))))

(defun has-valid-content (string)
  "Produce t if STRING has no duplicate word content words."
  (let* ((tokens (split-string string " " t))
         (contents (mapcar #'letter-content tokens)))
    (eq (length contents)
        (length (seq-uniq contents)))))

(has-valid-content "abcde fghij")
(has-valid-content "abcde xyz ecdab")

(defun is-more-valid (string)
  "Produce t if STRING is a pass phrase which meets both conditions."
  (and (has-valid-content string)
       (is-valid string)))

(defun day4-part-2 (input)
  "Solve day 3 part 2 for INPUT."
  (interactive "sInput: ")
  (message "%s"
           (length (cl-remove-if-not #'is-more-valid (parse-input input)))))

(provide 'day4)
;;; day4 ends here
