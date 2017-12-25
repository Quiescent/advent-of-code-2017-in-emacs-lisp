;;; day25 --- a solution to day 25 -*- lexical-binding: t; -*-

;;; Commentary:

;; Program:
;; Begin in state A.
;; Perform a diagnostic checksum after 12629077 steps.
;;
;; In state A:
;;   If the current value is 0:
;;     - Write the value 1.
;;     - Move one slot to the right.
;;     - Continue with state B.
;;   If the current value is 1:
;;     - Write the value 0.
;;     - Move one slot to the left.
;;     - Continue with state B.
;;
;; In state B:
;;   If the current value is 0:
;;     - Write the value 0.
;;     - Move one slot to the right.
;;     - Continue with state C.
;;   If the current value is 1:
;;     - Write the value 1.
;;     - Move one slot to the left.
;;     - Continue with state B.
;;
;; In state C:
;;   If the current value is 0:
;;     - Write the value 1.
;;     - Move one slot to the right.
;;     - Continue with state D.
;;   If the current value is 1:
;;     - Write the value 0.
;;     - Move one slot to the left.
;;     - Continue with state A.
;;
;; In state D:
;;   If the current value is 0:
;;     - Write the value 1.
;;     - Move one slot to the left.
;;     - Continue with state E.
;;   If the current value is 1:
;;     - Write the value 1.
;;     - Move one slot to the left.
;;     - Continue with state F.
;;
;; In state E:
;;   If the current value is 0:
;;     - Write the value 1.
;;     - Move one slot to the left.
;;     - Continue with state A.
;;   If the current value is 1:
;;     - Write the value 0.
;;     - Move one slot to the left.
;;     - Continue with state D.
;;
;; In state F:
;;   If the current value is 0:
;;     - Write the value 1.
;;     - Move one slot to the right.
;;     - Continue with state A.
;;   If the current value is 1:
;;     - Write the value 1.
;;     - Move one slot to the left.
;;     - Continue with state E.

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))
(require 'seq)
(require 'subr-x)

;; Part 1:

(defun direction-to-number (direction)
  "Convert DIRECTION into an amount to adjust the pointer by."
  (if (string-equal direction "right")
      1
    -1))

(defun parse-state-transitions (lines program)
  "Put state transitions described by LINES into PROGRAM."
  (progn
    (cl-mapc
     (lambda (state-transition)
       (let* ((lines (split-string state-transition "\n" t))
              (state (cl-remove ?: (nth 2 (split-string (car lines) " ")))))
         (puthash state
                  (let ((transition (make-vector 2 0))
                        (val-1      (string-to-number
                                     (cl-remove ?:
                                                (nth 5
                                                     (split-string (cadr lines)
                                                                   " "
                                                                   t)))))
                        (val-2      (string-to-number
                                     (cl-remove ?:
                                                (nth 5
                                                     (split-string (nth 5 lines)
                                                                   " "
                                                                   t))))))
                    (aset transition 0 (make-vector 3 0))
                    (aset (aref transition val-1)
                          0
                          (string-to-number (cl-remove ?. (nth 4 (split-string (nth 2 lines) " " t)))))
                    (aset (aref transition val-1)
                          1
                          (direction-to-number (cl-remove ?. (nth 6 (split-string (nth 3 lines) " " t)))))
                    (aset (aref transition val-1)
                          2
                          (cl-remove ?. (nth 4 (split-string (nth 4 lines) " " t))))
                    
                    (aset transition 1 (make-vector 3 0))
                    (aset (aref transition val-2)
                          0
                          (string-to-number (cl-remove ?. (nth 4 (split-string (nth 6 lines) " " t)))))
                    (aset (aref transition val-2)
                          1
                          (direction-to-number (cl-remove ?. (nth 6 (split-string (nth 7 lines) " " t)))))
                    (aset (aref transition val-2)
                          2
                          (cl-remove ?. (nth 4 (split-string (nth 8 lines) " " t))))
                    transition)
                  program)))
     (split-string lines "\n\n" t))
    program))

(parse-state-transitions
 "In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."
 (make-hash-table :test #'equal))

(defun parse-header (program)
  "Parse the header of the string representation of PROGRAM.

Produces both the program itself (in hash table format) and the
lines which are still to be parsed."
  (let ((lines          (split-string program "\n"))
        (output-program (make-hash-table :test #'equal)))
    `(,(apply #'concat (cl-mapcar (lambda (line) (concat line "\n"))
                                  (seq-drop lines 3)))
      .
      ,(progn
         (puthash 'START
                  (cl-remove ?. (seq-subseq (car lines) (- (length (car lines)) 2)))
                  output-program)
         (puthash 'STEPS
                  (string-to-number (nth 5 (split-string (cadr lines) " ")))
                  output-program)
         output-program))))

(defun run-program (program initial-state steps)
  "Run PROGRAM with INITIAL-STATE for STEPS times."
  (let ((tape          (make-hash-table :test #'eq))
        (current-state initial-state)
        (pointer       0))
    (dotimes (_ steps (cl-reduce #'+ (hash-table-values tape)))
      (let* ((value-at-point   (gethash pointer tape 0))
             (state-transition (aref (gethash current-state program)
                                     value-at-point)))
        (puthash pointer (aref state-transition 0) tape)
        (cl-incf pointer (aref state-transition 1))
        (setq current-state (aref state-transition 2))))))

(defun day25-part-1 (input)
  "Solve day 25 for INPUT."
  (interactive "sInput: ")
  (let* ((parsed-header   (parse-header input))
         (header          (cdr parsed-header))
         (rest-of-program (car parsed-header))
         (program         (parse-state-transitions rest-of-program header)))
    (message "%s" (run-program program
                               (gethash 'START program)
                               (gethash 'STEPS program)))))

;; Solution: 3745

(provide 'day25)
;;; day25 ends here
