;;; day8 --- a solution to day 8 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(require 'seq)
(require 'subr-x)

;; Part 1

(defun parse-instruction (instruction-line)
  "Parse an instruction from INSTRUCTION-LINE."
  (pcase (split-string instruction-line " " t)
    (`(,register ,operation ,amount ,_ ,register-check ,comparison ,compare-to)
     `(,register
       ,(if (string-equal operation "inc") #'+ #'-)
       ,(string-to-number amount)
       ,register-check
       ,(cond
         ((string-equal comparison ">") #'>)
         ((string-equal comparison "<") #'<)
         ((string-equal comparison ">=") #'>=)
         ((string-equal comparison "<=") #'<=)
         ((string-equal comparison "==") #'eql)
         ((string-equal comparison "!=") #'/=))
       ,(string-to-number compare-to)))))

(defun parse-instructions (instruction-lines)
  "Pares all instructions from INSTRUCTION-LINES."
  (mapcar #'parse-instruction instruction-lines))

(defun get-register (registers register-name)
  "Get a value from REGISTERS with name REGISTER-NAME."
  (or (gethash register-name registers) 0))

(defun set-register (registers register-name register-new-value)
  "Set the in REGISTERS, by REGISTER-NAME, to REGISTER-NEW-VALUE."
  (puthash register-name register-new-value registers))

(defun execute-instruction (registers instruction)
  "In the context of REGISTERS, execute INSTRUCTION."
  (pcase instruction
    (`(,register ,operation ,amount ,register-check ,comparison ,compare-to)
     (when (funcall comparison
                    (get-register registers register-check)
                    compare-to)
       (set-register registers
                     register
                     (funcall operation
                              (get-register registers register)
                              amount))))))

(defun execute-instructions (registers instructions)
  "In the context of REGISTERS, execute INSTRUCTIONS."
  (dolist (instruction instructions (seq-max (hash-table-values registers)))
    (execute-instruction registers instruction)))

(defun day8-part-1 (input)
  "Solve day 8 for INPUT."
  (interactive "sInput: ")
  (let ((instructions (parse-instructions (parse-lines input)))
        (registers (make-hash-table :test #'equal)))
    (message "%s" (execute-instructions registers instructions))))

;; Solution: 4888

;; Part 2

(defun execute-instructions-keeping-track-of-max (registers instructions)
  "In the context of REGISTERS, execute INSTRUCTIONS, keeping track of the maximum register."
  (let (max)
    (dolist (instruction instructions max)
      (execute-instruction registers instruction)
      (let* ((values (hash-table-values registers))
             (new-max (seq-max (if (eq values nil) '(0) values))))
        (when (or (null max)
                  (> new-max max))
          (setq max new-max))))))

(defun day8-part-2 (input)
  "Solve day 8 part 2 for INPUT."
  (interactive "sInput: ")
  (let ((instructions (parse-instructions (parse-lines input)))
        (registers (make-hash-table :test #'equal)))
    (message "%s" (execute-instructions-keeping-track-of-max registers instructions))))

(provide 'day8)
;;; day8 ends here
