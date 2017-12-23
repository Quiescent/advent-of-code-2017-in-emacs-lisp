;;; day23 --- a solution to day 23 -*- lexical-binding: t; -*-

;;; Commentary:

;; I couldn't find a way to programatically optimise this one.  So
;; what I did in the end was interpret the assembly code and write a
;; python program from it (see day23.py).
;;
;; Once I'd written the program I figured out what the overall program
;; was doing and just wrote a quick program to determine the solution.
;;
;; In essence I was given a program which counted the number of
;; non-prime numbers where numbers were drawn in increments of 17 in
;; an inclusive range from two numbers (107900 and 124900).

;;; Code:

(require 'parse)
(require 'day18)
(eval-when-compile
  (require 'cl))

;; Part 1

(defun execute-instruction-day23 (instruction register-bank)
  "Execute INSTRUCTION on REGISTER-BANK."
  (pcase instruction
    (`(SET (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (get-value type y register-bank)
                     register-bank)
       nil))
    (`(SUB (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (- (get-value 'REG x register-bank)
                        (get-value type y register-bank))
                     register-bank)
       nil))
    (`(MUL (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (multiply (get-register x register-bank)
                               (get-value type y register-bank))
                     register-bank)
       nil))
    (`(JNZ (,type-x ,x)
           (,type-y ,y))
     (when (/= (get-value type-x x register-bank) 0)
       `(JUMP ,(get-value type-y y register-bank))))))

(defun execute-counting-mul (program)
  "Produce the count calls to `MUL' when executing PROGRAM."
  (let* ((program-pointer 0)
         (mul-executions  0)
         (program-length  (length program))
         (register-bank   (make-registers-init-0))
         result)
    (while (and (>= program-pointer 0)
                (< program-pointer program-length))
      (let ((next-instruction (aref program program-pointer)))
        (when (eq 'MUL (car next-instruction))
          (cl-incf mul-executions))
        (setq result
              (execute-instruction-day23 next-instruction
                                         register-bank))
        (if (and (consp result)
                 (eq (car result) 'JUMP))
            (cl-incf program-pointer (cadr result))
          (cl-incf program-pointer))))
    mul-executions))

(defun day23-part-1 (input)
  "Solve day 23 for INPUT."
  (interactive "sInput: ")
  (let* ((lines         (parse-lines input))
         (instructions  (apply #'vector (parse-instructions lines))))
    (message "%s" (execute-counting-mul instructions))))

;; Solution: 5929

;; Part 2

(defun execute-with-debugging-non-working-optimisation (program)
  "Execute PROGRAM producing the value of h at the end.

Attempts to optimise out loops dynamically."
  (let* ((program-pointer 0)
         (program-length  (length program))
         (register-bank   (make-registers-init-0))
         jump-optimisations
         jump-starts
         jump-ends
         copy-bank
         result)
    (set-register ?a 1 register-bank)
    (while (and (>= program-pointer 0)
                (< program-pointer program-length))
      (let ((next-instruction (aref program program-pointer)))
        (setq result
              (execute-instruction-day23 next-instruction
                                         register-bank))
        (if (and (consp result)
                 (eq (car result) 'JUMP))
            (let* ((jump-length (cadr result))
                   (found-optimisation (assoc program-pointer jump-optimisations))
                   (jump-start (when (and (null found-optimisation)
                                          (< jump-length 0))
                                 (pop jump-starts)))
                   (jump-end   (when (and (null found-optimisation)
                                          (< jump-length 0))
                                 (pop jump-ends))))
              (cond
               (found-optimisation
                (progn
                  (let* ((counter-register (cadadr next-instruction))
                         (times (abs (/ (get-register counter-register register-bank)
                                        (get-register counter-register (cadr found-optimisation))))))
                    (setq register-bank (add-register-banks register-bank
                                                            (scalar-multiply-register (cadr found-optimisation)
                                                                                      times))))))
               ((and (eq jump-end (+ program-pointer jump-length))
                     (eq jump-start program-pointer)
                     (not (eq (1- program-length)
                              program-pointer)))
                (let ((optimisation (subtract-register-banks register-bank
                                                             copy-bank)))
                  (push (list jump-start optimisation)
                        jump-optimisations)
                  (setq register-bank (add-register-banks register-bank
                                                          optimisation))))
               (t (progn
                    (when (and (< jump-length 0)
                               (not (eq (1- program-length)
                                        program-pointer)))
                      (push program-pointer jump-starts)
                      (setq copy-bank (copy-register-bank register-bank)))
                    (cl-incf program-pointer jump-length)
                    (when (and (< jump-length 0)
                               (not (eq (1- program-length)
                                        program-pointer)))
                      (push program-pointer jump-ends))))))
          (cl-incf program-pointer))))
    (get-value 'REG ?h register-bank)))

(defun execute-with-debugging (program)
  "Execute PROGRAM and produce the value of register h."
  (let* ((program-pointer 0)
         (program-length  (length program))
         (register-bank   (make-registers-init-0))
         result)
    (set-register ?a 1 register-bank)
    (while (< program-pointer program-length)
      (setq result
            (execute-instruction-day23 (aref program program-pointer)
                                       register-bank))
      (if (and (consp result)
               (eq (car result) 'JUMP))
          (cl-incf program-pointer (cadr result))
        (cl-incf program-pointer)))
    (get-value 'REG ?h register-bank)))

(defun day23-part-2 (input)
  "Solve part e day 23 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (instructions  (apply #'vector (parse-instructions lines))))
    (message "%s" (execute-with-debugging instructions))))

;; Solution: 907

(provide 'day23)
;;; day23 ends here
