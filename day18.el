;;; day18 --- a solution to day 18 -*- lexical-binding: t; -*-

;;; Commentary:

;; A failed attempt where they operated in lock-step
;; (when (and (not (> pointer-0 program-length))
;;                  (not (is-blocking last-instruction-result-0 channel-0)))
;;         (setq last-instruction-result-0
;;               (execute-instruction-transmition (aref instruction-vec
;;                                                      pointer-0)
;;                                                register-bank-0
;;                                                'channel-0
;;                                                'channel-1))
;;         (when (eq (car last-instruction-result-0) 'SND)
;;           (cl-incf send-count))
;;         (if (and last-instruction-result-0
;;                  (eq (car last-instruction-result-0)
;;                      'JUMP))
;;             (setq pointer-0 (+ pointer-0 (cadr last-instruction-result-0)))
;;           (cl-incf pointer-0)))
;;       (when (and (not (> pointer-1 program-length))
;;                  (not (is-blocking last-instruction-result-1 channel-1)))
;;         (setq last-instruction-result-1
;;               (execute-instruction-transmition (aref instruction-vec
;;                                                      pointer-1)
;;                                                register-bank-1
;;                                                'channel-1
;;                                                'channel-0))
;;         (if (and last-instruction-result-1
;;                  (eq (car last-instruction-result-1)
;;                      'JUMP))
;;             (setq pointer-1 (+ pointer-1 (cadr last-instruction-result-1)))
;;           (cl-incf pointer-1)))

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))
(require 'seq)

;; Part 1:

(defun is-a-number (string)
  "Produce t if STRING represents a number."
  (string-match-p "\\([0-9]+\\)\\|-\\([0-9]+\\)" string))

(defun parse-argument (argument)
  "Parse ARGUMENT into a literal or register reference."
  (if (is-a-number argument)
      `(NUM ,(string-to-number argument))
    `(REG ,(aref argument 0))))

(defun parse-instruction (instruction-line)
  "Parse the instruction in INSTRUCTION-LINE."
  (pcase (split-string instruction-line " " t)
    (`("snd" ,x)
     `(SND ,(parse-argument x)))
    (`("set" ,x ,y)
     `(SET ,(parse-argument x)
           ,(parse-argument y)))
    (`("add" ,x ,y)
     `(ADD ,(parse-argument x)
           ,(parse-argument y)))
    (`("mul" ,x ,y)
     `(MUL ,(parse-argument x)
           ,(parse-argument y)))
    (`("mod" ,x ,y)
     `(MOD ,(parse-argument x)
           ,(parse-argument y)))
    (`("rcv" ,x)
     `(RCV ,(parse-argument x)))
    (`("jgz" ,x ,y)
     `(JGZ ,(parse-argument x)
           ,(parse-argument y)))
    (`("jnz" ,x ,y)
     `(JNZ ,(parse-argument x)
           ,(parse-argument y)))
    (`("sub" ,x ,y)
     `(SUB ,(parse-argument x)
           ,(parse-argument y)))))

(defun parse-instructions (lines)
  "Parse all instructions in LINES."
  (mapcar #'parse-instruction lines))

(defun make-registers ()
  "Make a register bank."
  (make-vector 256 nil))

(defun make-registers-init-0 ()
  "Create a register bank with values initialised to zero."
  (make-vector 256 0))

(defun copy-register-bank (register-bank)
  "Copy REGISTER-BANK."
  (seq-subseq register-bank 0))

(defun add-register-banks (register-bank-1 register-bank-2)
  "Add the corresponding registers in REGISTER-BANK-1 and REGISTER-BANK-2.

Produces a new register bank."
  (cl-map 'vector #'+ register-bank-1 register-bank-2))

(defun subtract-register-banks (register-bank-1 register-bank-2)
  "Subtract the corresponding registers in REGISTER-BANK-1 and REGISTER-BANK-2.

Produces a new register bank."
  (cl-map 'vector #'- register-bank-1 register-bank-2))

(defun scalar-multiply-register (register-bank scalar)
  "Multiply each register value in REGISTER-BANK by SCALAR."
  (cl-map 'vector (apply-partially #'* scalar) register-bank))

(defmacro make-number (x)
  "Create a number from X.

Could be a bignum."
  x)

(defun get-register (key register-bank)
  "Get the value of register by nam KEY in REGISTER-BANK."
  (let ((lookup (aref register-bank key)))
    (if (null lookup)
        (make-number 0)
      lookup)))

(defun set-register (key value register-bank)
  "Set the value of register by name KEY to VALUE in REGISTER-BANK."
  (aset register-bank key value))

(defvar last-played nil
  "The last played sound.")

(defun get-value (type val register-bank)
  "Get a value.

If TYPE is REG, then search for register by name VAL in
REGISTER-BANK."
  (if (eq type 'REG)
      (get-register val register-bank)
    val))

(defconst zero (make-number 0)
  "The number zero.")

(defmacro add (x y)
  "Add X and Y.

Op could be subbed for bignum ops."
  `(+ ,x ,y))

(defmacro subtract (x y)
  "Subtract X and Y.

Op could be subbed for bignum ops."
  `(- ,x ,y))

(defmacro multiply (x y)
  "Multiply X and Y.

Op could be subbed for bignum ops."
  `(* ,x ,y))

(defmacro modulo (x y)
  "Produce the modulo of X and Y.

Op could be subbed for bignum ops."
  `(mod ,x ,y))

(defmacro compare (x y)
  "Produce 1 if X is greater than Y, 0 if they're equal and -1 otherwise."
  `(if (> ,x ,y)
       1
     (if (< ,x ,y)
         -1
       0)))

(defun execute-instruction (instruction register-bank)
  "Execute INSTRUCTION with REGISTER-BANK."
  (pcase instruction
    (`(SND (,type ,x))
     (progn
       (setq last-played (get-value type x register-bank))
       nil))
    (`(SET (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (get-value type y register-bank)
                     register-bank)
       nil))
    (`(ADD (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (add (get-register x register-bank)
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
    (`(MOD (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (modulo (get-register x register-bank)
                             (get-value type y register-bank))
                     register-bank)
       nil))
    (`(RCV (,type ,x))
     (when (compare (get-value type x register-bank) zero)
       `(RCV ,last-played)))
    (`(JGZ (,x-type ,x)
           (,y-type ,y))
     (when (> (get-value x-type x register-bank) 0)
       `(JUMP ,(get-value y-type y register-bank))))))

(defun execute-instructions-to-recv (instructions)
  "Execute INSTRUCTIONS and get the last played sound."
  (let ((register-bank (make-registers))
        (last-played nil)
        (instruction-vec (apply #'vector instructions))
        (pointer 0)
        (last-instruction-result nil))
    (while (or (not last-instruction-result)
               (not (eq (car last-instruction-result)
                        'RCV)))
      (setq last-instruction-result
            (execute-instruction (aref instruction-vec
                                       pointer)
                                 register-bank))
      (if (and last-instruction-result
               (eq (car last-instruction-result)
                   'JUMP))
          (setq pointer (+ pointer (cadr last-instruction-result)))
        (cl-incf pointer)))
    (cadr last-instruction-result)))

(defun day18-part-1 (input)
  "Solve day 18 for INPUT."
  (interactive "sInput: ")
  (let* ((lines        (parse-lines input))
         (instructions (parse-instructions lines))
         (last-sound   (execute-instructions-to-recv instructions)))
    (message "%s" last-sound)))

;; Solution: 2951

;; Part 2

(defvar channel-0 nil
  "Transmition channel for thread 0 to send signals.")
(defvar channel-1 nil
  "Transmission channel for thread 1 to send segnals.")

(defun is-blocking (last-instruction channel)
  "Produce t if LAST-INSTRUCTION is a receive (RCV) and CHANNEL is empty (nil)."
  (and (eq (car last-instruction) 'RCV)
       (null channel)))

(defvar send-count nil
  "The number of times that thread one transmitted.")
(defvar pointer-0 nil
  "The pointer for program zero.")
(defvar pointer-1 nil
  "The pointer for program one.")

(defun execute-instruction-transmition (instruction register-bank rec-channel snd-channel)
  "Execute INSTRUCTION on REGISTER-BANK.

Receives from other threads on REC-CHANNEL and sends to other
threads on SND-CHANNEL."
  (pcase instruction
    (`(SND (,type ,x))
     (progn
       (setf (symbol-value snd-channel)
             (nconc (symbol-value snd-channel)
                    (list (get-value type x register-bank))))
       '(SND)))
    (`(SET (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (get-value type y register-bank)
                     register-bank)
       nil))
    (`(ADD (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (add (get-register x register-bank)
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
    (`(MOD (,_ ,x)
           (,type ,y))
     (progn
       (set-register x
                     (modulo (get-register x register-bank)
                             (get-value type y register-bank))
                     register-bank)
       nil))
    (`(RCV (,type ,x))
     (if (symbol-value rec-channel)
         (progn
           (set-register x (pop (symbol-value rec-channel)) register-bank)
           nil)
       '(RCV)))
    (`(JGZ (,x-type ,x)
           (,y-type ,y))
     (when (> (get-value x-type x register-bank) 0)
       `(JUMP ,(get-value y-type y register-bank))))))

(defun execute (program register-bank pointer program-length rec-channel snd-channel)
  "Run PROGRAM with REGISTER-BANK at position POINTER.

Program pointer is OOB at POINTER >= PROGRAM-LENGTH.

Receive messages on REC-CHANNEL.

Send messages on SND-CHANNEL."
  (let ((last-instruction-result
         (execute-instruction-transmition (aref program
                                                (symbol-value pointer))
                                          register-bank
                                          rec-channel
                                          snd-channel)))
    (when (and (eq (car last-instruction-result) 'SND)
               ;; A total hack but w/e
               (eq pointer 'pointer-1))
      (cl-incf send-count))
    (if (and last-instruction-result
             (eq (car last-instruction-result)
                 'JUMP))
        (let ((jump-value (cadr last-instruction-result)))
          (cl-incf (symbol-value pointer) (if (eq jump-value 0) 1 jump-value)))
      (when (not (eq (car last-instruction-result) 'RCV))
        (cl-incf (symbol-value pointer))))
    last-instruction-result))

(defun execute-instructions-race (instructions)
  "Execute INSTRUCTIONS on two threads.

Thread zero should have register `p' set to 0 and thread one
should have `p' set to 1 at the start."
  (let* ((register-bank-0 (make-registers))
         (register-bank-1 (make-registers))
         (channel-0 nil)
         (channel-1 nil)
         (send-count 0)
         (program (apply #'vector instructions))
         (pointer-0 0)
         (pointer-1 0)
         (program-length (length program))
         (last-instruction-result-0 nil)
         (last-instruction-result-1 nil))
    (set-register ?p (make-number 0) register-bank-0)
    (set-register ?p (make-number 1) register-bank-1)
    (while (not (or (>= pointer-0 program-length)
                    (>= pointer-1 program-length)
                    (and (is-blocking last-instruction-result-0 channel-0)
                         (is-blocking last-instruction-result-1 channel-1))))
      (while (not (or (>= pointer-0 program-length)
                      (is-blocking last-instruction-result-0 channel-1)))
        (setq last-instruction-result-0
              (execute program register-bank-0 'pointer-0
                       program-length 'channel-1 'channel-0)))
      (while (not (or (>= pointer-1 program-length)
                      (is-blocking last-instruction-result-1 channel-0)))
        (setq last-instruction-result-1
              (execute program register-bank-1 'pointer-1
                       program-length 'channel-0 'channel-1))))
    send-count))

(defun day18-part-2 (input)
  "Solve part 2 day 18 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (instructions (parse-instructions lines))
         (send-count  (execute-instructions-race instructions)))
    (message "%s" send-count)))

;; Wrong solution: 7493 (?) (was counting the wrong sends)

;; Solution: 7366

(provide 'day18)
;;; day18 ends here
