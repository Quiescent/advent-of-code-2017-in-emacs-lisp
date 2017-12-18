;;; day18 --- a solution to day 18 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)

;; Part 1:

(defun is-a-number (string)
  (string-match-p "\\([0-9]+\\)\\|-\\([0-9]+\\)" string))

(defun parse-argument (argument)
  (if (is-a-number argument)
      `(NUM ,(string-to-number argument))
    `(REG ,(aref argument 0))))

(defun parse-instruction (instruction-line)
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
           ,(parse-argument y)))))

(defun parse-instructions (lines)
  (mapcar #'parse-instruction lines))

(defun make-registers ()
  (make-char-table 'registers))

(defmacro make-number (x)
  x)

(defun get-register (key register-bank)
  (let ((lookup (aref register-bank key)))
    (if (null lookup)
        (make-number 0)
      lookup)))

(defun set-register (key value register-bank)
  (aset register-bank key value))

(defvar last-played nil)

(defun get-value (type val register-bank)
  (if (eq type 'REG)
      (get-register val register-bank)
    val))

(defconst zero (make-number 0))

(defmacro add (x y)
  `(+ ,x ,y))

(defmacro subtract (x y)
  `(- ,x ,y))

(defmacro multiply (x y)
  `(* ,x ,y))

(defmacro modulo (x y)
  `(mod ,x ,y))

(defmacro compare (x y)
  `(if (> ,x ,y)
       1
     (if (< ,x ,y)
         -1
       0)))

(defun execute-instruction (instruction register-bank)
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
    (`(JGZ (,_ ,x)
           (,type ,y))
     (when (> (get-register x register-bank)
              0)
       `(JUMP ,y)))))

(defun execute-instructions-to-recv (instructions)
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

;; Solution:

;; Part 2

(defvar channel-0 nil)
(defvar channel-1 nil)

(defun execute-instruction-transmition (instruction register-bank rec-channel snd-channel)
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
    (`(JGZ (,_ ,x)
           (,type ,y))
     (when (compare (get-register x register-bank) zero)
       `(JUMP ,(get-value type y register-bank))))))

(defun is-blocking (last-instruction channel)
  (and (eq (car last-instruction) 'RCV)
       (null channel)))

(defvar send-count nil)
(defvar pointer-0 nil)
(defvar pointer-1 nil)

(defun maybe-execute (program register-bank pointer last-instruction-result program-length)
  (when (and (not (>= (symbol-value pointer) program-length))
             (not (is-blocking last-instruction-result channel-0)))
    (setq last-instruction-result
          (execute-instruction-transmition (aref program
                                                 (symbol-value pointer))
                                           register-bank
                                           'channel-0
                                           'channel-1))
    (when (and (eq (car last-instruction-result) 'SND)
               ;; A total hack but w/e
               (eq pointer 'pointer-1))
      (cl-incf send-count))
    (if (and last-instruction-result
             (eq (car last-instruction-result)
                 'JUMP))
        (let ((jump-value (cadr last-instruction-result)))
          (cl-incf (symbol-value pointer) (if (eq jump-value 0) 1 jump-value)))
      (cl-incf (symbol-value pointer)))
    last-instruction-result))

(defun execute-instructions-transmit (instructions)
  (let* ((register-bank-0 (make-registers))
         (register-bank-1 (make-registers))
         (channel-0 nil)
         (channel-1 nil)
         (send-count 0)
         (instruction-vec (apply #'vector instructions))
         (pointer-0 0)
         (pointer-1 0)
         (program-length (length instruction-vec))
         (last-instruction-result-0 nil)
         (last-instruction-result-1 nil))
    (set-register ?p (make-number 0) register-bank-0)
    (set-register ?p (make-number 1) register-bank-1)
    (while (not (or (>= pointer-0 program-length)
                    (and (or (>= pointer-0 program-length)
                             (is-blocking last-instruction-result-0 channel-0))
                         (or (>= pointer-1 program-length)
                             (is-blocking last-instruction-result-1 channel-1)))))
      (setq last-instruction-result-0
            (maybe-execute instruction-vec
                           register-bank-0
                           'pointer-0
                           last-instruction-result-0
                           program-length))
      (setq last-instruction-result-1
            (maybe-execute instruction-vec
                           register-bank-1
                           'pointer-1
                           last-instruction-result-1
                           program-length)))
    (message "Zero: %s" pointer-0)
    send-count))

(defun day18-part-2 (input)
  "Solve part 2 day 18 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (instructions (parse-instructions lines))
         (send-count  (execute-instructions-transmit instructions)))
    (message "%s" send-count)))

;; Solution: 

(provide 'day18)
;;; day18 ends here


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
