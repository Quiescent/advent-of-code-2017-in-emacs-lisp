;;; day17 --- a solution to day 17 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1

(defun insert-circular (buffer length pos insert)
  "Insert into circular BUFFER.

BUFFER can be longer than LENGTH, but LENGTH is the number of
used slots.  Value will be inserted to POS as INSERT."
  (progn
    (when (< pos (length buffer))
      (cl-replace buffer
                  buffer
                  :start1 (1+ pos)
                  :end1   (1+ length)
                  :start2 pos
                  :end2   length))
    (aset buffer pos insert)))

(defun insert-and-skip (skip-length max-number)
  "Insert into buffer and then skip ahead by SKIP-LENGTH up to MAX-NUMBER."
  (let* ((vector-length  (1+ max-number))
         (buffer         (make-vector vector-length nil))
         (current-insert 1)
         (pos            0)
         (current-length 1))
    (aset buffer 0 0)
    (dotimes (_ (1- vector-length) buffer)
      (setq pos (1+ (mod (+ pos skip-length) current-length)))
      (insert-circular buffer current-length pos current-insert)
      (cl-incf current-insert)
      (cl-incf current-length))))

(defun day17-part-1 (input)
  "Solve day 17 for INPUT."
  (interactive "sInput: ")
  (let* ((skip-length (string-to-number input))
         (buffer      (insert-and-skip skip-length 2017))
         (pos-2017    (cl-position 2017 buffer))
         (next        (aref buffer (mod (1+ pos-2017) 2017))))
    (message "%s" next)))

;; solution 1487

;; Part 2

(defun my-nth-cdr (n list)
  "Get the N th cdr in LIST.

Built in version detects cycles and bails."
  (dotimes (_ n list)
    (setq list (cdr list))))

(defun insert-and-skip-list (skip-length max-number)
  "Construct a circular list by going SKIP-LENGTH ahead and inserting n.

Length of the circular buffer is MAX-NUMBER + 1, because the
range is inclusive on both ends."
  (let* ((buffer         '(0))
         (current-insert 1)
         (buffer-ptr     buffer))
    (setcdr buffer buffer)
    (dotimes (_ max-number buffer)
      (let ((cell (my-nth-cdr skip-length buffer-ptr)))
        (setcdr cell (cons current-insert (cdr cell)))
        (cl-incf current-insert)
        (setq buffer-ptr (cdr cell))))))

(defun day17-part-2 (input)
  "Solv part 2e day 17 for INPUT."
  (interactive "sInput: ")
  (let* ((skip-length (string-to-number input))
         (buffer      (insert-and-skip-list skip-length 50000000))
         (pos-0       (cl-position 0 buffer))
         (next        (nth (mod (1+ pos-0) 50000000) buffer)))
    (message "%s" next)))

(provide 'day17)
;;; day17 ends here
