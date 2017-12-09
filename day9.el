;;; day9 --- a solution to day 8 -*- lexical-binding: t; -*-

;;; Commentary:

;; My mistake:
;;  a) I thought that the second half would be more complex.
;;  b) I thought that it wouldn't care about the content of garbage or
;;     of segments

;;; Code:

(require 'parse)
(require 'seq)
(eval-when-compile
  (require 'cl))

;; Part 1

(defvar parse-stream nil "The stream of tokens parsed so far.")
(defvar segments nil "The segments parsed during parsing.")

(defun parse (remaining-input)
  "Parse tokens from REMAINING-INPUT."
  (progn
    (while (not (string-equal remaining-input ""))
      (pcase (split-to-next remaining-input)
        (`(,match ,left ,right)
         (push match parse-stream)
         (push left segments)
         (setq remaining-input right))))
    (setq segments (reverse segments))
    (progn
      (reverse parse-stream))))

(defconst interesting-chars '(?} ?{ ?> ?<) "Chars which interrupt the stream.")

(defun split-to-next (remaining)
  "Split REMAINING up to the next interesting char."
  (let ((idx 0))
    (while (and (< idx (length remaining))
                (not (member (aref remaining idx)
                             interesting-chars)))
      (when (eq (aref remaining idx) ?!)
        (cl-incf idx))
      (cl-incf idx))
    `(,(aref remaining idx)
      ,(substring remaining 0 (1+ idx))
      ,(substring remaining (1+ idx)))))

(split-to-next "a!>,<a>,<a>")

(defun score (stream depth)
  "Traverse down STREAM adding DEPTH to score when we go deeper.

DEPTH is also decremented when we go up."
  (let ((score 0))
    (while (not (null stream))
      (let ((next-element (pop stream)))
        (cond
         ((eq next-element ?<)
          (setq stream (cdr (seq-drop-while (lambda (x) (not (eq x ?>))) stream))))
         ((eq next-element ?{)
          (progn
            (incf depth)
            (incf score depth)))
         ((eq next-element ?})
          (progn
            (decf depth))))))
    score))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{}") 0))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{{{}}}") 0))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{{{},{},{{}}}}") 0))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{<a>,<a>,<a>,<a>}") 0))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{{<!!>},{<!!>},{<!!>},{<!!>}}") 0))

(let ((parse-stream nil)
      (segments nil))
  (score (parse "{{<a!>},{<a!>},{<a!>},{<ab>}}") 0))

(defun day9-part-1 (input)
  "Solve day 8 for INPUT."
  (interactive "sInput: ")
  (let* ((lines               (parse-lines input))
         (parse-stream        nil)
         (segments            nil)
         (stream              (parse (car lines))))
    (message "%s" (score stream 0))))

;; Solution: 10820

;; Part 2


(defun garbage (stream)
  "First attempt to get garbage out of STREAM.
Doesn't work."
  (let ((garbage-segments ""))
    (while (not (null stream))
      (let ((next-element (pop stream))
            (next-segment (pop segments)))
        (when (eq next-element ?<)
          (while (and stream segments
                      (not (eq (car stream) ?>)))
            (setq next-element (pop stream))
            (setq next-segment (pop segments))
            (setq garbage-segments (concat garbage-segments next-segment)))
          (setq next-segment (pop segments))
          (when (> (length next-segment) 1)
            (setq garbage-segments (concat garbage-segments (substring next-segment 0 -1)))))))
    garbage-segments))

(defun day9-part-2 (input)
  "Solve day 8 part 2 for INPUT."
  (interactive "sInput: ")
  (message
   "%s"
   (length (remove-nots (apply #'concat (garbage-segments input))))))

(day9-part-2 "<random characters>")
(day9-part-2 "<>")
(day9-part-2 "<<<<>")
(day9-part-2 "<{!>}>")
(day9-part-2 "<!!>")
(day9-part-2 "<!!!>>")
(day9-part-2 "<{o\"i!a,<{i<a>")
(day9-part-2 "<{o\"i!!!!a,<{i<!a>")
(day9-part-2 "<{o\"!!>i!!!!a,<{i<!a>")

(defun garbage-segments (string)
  "Get garbage out of STRING using an Emacs buffer.
Probably a better way to parse long strings anyway."
  (let ((segments nil))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (search-forward "<" nil t)
        (let ((found nil)
              (start (point))
              end)
          (while (and (null found)
                      (search-forward ">"))
            (let ((match (buffer-substring start (point))))
              (when (cl-evenp (length (seq-take-while (lambda (x) (eq x ?!))
                                                      (substring (reverse match) 1))))
                (setq found t)
                (setq end (point)))))
          (push (buffer-substring start (1- end)) segments)))
      segments)))

(garbage-segments "<{o\"!!!>i!!!!a,<{i<!a>")

(defun remove-nots (string)
  "Remove the bang sequences from STRING."
  (let ((result ""))
    (while (not (string-equal string ""))
      (if (eq (aref string 0) ?!)
          (setq string (substring string 2))
        (progn
          (setq result (concat result (substring string 0 1)))
          (setq string (substring string 1)))))
    result))

(remove-nots "{o\"i!!a,<{i<a")
(remove-nots "<{o\"i!!!!a,<{i<!a>")

;; Solution: 5547

(provide 'day9)
;;; day9 ends here
