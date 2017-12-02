;;; day2-with-library-functions --- solving day 2 with libraries as much as possible -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path default-directory)
(require 'seq)
(require 'parse)

(defun largest-and-smallest-per-row (rows-as-lists)
  "Produce tuples of the largest and smallest per row in ROWS-AS-LISTS."
  (mapcar (lambda (row) `(,(seq-min row) ,(seq-min row)))
          rows-as-lists))

(defun day2-part-1 (input)
  "Solve day 2 for INPUT."
  (interactive "sInput: ")
  (message (format "%s"
                   (apply #'+
                          (mapcar (lambda (xs) (apply #'- xs))
                                  (largest-and-smallest-per-row (parse-tokenise-matrix input)))))))

(defun find-even-dividers (xs)
  "Find two numbers in XS, y and z, such that y evenly divids z."
  (let ((sorted (sort xs  #'>))
        y
        z)
    (while (null z)
      (setq y (pop sorted))
      (setq z (car (cl-remove-if-not (lambda (x) (eql (mod y x) 0)) sorted))))
    `(,y ,z)))

(defun day2-part-2 (input)
  "Solve day 2 part 2 for INPUT."
  (interactive "sInput: ")
  (message (format "%s"
                   (apply #'+
                          (mapcar (lambda (xs) (apply #'/ xs))
                                  (mapcar #'find-even-dividers (tokenise-matrix input)))))))

(provide 'day2-with-library-functions)
;;; day2-with-library-functions ends here
