;;; parse --- my library of functions which make it easier to parse advent of code puzzles -*- lexical-binding: t; -*-

;;; Commentary:

;; This is an amalgamation of all of the parsing functions which I
;; write while I'm doing the advent of code.  I've added a require to
;; the top of each solution file so that I don't have to write that
;; boiler plate code at the start of each day.

;;; Code:

(defun parse-lines (lines-as-string)
  "Split the LINES-AS-STRING into list of strings, one per line."
  (split-string lines-as-string "\n" t))

(defun parse-tokenise-matrix (matrix-as-string)
  "Tokenise MATRIX-AS-STRING into a list of lists of numbers.

MAXTRIX-AS-STRING is split first by newlines and then by tabs."
  (let ((rows (parse-lines matrix-as-string)))
    (mapcar (lambda (row) (mapcar #'string-to-number
                                  (split-string row "\t" t)))
            rows)))

(provide 'parse)
;;; parse ends here
