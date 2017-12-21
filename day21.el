;;; day21 --- a solution to day 21 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; ../.. => .../#../#..
;; #./.. => ###/#.#/.#.
;; ##/.. => ###/.##/##.
;; .#/#. => .#./..#/...
;; ##/#. => ##./.##/#..
;; ##/## => #.#/###/.##
;; .../.../... => #.#./.#.#/#.#./###.
;; #../.../... => #..#/.###/##../##..
;; .#./.../... => #.##/####/.###/....
;; ##./.../... => ####/.#../#.##/#.##
;; #.#/.../... => ..../#.../.##./#.##
;; ###/.../... => .###/.#../...#/.#..
;; .#./#../... => .###/#..#/#.../#...
;; ##./#../... => ..##/...#/#.##/..##
;; ..#/#../... => #.##/.#../...#/..##
;; #.#/#../... => #.##/..##/..../##.#
;; .##/#../... => .###/.###/#.../....
;; ###/#../... => #.../####/.#.#/....
;; .../.#./... => ...#/##.#/...#/###.
;; #../.#./... => #.#./####/.#../##.#
;; .#./.#./... => #..#/.##./..##/...#
;; ##./.#./... => ###./#.#./#.../###.
;; #.#/.#./... => ..#./###./####/.#.#
;; ###/.#./... => .#.#/#..#/..#./#..#
;; .#./##./... => ####/##../##../..##
;; ##./##./... => #.../..##/#.#./....
;; ..#/##./... => ..../#..#/.#../#..#
;; #.#/##./... => ###./..##/#.#./#...
;; .##/##./... => ...#/#..#/####/...#
;; ###/##./... => ..../#.##/###./...#
;; .../#.#/... => #.../#.../...#/#...
;; #../#.#/... => ##../#..#/.##./.##.
;; .#./#.#/... => ##../.###/#.##/#.#.
;; ##./#.#/... => ##.#/.#.#/#.#./..#.
;; #.#/#.#/... => .##./...#/...#/.#..
;; ###/#.#/... => ####/..#./###./#.##
;; .../###/... => #..#/.#.#/#.##/..#.
;; #../###/... => .#../##../##../#.##
;; .#./###/... => #.#./...#/#.#./#.##
;; ##./###/... => #.#./#..#/.###/.###
;; #.#/###/... => ..#./...#/..#./#..#
;; ###/###/... => ##../###./####/....
;; ..#/.../#.. => ##../.#../#.#./.##.
;; #.#/.../#.. => .##./##.#/.#../#...
;; .##/.../#.. => ####/..#./#..#/##..
;; ###/.../#.. => #.#./..../..#./####
;; .##/#../#.. => ..##/..##/.##./##..
;; ###/#../#.. => #..#/#..#/.##./.#..
;; ..#/.#./#.. => #..#/#.##/##../#..#
;; #.#/.#./#.. => .#.#/.#.#/.##./.#.#
;; .##/.#./#.. => ####/#.##/..../.###
;; ###/.#./#.. => #..#/.#.#/.##./....
;; .##/##./#.. => ###./##../#..#/....
;; ###/##./#.. => ...#/.#../.#../....
;; #../..#/#.. => ###./#.../..##/#...
;; .#./..#/#.. => .#../#.##/.##./..#.
;; ##./..#/#.. => ..#./.##./..../..##
;; #.#/..#/#.. => #.#./###./.#.#/#..#
;; .##/..#/#.. => ####/..##/###./.#.#
;; ###/..#/#.. => ##.#/.##./.###/###.
;; #../#.#/#.. => ..../#.##/.#.#/#..#
;; .#./#.#/#.. => .###/..../.###/#.##
;; ##./#.#/#.. => ####/..##/#.##/#.##
;; ..#/#.#/#.. => ..#./..##/####/#...
;; #.#/#.#/#.. => .##./.#.#/.#.#/##..
;; .##/#.#/#.. => ##.#/##.#/#.##/.###
;; ###/#.#/#.. => #..#/.##./#.##/.###
;; #../.##/#.. => ####/...#/..##/##..
;; .#./.##/#.. => .##./#.##/...#/#...
;; ##./.##/#.. => .##./..#./###./....
;; #.#/.##/#.. => .#.#/##.#/..#./##.#
;; .##/.##/#.. => ###./####/.##./####
;; ###/.##/#.. => ..#./##.#/.#../..#.
;; #../###/#.. => ##../#.##/#.../.#.#
;; .#./###/#.. => ..#./#.##/...#/...#
;; ##./###/#.. => .###/###./.##./###.
;; ..#/###/#.. => #.../..../#.../#...
;; #.#/###/#.. => .###/...#/...#/..#.
;; .##/###/#.. => #.#./..../###./.#.#
;; ###/###/#.. => #..#/#.../#.##/##.#
;; .#./#.#/.#. => .#../##../..##/#.##
;; ##./#.#/.#. => #.##/#.#./#..#/##.#
;; #.#/#.#/.#. => #..#/.###/..../###.
;; ###/#.#/.#. => #.#./.#.#/####/#.#.
;; .#./###/.#. => ..##/..#./..##/###.
;; ##./###/.#. => ##../#.#./#.#./.#..
;; #.#/###/.#. => ####/.##./####/#.#.
;; ###/###/.#. => ####/..#./####/....
;; #.#/..#/##. => ###./..#./.#../...#
;; ###/..#/##. => #.#./#.##/#..#/##..
;; .##/#.#/##. => ..../.#../..../....
;; ###/#.#/##. => .###/..#./#.#./####
;; #.#/.##/##. => ..../.#.#/#.#./...#
;; ###/.##/##. => ##../.#../.#.#/..##
;; .##/###/##. => ..#./#.#./##../..##
;; ###/###/##. => ..#./###./#.#./..##
;; #.#/.../#.# => #.#./..../#.##/.#.#
;; ###/.../#.# => #.##/#.../..##/...#
;; ###/#../#.# => ####/.###/..#./.#.#
;; #.#/.#./#.# => ..#./#..#/#..#/##..
;; ###/.#./#.# => ..../##../.#.#/##.#
;; ###/##./#.# => ..##/..##/.#../####
;; #.#/#.#/#.# => ####/...#/#.#./#.#.
;; ###/#.#/#.# => #.##/...#/..#./...#
;; #.#/###/#.# => #.##/####/#..#/..##
;; ###/###/#.# => .##./.##./.##./.#..
;; ###/#.#/### => .#../..../..../.###
;; ###/###/### => #.#./#.#./###./###.

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))
(require 'seq)
(require 'dash)

;; Part 1

(defun to-dotted (xs)
  "Produce a dotted list version of XS."
  (progn
    (setcdr (nthcdr (- (length xs) 2) xs)
            (nth    (1- (length xs))  xs))
    xs))

(to-dotted '(1 3 2))

(defun parse-rule (rule-line)
  "Parse the rule from string-formatted RULE-LINE."
  (pcase (split-string rule-line " => " t)
    (`(,from-lines ,to-lines)
     `(,(to-dotted (split-string from-lines "/" t))
       ,(split-string to-lines "/" t)))))

(parse-rule "../.# => ##./#../...")

(defun all-rotations-2 (rule)
  "Produce all translations of input 2x2-rule RULE.

Includes flipping, rotating and rotating and then flipping."
  (let* ((input-pattern (car rule))
         (line-one                (car input-pattern))
         (line-two                (cdr input-pattern))
         (first-rotation          (cons (string (aref line-two 0) (aref line-one 0))
                                        (string (aref line-two 1) (aref line-one 1))))
         (line-one-first-rotation (car first-rotation))
         (line-two-first-rotation (cdr first-rotation)))
    (cl-remove-duplicates
     (list
      input-pattern

      first-rotation

      (cons (reverse line-two-first-rotation)
            (reverse line-one-first-rotation))

      (cons (reverse line-one-first-rotation)
            (reverse line-two-first-rotation))

      (cons line-two-first-rotation
            line-one-first-rotation)

      (cons (string (aref line-two 1) (aref line-two 0))
            (string (aref line-one 1) (aref line-one 0)))

      (cons (string (aref line-one 1) (aref line-two 1))
            (string (aref line-one 0) (aref line-two 0)))

      (cons (reverse line-two)
            (reverse line-one))

      (cons (reverse line-one)
            (reverse line-two))

      (cons line-two
            line-one))
     :test #'equal)))

(defun all-rotations-3 (rule)
  "Produce all translations of input 2x2-rule RULE.

Includes flipping, rotating and rotating and then flipping."
  (let* ((input-pattern             (car   rule))
         (line-one                  (car   input-pattern))
         (line-two                  (cadr  input-pattern))
         (line-three                (cddr input-pattern))
         (first-rotation            (cons (string (aref line-three 0) (aref line-two   0) (aref line-one   0))
                                          (cons (string (aref line-three 1) (aref line-two   1) (aref line-one   1))
                                                (string (aref line-three 2) (aref line-two   2) (aref line-one   2)))))
         (line-one-first-rotation   (car  first-rotation))
         (line-two-first-rotation   (cadr first-rotation))
         (line-three-first-rotation (cddr first-rotation)))
    (cl-remove-duplicates
     (list
      input-pattern

      first-rotation

      (cons (reverse line-one-first-rotation)
            (cons (reverse line-two-first-rotation)
                  (reverse line-three-first-rotation)))

      (cons (reverse line-three-first-rotation)
            (cons (reverse line-two-first-rotation)
                  (reverse line-one-first-rotation)))

      (cons line-three-first-rotation
            (cons line-two-first-rotation
                  line-one-first-rotation))

      (cons (string (aref line-three 2) (aref line-three 1) (aref line-three 0))
            (cons (string (aref line-two   2) (aref line-two   1) (aref line-two   0))
                  (string (aref line-one   2) (aref line-one   1) (aref line-one   0))))

      (cons (string (aref line-one   2) (aref line-two   2) (aref line-three 2))
            (cons (string (aref line-one   1) (aref line-two   1) (aref line-three 1))
                  (string (aref line-one   0) (aref line-two   0) (aref line-three 0))))

      (cons (reverse line-one)
            (cons (reverse line-two)
                  (reverse line-three)))

      (cons (reverse line-three)
            (cons (reverse line-two)
                  (reverse line-one)))

      (cons line-three
            (cons line-two
                  line-one)))
     :test #'equal)))

(all-rotations-2 (parse-rule "../.# => ##./#../..."))
(all-rotations-3 (parse-rule ".#./..#/### => #..#/..../..../#..#"))
(all-rotations-3 '((".#." "..#" . "###")))

(defun all-rotations (rule)
  "Produce all translations of inputs to rules."
  (if (consp (cdar rule))
      (all-rotations-3 rule)
    (all-rotations-2 rule)))

(defun parse-rules (lines)
  "Parse all rules in LINES."
  (mapcar (lambda (line)
            (let ((rule (parse-rule line)))
              (cons (all-rotations rule)
                    (cadr rule))))
          lines))

;; (defun split-into-twos (grid)
;;   (let ((rows (seq-partition grid 2)))
;;     (if (eq 1 (length rows))
;;         (-zip (seq-partition (caar  rows) 2)
;;               (seq-partition (cadar rows) 2))
;;       (apply (apply-partially #'cl-mapcar
;;                               (lambda (row1 row2)
;;                                 (-zip (seq-partition row1 2)
;;                                       (seq-partition row2 2))))
;;              rows))))

(defun split-into-twos (grid)
  "Break GRID up into squares of length 2."
  (let ((rows (seq-partition grid 2)))
    (mapcar (lambda (row-pair)
              (-zip (seq-partition (car  row-pair) 2)
                    (seq-partition (cadr row-pair) 2)))
            rows)))

(split-into-twos '("#..#"
                   "...."
                   "...."
                   "#..#"))

(split-into-twos '("#."
                   ".."))

(split-into-twos '("##.##."
                   "#..#.."
                   "......"
                   "##.##."
                   "#..#.."
                   "......"))

;; (defun split-into-threes (grid)
;;   (let ((rows (seq-partition grid 3)))
;;     (if (eq 1 (length rows))
;;         (-zip (seq-partition (caar   rows) 3)
;;               (seq-partition (cadar  rows) 3)
;;               (seq-partition (caddar rows) 3))
;;       (apply (apply-partially #'cl-mapcar
;;                               (lambda (row1 row2 row3)
;;                                 (-zip (seq-partition row1 3)
;;                                       (seq-partition row2 3)
;;                                       (seq-partition row3 3))))
;;              rows))))

(defun split-into-threes (grid)
  "Break GRID up into squares of length 3."
  (let ((rows (seq-partition grid 3)))
    (mapcar (lambda (row-pair)
              (mapcar #'to-dotted
                      (-zip (seq-partition (car   row-pair) 3)
                            (seq-partition (cadr  row-pair) 3)
                            (seq-partition (caddr row-pair) 3))))
            rows)))

(split-into-threes '(".#." "..#" "###"))

(defun substitute (square rules)
  "Substitute SQUARE for it's target substitution in RULES."
  (cdr (cl-find-if (lambda (key)
                     (cl-find square key :test #'equal))
                   rules :key #'car)))

(substitute '("#." . "..")
            (parse-rules
             (split-string "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
                           "\n")))

(defun join-block-line-2 (block-lines)
  "Join the 2x2 blocks in BLOCK-LINES."
  (mapcar (lambda (xs) (apply (apply-partially #'cl-concatenate 'string) xs))
          (list (mapcar #'car block-lines)
                (mapcar #'cadr  block-lines))))

(join-block-line-2 '(("#." "..") (".#" "..")))

(defun join-block-line-3 (block-lines)
  "JOIN the 3x3 blocks in BLOCK-LINES."
  (mapcar (lambda (xs) (apply (apply-partially #'cl-concatenate 'string) xs))
          (list (mapcar #'car block-lines)
                (mapcar #'cadr  block-lines)
                (mapcar #'caddr  block-lines))))

(defun join-block-line-4 (block-lines)
  "Join the 4x4 blocks in BLOCK-LINES."
  (mapcar (lambda (xs) (apply (apply-partially #'cl-concatenate 'string) xs))
          (list (mapcar #'car     block-lines)
                (mapcar #'cadr    block-lines)
                (mapcar #'caddr   block-lines)
                (mapcar #'cadddr  block-lines))))

(join-block-line-3 '(("##" "#." "..") (".." ".#" "..")))

(defun expand (grid rules)
  "Execute a single iteration of expanding GRID by RULES."
  (let* ((length-2    (= (mod (length (car grid)) 2) 0))
         (block-lines (if length-2
                          (split-into-twos grid)
                        (split-into-threes grid)))
         (joiner      (if length-2
                          #'join-block-line-3
                        #'join-block-line-4)))
    (apply (apply-partially #'seq-concatenate 'list)
           (mapcar (lambda (block-line)
                     (let ((new-block-line (mapcar (lambda (x) (substitute x rules)) block-line)))
                       (funcall joiner new-block-line)))
                   block-lines))))

(expand '("#..#"
          "...."
          "...."
          "#..#")
        (parse-rules
         (split-string "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
                       "\n")))

(expand '("##.##."
          "#..#.."
          "......"
          "##.##."
          "#..#.."
          "......")
        (parse-rules
         (split-string "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
                       "\n")))

(defconst starting-grid '(".#."
                          "..#"
                          "###")
  "The starting gird for the problem.")

(defun day21-part-1 (input)
  "Solve day 21 for INPUT."
  (interactive "sInput: ")
  (let* ((lines         (parse-lines input))
         (rules         (parse-rules lines))
         (starting-grid '(".#."
                          "..#"
                          "###")))
    (dotimes (_ 5 starting-grid)
      (setq starting-grid (expand starting-grid rules)))
    (message "%s" (cl-reduce #'+
                             (cl-mapcar (lambda (xs) (cl-count ?# xs))
                                        starting-grid)))))

;; Solution: 194

;; Part 2

(defun day21-part-2 (input)
  "Solve part 2 day 21 for INPUT."
  (interactive "sInput: ")
  (let* ((lines         (parse-lines input))
         (rules         (parse-rules lines))
         (starting-grid '(".#."
                          "..#"
                          "###")))
    (dotimes (_ 18 starting-grid)
      (setq starting-grid (expand starting-grid rules)))
    (message "%s" (cl-reduce #'+
                             (cl-mapcar (lambda (xs) (cl-count ?# xs))
                                        starting-grid)))))

;; Solution: 2536879

(provide 'day21)
;;; day21 ends here
