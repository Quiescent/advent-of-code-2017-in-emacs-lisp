;;; day12 --- a solution to day 12 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'parse)
(require 'seq)
(require 'subr-x)
(eval-when-compile
  (require 'cl))

;; Part 1:

(defun parse-relationship (line)
  "Parse a relationship from LINE."
  (pcase (split-string line " <-> " t)
    (`(,from ,to-list)
     `(,(string-to-number from)
       ,(mapcar #'string-to-number
                (split-string to-list ", " t))))))

(defun build-graph (relationships)
  "Build a graph from RELATIONSHIPS."
  (let ((graph (make-hash-table :test #'equal)))
    (mapc (lambda (x) (mapc (lambda (to)
                              (puthash (car x) (cons to (gethash (car x) graph)) graph))
                            (cadr x)))
          relationships)
    graph))

(defvar visited '() "The nodes visited during this DFS.")

(defun dfs (graph start)
  "Search through GRAPH from START pushing nodes to `visited' as we go.

Search is done depth first."
  (when (not (member start visited))
    (push start visited)
    (mapc (lambda (to) (dfs graph to))
          (gethash start graph))))

(defun day12-part-1 (input)
  "Solve day 12 for INPUT."
  (interactive "sInput: ")
  (let* ((lines   (parse-lines input))
         (visited nil)
         (graph   (build-graph (mapcar #'parse-relationship lines))))
    (dfs graph 0)
    (message "%s" (length visited))))

(day12-part-1 "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

;; Part 2:

(defun find-groups (graph)
  "Find all groups in GRAPH.

A group is a strongly connected component.  Since all
relationships are bidirectional and every edge appears in the
from side of a relationship we don't have to worry about any
funnies."
  (let* ((groups             nil)
         (all-keys           (hash-table-keys graph))
         (length-of-all-keys (length all-keys))
         (visited-count      0)
         (remaining-keys     all-keys)
         (all-programs       (seq-uniq (apply (apply-partially   #'seq-concatenate 'list)
                                              all-keys
                                              (hash-table-values graph)))))
    (while (not (eq visited-count
                    length-of-all-keys))
      (let ((visited    '())
            (next-start (car remaining-keys)))
        (dfs graph next-start)
        (incf visited-count (length visited))
        (push visited groups)
        (setq remaining-keys (cl-set-difference remaining-keys visited))))
    groups))

(defun day12-part-2 (input)
  "Solv part 2e day 12 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (visited nil)
         (graph   (build-graph (mapcar #'parse-relationship lines))))
    (message "%s" (length (find-groups graph)))))


(provide 'day12)
;;; day12 ends here
