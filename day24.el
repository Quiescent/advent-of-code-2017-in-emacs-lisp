;;; day24 --- a solution to day 24 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Part 1:

(require 'parse)
(eval-when-compile
  (require 'cl))

(defun parse-component (component-string)
  "Parse a component from COMPONENT-STRING."
  (pcase (split-string component-string "/" t)
    (`(,x ,y)
     `(,(string-to-number x)
       ,(string-to-number y)))))

(defun parse-components (component-lines)
  "Parse all components from COMPONENT-LINES."
  (mapcar #'parse-component component-lines))

(defun find-starting-components (components)
  "Find the components from which a bridge can be built in COMPONENTS."
  (cl-remove-if-not (lambda (component) (or (eq 0 (car component))
                                            (eq 0 (cadr component))))
                    components))

(find-starting-components '((0 2) (3 0) (1 2)))

(defun match-component (component components)
  "Find the components which can continue from COMPONENT in COMPONENTS.

Produce a collection of 2-lists where the first element is the
original (so that it can be removed from candidates later) and
the second element is the component in the order in which it
connected to the bridge."
  (let* ((target-y            (cadr component))
         (matching-components (cl-remove-if-not (lambda (candidate)
                                                  (let ((candidate-x (car  candidate))
                                                        (candidate-y (cadr candidate)))
                                                    (or (eq target-y candidate-y)
                                                        (eq target-y candidate-x))))
                                                components)))
    (cl-mapcar (lambda (matching-component)
                 (let ((matching-component-x (car matching-component))
                       (matching-component-y (cadr matching-component)))
                   (if (eq matching-component-x target-y)
                       (list matching-component
                             matching-component)
                     (list matching-component
                           (list matching-component-y
                                 matching-component-x)))))
               matching-components)))

(defun gen-bridges-iter (current-component components)
  "Generate all bridges starting from CURRENT-COMPONENT using COMPONENTS."
  (let ((matching-components (match-component current-component components)))
    (if (null matching-components)
        (list (list current-component))
      (cl-mapcan (lambda (matching-component)
                   (cl-mapcar (lambda (bridge) (cons current-component bridge))
                              (gen-bridges-iter (cadr matching-component)
                                                (cl-remove (car matching-component) components))))
                 matching-components))))

(defun gen-bridges (components)
  "Produce all bridges which use COMPONENTS."
  (let ((starting-components (find-starting-components components)))
    (cl-mapcan (lambda (starting-component)
                 (cl-mapcar (lambda (bridge) bridge)
                            (gen-bridges-iter starting-component
                                              (cl-remove starting-component components))))
               starting-components)))

(defun score (bridge)
  "Produce the score of BRIDGE."
  (cl-reduce #'+ (cl-mapcar (lambda (component) (+ (car  component)
                                                   (cadr component)))
                            bridge)))

(defun highest-scoring-bridge (bridges)
  "Produce the bridge with the highest score in BRIDGES."
  (cl-reduce (lambda (best next-bridge)
               (if (> (score next-bridge)
                      (score best))
                   next-bridge
                 best))
             bridges))

(defun day24-part-1 (input)
  "Solve day 24 for INPUT."
  (interactive "sInput: ")
  (let* ((lines       (parse-lines input))
         (components  (parse-components lines))
         (bridges     (gen-bridges components))
         (best-bridge (highest-scoring-bridge bridges)))
    (message "%s" (score best-bridge))))

;; Solution: 1695

;; Part 2:


(defun longest-bridges (bridges)
  "Produce the longest bridge(s) in BRIDGES."
  (cl-reduce (lambda (best next-bridge)
               (let ((length-longest (length (car best)))
                     (length-current (length next-bridge)))
                 (if (> length-current length-longest)
                     (list next-bridge)
                   (if (eq length-current length-longest)
                       (cons next-bridge best)
                     best))))
             bridges
             :initial-value (list (car bridges))))

(defun day24-part-2 (input)
  "Solve part 2 day 24 for INPUT."
  (interactive "sInput: ")
  (let* ((lines           (parse-lines input))
         (components      (parse-components lines))
         (bridges         (gen-bridges components))
         (longest-bridges (longest-bridges bridges))
         (best-bridge     (highest-scoring-bridge longest-bridges)))
    (message "%s" (score best-bridge))))

;; Solution: 1673

(provide 'day24)
;;; day24 ends here
