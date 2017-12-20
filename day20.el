;;; day20 --- a solution to day 20 -*- lexical-binding: t; -*-

;;; Commentary:

;; Failed attempt

;; (defun intersection-time (d1 d2 v1 v2 a1 a2)
;;   (/ (* 2 (- (+ d1 v1 ) (+ d2 v2)))
;;      (- a1 a2)))
;;
;; (defun particle-intersection-time (particle1 particle2)
;;   (pcase (list particle1 particle2)
;;     (`(((,_ ,p1x ,p1y ,p1z) (,_ ,v1x ,v1y ,v1z) (,_ ,a1x ,a1y ,a1z))
;;        ((,_ ,p2x ,p2y ,p2z) (,_ ,v2x ,v2y ,v2z) (,_ ,a2x ,a2y ,a2z)))
;;      (let ((intersect-time-x (intersection-time p1x p2x v1x v2x a1x a2x))
;;            (intersect-time-y (intersection-time p1y p2y v1y v2y a1y a2y))
;;            (intersect-time-z (intersection-time p1z p2z v1z v2z a1z a2z)))
;;        (when (= intersect-time-x
;;                 intersect-time-y
;;                 intersect-time-z)
;;          intersect-time-x)))))
;;
;; (defun remove-collisions (particles)
;;   (let ((intersections (maplist (lambda (rest) `()))))))

;; First Part

;;; Code:

(require 'parse)
(eval-when-compile
  (require 'cl))

;; Part 1

(defun parse-triple (triple-with-equals)
  "Parse a triple from TRIPLE-WITH-EQUALS."
  (mapcar #'string-to-number
          (split-string (substring (substring triple-with-equals 3) 0 -1) ",")))

(defun parse-description (partical-line)
  "Parse the position, velocity and acceleration in triples from PARTICAL-LINE."
  (pcase (split-string partical-line ", " t)
    (`(,p ,v ,a)
     `((?p . ,(parse-triple p))
       (?v . ,(parse-triple v))
       (?a . ,(parse-triple a))))))

(cdr (assoc ?a (parse-description "p=<-717,-4557,2578>, v=<153,21,30>, a=<-8,8,-7>")))

(defun sqr (x)
  "Square X."
  (* x x))

(defun displacement (v a time)
  "Produce the displacement of a particle moving at V with acceleration of A after TIME."
  (+ (* v time)
     (/ (* a (sqr time)) 2)))

(defun new-displacement (particle time)
  "Produce the new displacement triple of PARTICLE after TIME."
  (pcase particle
    (`((,_ ,px ,py ,pz) (,_ ,vx ,vy ,vz) (,_ ,ax ,ay ,az))
     `(,(+ px (displacement vx ax time))
       ,(+ py (displacement vy ay time))
       ,(+ pz (displacement vz az time))))))

(defun manhattan (displacement)
  "Produce the manhattan distance of a particle at DISPLACEMENT."
  (pcase displacement
    (`(,px ,py ,pz)
     (+ (abs px) (abs py) (abs pz)))))

(defun closest-in-long-term (particles)
  "Produce the position of the particle which is ultimately the cosest in PARTICLES."
  (let (closest-at-1000000
        (current-particle-position 0)
        (min-particle-position 0))
    (dolist (particle particles min-particle-position)
      (let* ((acceleration-vector (cdr (assoc ?a particle)))
             (current-pos-at-1000000 (manhattan (new-displacement particle 1000000))))
        (when (or (null closest-at-1000000)
                  (< current-pos-at-1000000 closest-at-1000000))
          (setq closest-at-1000000 current-pos-at-1000000)
          (setq min-particle-position current-particle-position)))
      (cl-incf current-particle-position))))

(defun day20-part-1 (input)
  "Solve day 20 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (particles (mapcar #'parse-description lines)))
    (message "%s" (closest-in-long-term particles))))

(day20-part-1 "p=<-717,-4557,2578>, v=<153,21,30>, a=<-8,8,-7>
p=<1639,651,-987>, v=<29,-19,129>, a=<-5,0,-6>
p=<-10482,-248,-491>, v=<4,10,81>, a=<21,0,-4>
p=<-6607,-2542,1338>, v=<-9,52,-106>, a=<14,2,4>
p=<-4468,1178,-6474>, v=<146,44,66>, a=<1,-3,1>
p=<1298,-3391,-2843>, v=<-44,26,113>, a=<0,6,-1>
p=<-36,-1100,4900>, v=<62,22,56>, a=<-4,1,-15>
p=<-357,911,-1551>, v=<-1,16,141>, a=<2,-7,-6>
p=<-582,1505,-1092>, v=<97,-55,30>, a=<-7,-3,3>
p=<-2436,-2689,1860>, v=<-28,83,-39>, a=<17,7,-7>
p=<-483,119,240>, v=<6,22,-25>, a=<2,-3,1>
p=<-546,2,33>, v=<95,-95,15>, a=<-7,10,-2>")

;; Solution: 344

;; Part 2

(defun tick-displacement (particle)
  "Move PARTICLE one tick."
  (pcase particle
    (`((,_ ,px ,py ,pz) (,_ ,vx ,vy ,vz) (,_ ,ax ,ay ,az))
     `((?p ,(+ px ax vx)
           ,(+ py ay vy)
           ,(+ pz az vz))
       (?v ,(+ vx ax) ,(+ vy ay) ,(+ vz az))
       (?a ,ax ,ay ,az)))))

(defun displacement-key (particle)
  "Produce a key for the displacement of PARTICLE."
  (car particle))

(defun simulate-removing-collisions (particles)
  "Keep ticking a thousand times colliding PARTICLES.

Produce the particle collection when done."
  (dotimes (_ 1000 particles)
    (let ((positions-found (make-hash-table :test #'equal)))
      (setq particles (mapcar #'tick-displacement
                              particles))
      (mapc (lambda (particle)
              (let ((key (displacement-key particle)))
                (puthash key
                         (1+ (gethash key positions-found 0))
                         positions-found)))
            particles)
      (setq particles (cl-remove-if (lambda (particle)
                                      (> (gethash (displacement-key particle)
                                                  positions-found)
                                         1))
                                    particles)))))

(defun day20-part-2 (input)
  "Solve part 2 day 20 for INPUT."
  (interactive "sInput: ")
  (let* ((lines (parse-lines input))
         (particles (mapcar #'parse-description lines)))
    (message "%s" (length (simulate-removing-collisions particles)))))

(day20-part-2 "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")

(provide 'day20)
;;; day20 ends here
