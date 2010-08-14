;;
;; Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this list
;; of conditions and the following disclaimer.
;; Redistributions in binary form must reproduce the above copyright notice, this
;; list of conditions and the following disclaimer in the documentation and/or
;; other materials provided with the distribution.
;; Neither the name of the <ORGANIZATION> nor the names of its contributors may be
;; used to endorse or promote products derived from this software without specific
;; prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

;;
;; Clix - inspired by the old arcade classic.
;;

(in-package :clix)


;; misc

(defun limit-val (min max val)
  (cond ((> val max) max)
        ((< val min) min)
        (t val)))

(defun polygon-area (pts)
  "Calculates the signed area of a polygon given a list of points (in order).
   Assumes that the last point is connected to the first point.
   http://mathworld.wolfram.com/PolygonArea.html"

  (labels ((matrix-det (pt1 pt2)
             (- (* (first pt1) (second pt2))
                (* (first pt2) (second pt1)))))
    (destructuring-bind (iarea last-pt)
        (reduce #'(lambda (res pt)
                    (list (+ (first res) (matrix-det (second res) pt)) pt))
                (rest pts) :initial-value (list 0 (first pts)))
      (* 0.5 (+ iarea (matrix-det last-pt (first pts)))))))

;; points

;; #+debug-game
(defparameter *test-points*
  ;;'((0 0) (100 0) (0 100) (100 100) (0 50) (75 0) (75 50) (85 50) (85 0) (85 100) (100 50)))
  '((100 100) (0 0) (0 100) (100 0) (50 0) (50 25) (75 25) (75 0)))

(defun point-in-list(pts pt)
  (member-if #'(lambda (pte) (equal pte pt)) pts))

(defun add-point(pts pt)
  "Add a cartesian point to a list of points. Unchanged if point already exists in list."
  (if (not pts)
      (list pt)
      (if (point-in-list pts pt)
          pts
          (append pts (list pt)))))

(defmacro set-neighbors-on-axis(res pt ptn delta fun1 fun2 axis-fun)
  (let ((ress (gensym)) (ptns (gensym)) (ptav (gensym))
        (pt1  (gensym)) (pt2  (gensym)))
    `(let* ((,ress ,res)
            (,ptns ,ptn)
            (,ptav (,axis-fun ,pt))
            (,pt1  (,fun1 ,ress))
            (,pt2  (,fun2 ,ress)))
       (when (and (> 0 ,delta) (or (eq nil ,pt1) (> ,ptav (,axis-fun ,pt1))))
         (setf (,fun1 ,ress) ,ptns))
       (when (and (< 0 ,delta) (or (eq nil ,pt2) (< ,ptav (,axis-fun ,pt2))))
         (setf (,fun2 ,ress) ,ptns)))))

(defun find-neighbors(pts pt)
  "finds the north, west, east and south point neighbors if they exist."
  (let ((ptx (first pt))
        (pty (second pt)))
    (reduce #'(lambda (res ptn)
                (let ((res (copy-list res))
                      (ptnx (first ptn))
                      (ptny (second ptn)))
                  (cond
                    ;; sets the :west(second) and :east(fourth) neighbors
                    ((eq pty ptny)
                     (set-neighbors-on-axis res pt ptn (- ptnx ptx) second fourth first))
                    ;; sets the :north(first) and :south(third) neighbors
                    ((eq ptx ptnx)
                     (set-neighbors-on-axis res pt ptn (- ptny pty) third first second)))
                  res))
            pts :initial-value '(nil nil nil nil))))

(defun find-left-right-neighbors(pts pt dir)
  (let ((neighbors (find-neighbors pts pt))
        ;; See the *directions* hash table setup above.
        (inds (first (gethash dir *directions*))))
    (list (elt neighbors (first inds)) (elt neighbors (second inds)))))

(defun find-closing-points (ppts pts spt cpt dir turn)
  (if (equal spt cpt)
      ppts
      (let* ((lr-neighbors (find-left-right-neighbors pts cpt dir))
             (dirs  (second (gethash dir *directions*)))
             (nei1  (apply (first turn)  (list lr-neighbors)))
             (nei2  (apply (second turn) (list lr-neighbors)))
             (next  (if nei1 nei1 nei2))
             (ndir  (if nei1
                        (apply (first turn)  (list dirs))
                        (apply (second turn) (list dirs)))))
        (find-closing-points (append ppts (list next)) pts spt next ndir turn))))

(defun find-enclosing-polygon (all-pts trail-pts dir)
  (let* ((spt (first trail-pts))
         (ept (car (last  trail-pts)))
         (left-turn  '(first second))
         (right-turn '(second first)))
    (reduce #'(lambda (res turn)
                (let ((poly (append trail-pts
                                    (cdr (find-closing-points
                                          (list ept) all-pts spt ept dir turn)))))
                  ;; For now the smallest enclosing area is picked from the two.
                  ;; Later when the stix is implemented we will pick the area
                  ;; that it is not inside.
                  (if (or (not res) (< (abs (polygon-area poly)) (abs (polygon-area res))))
                      poly res)))
            (list left-turn right-turn) :initial-value nil)))

