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

(defun point-in-list(pts pt)
  (member-if #'(lambda (pte) (equal pte pt)) pts))

(defun betweenp(num1 num2 num)
  (let ((low  (if (> num1 num2) num2 num1))
        (high (if (> num1 num2) num1 num2)))
    (and (<= low num) (>= high num))))

(defun point-on-linep(pts pt)
  (destructuring-bind (pt-x pt-y) pt
    (second
     (reduce #'(lambda (res pte)
                 (destructuring-bind ((pte-x pte-y) ((last-pt-x last-pt-y) rbool))
                     (list pte res)
                   (list pte (or rbool
                                 (cond
                                   ((eq pt-x last-pt-x) (betweenp pte-y last-pt-y pt-y))
                                   ((eq pt-y last-pt-y) (betweenp pte-x last-pt-x pt-x)))))))
             (cdr pts) :initial-value (list (car pts) nil)))))

(defun add-point(pts pt)
  "Add a cartesian point to a list of points. Unchanged if point already exists in list."
  (if (not pts)
      (list pt)
      (if (point-in-list pts pt)
          pts
          (append pts (list pt)))))

(defmacro set-neighbor-on-axis(res ptn ptnav ptav fun1 fun2 axis-fun)
  "Set nearest point neighbor on axis if closer than existing neighbor"
  (let ((ptndelta (gensym))
        (pt       (gensym)))
    `(let* ((,ptndelta (- ,ptnav ,ptav))
            (,pt  (if (< ,ptndelta 0) (,fun1 ,res)  (,fun2 ,res))))
       (when (and (not (eq ,ptndelta 0))
                  (or (eq nil ,pt)
                      (< (abs ,ptndelta) (abs (- ,ptav (,axis-fun ,pt))))))
         (if (< ,ptndelta 0)
             (setf (,fun1 ,res) ,ptn)
             (setf (,fun2 ,res) ,ptn))))))

(defun find-neighbors(pts pt)
  "finds the north, west, east and south point neighbors if they exist."
  (destructuring-bind (ptx pty) pt
    (reduce #'(lambda (res ptn)
                (destructuring-bind ((ptnx ptny) res) (list ptn (copy-list res))
                  (cond
                    ;; sets the :west(second) and :east(fourth) neighbors
                    ((eq pty ptny) (set-neighbor-on-axis
                                    res ptn ptnx ptx second fourth first))
                    ;; sets the :north(first) and :south(third) neighbors
                    ((eq ptx ptnx) (set-neighbor-on-axis
                                    res ptn ptny pty third first second)))
                  res))
            pts :initial-value '(nil nil nil nil))))

(defun find-left-right-neighbors(pts pt dir)
  ;; See the *directions* hash table setup above.
  (destructuring-bind ((lind rind) neighbors)
      (list (first (gethash dir *directions*)) (find-neighbors pts pt))
    (list (elt neighbors lind) (elt neighbors rind))))

(defun find-closing-points (ppts pts spt cpt dir turn)
  "Find the remaining points by 'turning' until the start point is reached."
  (if (equal spt cpt)
      ppts
      (destructuring-bind (turn1 turn2) turn
        (let* ((lr-neighbors (find-left-right-neighbors pts cpt dir))
               (dirs         (second (gethash dir *directions*)))
               (nei1         (apply turn1 (list lr-neighbors)))
               (nei2         (apply turn2 (list lr-neighbors)))
               (next         (if nei1 nei1 nei2))
               (ndir         (if nei1
                                 (apply turn1 (list dirs))
                                 (apply turn2 (list dirs)))))
          (find-closing-points (append ppts (list next)) pts spt next ndir turn)))))

(defun find-enclosing-polygon (all-pts trail-pts dir)
  "Find the enclosing polygon given a 'trail' of points.
   The trail-pts must already have been added to all-pts otherwise we loop forever."
  (let* ((spt        (first trail-pts))
         (ept        (car (last  trail-pts)))
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

