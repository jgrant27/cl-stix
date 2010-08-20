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

;; debug stuff : causes SBCL on Windows XP/7 to freak out (but works fine on Linux)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (find-package :swank) (not (find-package :windows)))
    (pushnew :debug-game *features*)))

;; Parameters
(defparameter *title-music*         nil)
(defparameter *game-music*          nil)
(defparameter *title-music-path*    "audio/8_bit.xm")
(defparameter *game-music-path*     "audio/qix_ed.mod")
#+debug-game
(progn
  (defparameter *title-music-path*  "/home/jgrant/cl-stix/audio/8_bit.xm")
  (defparameter *game-music-path*   "/home/jgrant/cl-stix/audio/qix_ed.mod"))

(defparameter *game-surface*        nil)
(defparameter *display-width*       320)
(defparameter *display-height*      200)
(defparameter *game-top-border*     30)
(defparameter *game-border*         10)
(defparameter *game-width*          (- *display-width*  (* 2 *game-border*)))
(defparameter *game-height*         (- *display-height*
                                       (+ *game-border* *game-top-border*)))
(defparameter *min-x*               (/ (- *display-width*  *game-width*)  2))
(defparameter *max-x*               (- *display-width*  *min-x*))
(defparameter *min-y*               (- *display-height* *game-height* *game-border*))
(defparameter *max-y*               (- *display-height* *game-border*))
(defparameter *diamond-color*       (sdl:color :r 255 :g 160 :b 160))
(defparameter *diamond-p-color*     (sdl:color :r 255 :g 190 :b 190))
(defparameter *edge-color*          (sdl:color :r 200 :g 200 :b 200))

(defparameter *game-points*         (list (list *min-x* *min-y*) (list *min-x* *max-y*)
                                          (list *max-x* *max-y*) (list *max-x* *min-y*)
                                          (list *min-x* *min-y*)))
(defparameter *trail-points*        nil)


;; These are the left right point neighbor indices depending on the travel direction
(defparameter *directions* (make-hash-table))
;; The find-neighbors function below defines these index configs
(setf (gethash :north *directions*) '((1 3) (:west  :east)))
(setf (gethash :west  *directions*) '((2 0) (:south :north)))
(setf (gethash :south *directions*) '((3 1) (:east  :west)))
(setf (gethash :east  *directions*) '((0 2) (:north :south)))

;; game structs
(defstruct player
  (y *max-y*)
  (x *min-x*)
  (yp *max-y*)
  (xp *min-x*)
  (size 3)
  (dir)
  (prev-dir)
  (speed 1)
  (is-creating-trail))

(defparameter *p1* (make-player))

;; Font stuff
;; (defparameter *font-path*          "quacksal.tff")
;; #+debug-game
;; (defparameter *font-path*          "/home/jgrant/cl-stix/quacksal.ttf")
;; (defparameter *font*               nil)

;; #+debug-game
(defparameter *test-points*
  ;;'((0 0) (100 0) (0 100) (100 100) (0 50) (75 0) (75 50) (85 50) (85 0) (85 100) (100 50)))
  ;;'((0 0) (0 100) (100 100) (100 0) (0 0) (50 0) (50 25) (75 25) (75 0)))
  '((0 0) (0 100) (100 100) (100 0) (0 0) (50 100) (50 75) (75 75) (75 100)))
