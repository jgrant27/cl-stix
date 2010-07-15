;;
;; Stix - inspired by the old arcade classic.
;;


(require 'asdf)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)
(asdf:operate 'asdf:load-op :lispbuilder-sdl-gfx)


;; debug stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :swank)
    (pushnew :my-game-debug *features*)))


(defpackage :i27.games.stix (:use :common-lisp ))

(in-package i27.games.stix)

;; Parameters
(defparameter *display-height* 480)
(defparameter *display-width*  640)
(defparameter *game-border*     40)
(defparameter *game-height*    (- *display-height* *game-border*))
(defparameter *game-width*     (- *display-width*  *game-border*))
(defparameter *min-y*          (/ (- *display-height* *game-height*) 2))
(defparameter *min-x*          (/ (- *display-width*  *game-width*)  2))
(defparameter *max-y*          (- *display-height* *min-y*))
(defparameter *max-x*          (- *display-width*  *min-x*))

;; game structs
(defstruct player
  (y *max-y*)
  (x *min-x*)
  (xp)
  (yp)
  (size 3)
  (dir)
  (speed 2))

;; vars
(defparameter *p1* (make-player))

;; funs
(defun reset-game ()
  "Set scores to 0 etc."
  (setf *p1* (make-player)))

(defun limit-val (min max val)
  (cond ((> val max)
         #+my-game-debug
         (format t "~%max val: ~A" val)
         max)
        ((< val min)
         #+my-game-debug
         (format t "~%min val: ~A" val)
         min)
        (t val)))

(defun update-player (key)
  (let ((py (player-y *p1*))
        (px (player-x *p1*)))
    (setf (player-yp *p1*) (player-y *p1*))
    (setf (player-xp *p1*) (player-x *p1*))
    (cond ((sdl:key= key :sdl-key-down)
           (setf (player-y *p1*)
                 (limit-val *min-y* *max-y* (+ py (player-speed *p1*)))))
          ((sdl:key= key :sdl-key-up)
           (setf (player-y *p1*)
                 (limit-val *min-y* *max-y* (- py (player-speed *p1*)))))
          ((sdl:key= key :sdl-key-left)
           (setf (player-x *p1*)
                 (limit-val *min-x* *max-x* (- px (player-speed *p1*)))))
          ((sdl:key= key :sdl-key-right)
           (setf (player-x *p1*)
                 (limit-val *min-x* *max-x* (+  px (player-speed *p1*)))))))
  #+my-game-debug
  (format t "~%player y:~A x:~A"
          (player-y *p1*) (player-x *p1*)))

(defun clear-diamond(px py size)
  ;; clear the previous position
  (when (and (numberp px) (numberp py))
    (sdl:draw-shape (list (sdl:point :x (- px size) :y py)
                          (sdl:point :x px :y (- py size))
                          (sdl:point :x (+ px size) :y py)
                          (sdl:point :x px :y (+ py size))
                          (sdl:point :x (- px size) :y py))
                    :color sdl:*black* :style :solid)))

(defun draw-diamond(px py size)
  ;; draw the new position
  (sdl:draw-shape (list (sdl:point :x (- px size) :y py)
                        (sdl:point :x px :y (- py size))
                        (sdl:point :x (+ px size) :y py)
                        (sdl:point :x px :y (+ py size))
                        (sdl:point :x (- px size) :y py))
                  :color sdl:*white* :style :solid))

(defun draw-screen ()
  (let ((py (player-y *p1*))
        (px (player-x *p1*))
        (pyp (player-yp *p1*))
        (pxp (player-xp *p1*))
        (size (player-size *p1*)))
    (clear-diamond pxp pyp size)
    ;; draw the border
    (sdl:draw-rectangle-* *min-x* *min-y* *game-width* *game-height*
                          :color sdl:*blue*)
    (draw-diamond px py size)
    ;;(sdl:clear-display sdl:*black*)
    ))

;; Create the window
(sdl:with-init (sdl:sdl-init-video)
  (sdl:window *display-width* *display-height*
              :bpp 16
              ;; :double-buffer t
              :title-caption "- S T I X -"
              :icon-caption "STIX")
  (setf (sdl:frame-rate) 30)
  (sdl:enable-key-repeat nil nil)

  ;; game loop
  (sdl:with-events ()
    (:quit-event () t)
    ;; Control keys
    (:key-down-event (:key key)
                     (cond ((or (sdl:key= key :sdl-key-down) (sdl:key= key :sdl-key-up)
                                (sdl:key= key :sdl-key-left) (sdl:key= key :sdl-key-right))
                            (update-player key))
                           ((sdl:key= key :sdl-key-escape)
                            (sdl:push-quit-event)
                            (reset-game))))
    ;; Where the action happens
    (:idle ()
           ;; debug stuff (works in single thread)
           #+my-game-debug
           (let ((connection
                  (or swank::*emacs-connection* (swank::default-connection))))
             (when (and connection (not (eql swank:*communication-style* :spawn)))
               (swank::handle-requests connection t)))

           ;; draw stuff here
           (draw-screen)
           ;; refresh
           (sdl:update-display))))
