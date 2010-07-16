;;
;; Stix - inspired by the old arcade classic.
;;


(require 'asdf)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)
(asdf:operate 'asdf:load-op :lispbuilder-sdl-gfx)
(asdf:operate 'asdf:load-op :lispbuilder-sdl-mixer)


;; debug stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :swank)
    (pushnew :my-game-debug *features*)))


(defpackage :i27.games.stix (:use :common-lisp ))

(in-package i27.games.stix)

;; Parameters
(defparameter *music-path*          "qix_music.ogg")
#+my-game-debug
(defparameter *music-path*          "/home/jgrant/cl-stix/qix_music.ogg")
(defparameter *music*               nil)

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

;; game structs
(defstruct player
  (y *max-y*)
  (x *min-x*)
  (yp *max-y*)
  (xp *min-x*)
  (size 3)
  (dir)
  (speed 1))

;; vars
(defparameter *p1* (make-player))

;; funs
(let ((currently-fullscreen? nil))
  (defun toggle-fullscreen ()
    (if currently-fullscreen?
        (sdl:resize-window *display-width* *display-height* :hw t :resizable nil)
        (sdl:resize-window *display-width* *display-height* :hw t :fullscreen t))
    (setf currently-fullscreen? (if currently-fullscreen? nil t))))

(let ((currently-playing-music? nil))
  (defun toggle-music ()
    (if currently-playing-music?
        (sdl-mixer:halt-music)
        (sdl-mixer:play-music *music* :loop t))
    (setf currently-playing-music? (if currently-playing-music? nil t))))

(defun reset-game ()
  "Set scores to 0 etc."
  (sdl:clear-display sdl:*black*)
  (setf *p1* (make-player)))

(defun limit-val (min max val)
  (cond ((> val max) max)
        ((< val min) min)
        (t val)))

(defun update-player (key)
  (let ((py (player-y *p1*))
        (px (player-x *p1*))
        (ps (player-speed *p1*)))
    (setf (player-yp *p1*) (player-y *p1*))
    (setf (player-xp *p1*) (player-x *p1*))
    (cond ((sdl:key= key :sdl-key-down)
           (setf (player-y *p1*) (limit-val *min-y* *max-y* (+ py ps)))
           (setf (player-dir *p1*) :down))
          ((sdl:key= key :sdl-key-up)
           (setf (player-y *p1*) (limit-val *min-y* *max-y* (- py ps)))
           (setf (player-dir *p1*) :up))
          ((sdl:key= key :sdl-key-left)
           (setf (player-x *p1*) (limit-val *min-x* *max-x* (- px ps)))
           (setf (player-dir *p1*) :left))
          ((sdl:key= key :sdl-key-right)
           (setf (player-x *p1*) (limit-val *min-x* *max-x* (+  px ps)))
           (setf (player-dir *p1*) :right))))
  ;; #+my-game-debug
  ;; (format t "~%player y:~A x:~A"
  ;;         (player-y *p1*) (player-x *p1*))
  )

(defun draw-diamond(x y size color1 color2)
  (sdl:draw-pixel (sdl:point :x x :y y) :color color2)
  (sdl:draw-shape (list (sdl:point :x (- x size) :y y)
                        (sdl:point :x x :y (- y size))
                        (sdl:point :x (+ x size) :y y)
                        (sdl:point :x x :y (+ y size))
                        (sdl:point :x (- x size) :y y))
                  :color color1 :style :solid))

(defun draw-screen ()
  (let* ((py (player-y *p1*))   (px (player-x *p1*))
         ;;(pyp (player-yp *p1*)) (pxp (player-xp *p1*))
         (size (player-size *p1*))
         (dir (player-dir *p1*))
         (trail-y py)
         (trail-x px))

    ;; calculate where to draw trail
    (cond ((eq dir :up)    (setf trail-y (+ py size 1)))
          ((eq dir :down)  (setf trail-y (- py size 1)))
          ((eq dir :left)  (setf trail-x (+ px size 1)))
          ((eq dir :right) (setf trail-x (- px size 1))))
    (sdl:draw-pixel-* px py :surface *game-surface* :color *edge-color*)

    ;; redraw main display
    (sdl:clear-display sdl:*black*)

    ;; show the game surface in the main window
    (sdl:blit-surface *game-surface* sdl:*default-display*)

    ;; draw the player
    (draw-diamond px py size *diamond-color* *diamond-p-color*)))


;; Create the window
(sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
  (sdl:show-cursor nil)

  ;; Init the game surface
  (setf *game-surface* (sdl:create-surface *display-width* *display-height*
                                           :x 0 :y 0))
  ;; draw the border
  (sdl:draw-rectangle-* *min-x* *min-y*
                        (+ 1 *game-width*) (+ 1 *game-height*)
                        :color *edge-color* :surface *game-surface*)

  (sdl:window *display-width* *display-height*
              ;;:fullscreen t
              ;; :double-buffer t
              :title-caption "- S T I X -"
              :icon-caption "STIX"
              :resizable nil)
  (setf (sdl:frame-rate) 30)
  (sdl:enable-key-repeat nil nil)

  ;; fire up the music !
  (sdl-mixer:open-audio)
  (setf (sdl-mixer:music-volume) 64)
  (setf *music* (sdl-mixer:load-music *music-path*))
  ;;(sdl-mixer:play-music *music* :loop t)

  ;; game loop
  (sdl:with-events ()
    (:quit-event ()
                 (sdl-mixer:halt-music)
                 (sdl-mixer:free *music*)
                 (sdl-mixer:close-audio)
                 t)
    ;; Control keys
    (:key-down-event (:key key)
                     (cond ((or (sdl:key= key :sdl-key-down)
                                (sdl:key= key :sdl-key-up)
                                (sdl:key= key :sdl-key-left)
                                (sdl:key= key :sdl-key-right))
                            (update-player key))
                           ((sdl:key= key :sdl-key-f)
                            (toggle-fullscreen))
                           ((sdl:key= key :sdl-key-m)
                            (toggle-music))
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
