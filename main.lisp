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
;; Stix - inspired by the old arcade classic.
;;


(require 'asdf)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)
(asdf:operate 'asdf:load-op :lispbuilder-sdl-gfx)
(asdf:operate 'asdf:load-op :lispbuilder-sdl-mixer)
;;(asdf:operate 'asdf:load-op :lispbuilder-sdl-ttf)


;; debug stuff : causes windows to freak out (but works on Linux)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (find-package :swank) (not (find-package :windows)))
    (pushnew :debug-game *features*)))


(defpackage :i27.games.stix
  (:use :common-lisp)
  (:export :main))

(in-package i27.games.stix)

;; Parameters
(defparameter *title-music*         nil)
(defparameter *game-music*          nil)
(defparameter *title-music-path*    "audio/8_bit.xm")
(defparameter *game-music-path*     "audio/qix_ed.mod")
#+debug-game
(progn
  (defparameter *title-music-path*  "/home/jgrant/cl-stix/audio/8_bit.xm")
  (defparameter *game-music-path*   "/home/jgrant/cl-stix/audio/qix_ed.mod"))

;; (defparameter *font-path*          "quacksal.tff")
;; #+debug-game
;; (defparameter *font-path*          "/home/jgrant/cl-stix/quacksal.ttf")
;; (defparameter *font*               nil)

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

(defun init-audio()
  (sdl-mixer:open-audio)
  (setf (sdl-mixer:music-volume) 64)
  (setf *game-music* (sdl-mixer:load-music *game-music-path*))
  (setf *title-music* (sdl-mixer:load-music *title-music-path*)))

(defun deinit-audio()
  (sdl-mixer:free *title-music*)
  (sdl-mixer:free *game-music*)
  (sdl-mixer:close-audio t))

(defun start-music()
  (sdl-mixer:play-music *game-music* :loop t))

(defun stop-music()
  (sdl-mixer:halt-music)
  (sdl-mixer:rewind-music))

(defun toggle-music ()
  (if (sdl-mixer:music-playing-p)
      (stop-music)
      (start-music)))

(let ((currently-fullscreen? nil))
  (defun toggle-fullscreen ()
    (if currently-fullscreen?
        (sdl:resize-window *display-width* *display-height* :hw t :resizable nil)
        (sdl:resize-window *display-width* *display-height* :hw t :fullscreen t))
    (setf currently-fullscreen? (if currently-fullscreen? nil t))))

(defun reset-game ()
  "Set scores to 0 etc."

  (setf *p1* (make-player))

  ;; init the game surface
  (setf *game-surface* (sdl:create-surface *display-width* *display-height*
                                           :x 0 :y 0))
  ;; draw the border
  (sdl:draw-rectangle-* *min-x* *min-y*
                        (+ 1 *game-width*) (+ 1 *game-height*)
                        :color *edge-color* :surface *game-surface*)

  ;; draw game title
  (sdl:initialise-default-font sdl:*font-10x20*)
  ;;(setf *font* (sdl-ttf:open-font *font-path* 20))
  (sdl:draw-string-solid-*
   "STIX" 12 10 :color *edge-color* :surface *game-surface*)

  (stop-music)

  ;; clear game characters
  (if sdl:*default-display*
      (sdl:clear-display sdl:*black*)))


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
  ;; #+debug-game
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


;; (defun main()

;;   (cffi:define-foreign-library sdl
;;     (:darwin (:or (:framework "SDL")
;;                   (:default "libSDL")))
;;     (:windows "SDL.dll")
;;     (:unix (:or "libSDL-1.2.so.0"
;;                 "libSDL-1.2.so"
;;                 "libSDL.so"
;;                 "libSDL")))
;;

;;   (cffi:use-foreign-library sdl)

;; Create the window
(sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
  (init-audio)

  (sdl:show-cursor nil)

  (sdl:window *display-width* *display-height*
              ;;:fullscreen t
              ;; :double-buffer t
              :title-caption "STIX"
              :icon-caption "STIX"
              :resizable nil)
  (setf (sdl:frame-rate) 30)
  (sdl:enable-key-repeat nil nil)

  (reset-game)

  ;; game loop
  (sdl:with-events ()
    (:quit-event ()
                 (deinit-audio)
                 t)

    ;; control keys
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
    ;; where the action happens
    (:idle ()
           ;; debug stuff (works in single thread)
           #+debug-game
           (let ((connection
                  (or swank::*emacs-connection* (swank::default-connection))))
             (when (and connection (not (eql swank:*communication-style* :spawn)))
               (swank::handle-requests connection t)))

           ;; draw stuff here
           (draw-screen)
           ;; refresh
           (sdl:update-display))))

;;  )


(defun dump-image()
  (load (compile-file "main"))
  (let ((filename #+windows "main.exe" #-windows "main"))
    #+clisp (saveinitmem filename :init-function #'i27.games.stix:main :executable t :norc t)
    #+sbcl (sb-ext:save-lisp-and-die filename :toplevel #'i27.games.stix:main :executable t)
    #+clozure (save-application
               filename :toplevel-function #'i27.games.stix:main :prepend-kernel t)))
