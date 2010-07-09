
(require 'asdf)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)


(defpackage :i27.games.stix (:use :common-lisp ))

(in-package i27.games.stix)

;; Parameters
(defparameter *display-height* 480)
(defparameter *display-width* 640)

(defvar *objects* nil)


;; Create the window
(sdl:WITH-INIT (sdl:SDL-INIT-VIDEO)
  (sdl:WINDOW *display-width* *display-height*
              :title-caption "- S T I X -"
              :icon-caption "STIX")
  (setf (sdl:FRAME-RATE) 30)
  (sdl:enable-key-repeat nil nil)

  ;; game loop
  (sdl:WITH-EVENTS ()
    (:QUIT-EVENT () T)
    ;; Control keys
    (:KEY-DOWN-EVENT (:KEY key)
                     (WHEN (sdl:KEY= key :SDL-KEY-ESCAPE) (sdl:PUSH-QUIT-EVENT)))
    ;; Where the action happens
    (:IDLE ()
           ;; draw stuff here
           (sdl:UPDATE-DISPLAY))))
