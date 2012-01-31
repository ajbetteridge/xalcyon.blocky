;;; xalcyon.lisp --- micro-xalcyon, a mini implementation of a xalcyon-like

;; Copyright (C) 2010, 2011, 2012  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Preamble

(defpackage :xalcyon 
  (:use :blocky :common-lisp))

(in-package :xalcyon)

(setf *screen-width* 1280)
(setf *screen-height* 720)
(setf *window-title* "Xalcyon")
(setf *use-antialiased-text* nil)
(setf *scale-output-to-window* nil)
(setf *frame-rate* 30)

(defvar *xalcyon-font* "sans-mono-bold-16") ;; one of the included fonts

(defun is-enemy (thing) 
  (has-tag thing :enemy))

;;; Some sounds

(defparameter *slam-sounds*
  (defresource 
      (:name "slam1" :type :sample :file "slam1.wav" :properties (:volume 52))
      (:name "slam2" :type :sample :file "slam2.wav" :properties (:volume 52))
    (:name "slam3" :type :sample :file "slam3.wav" :properties (:volume 52))))

(defparameter *bonux-sounds*
  (defresource 
      (:name "bonux1" :type :sample :file "bonux1.wav" :properties (:volume 12))
      (:name "bonux2" :type :sample :file "bonux2.wav" :properties (:volume 12))
    (:name "bonux3" :type :sample :file "bonux3.wav" :properties (:volume 12))))
 
;;; Musical accompaniment
 
(defparameter *soundtrack* 
  (defresource 
    (:name "beatup" :type :music :file "beatup.ogg")
    (:name "defmacron" :type :music :file "defmacron.ogg")
    (:name "ompula" :type :music :file "ompula.ogg")
    (:name "wraparound" :type :music :file "wraparound.ogg" :properties (:volume 200))
    (:name "xalcyon" :type :music :file "xalcyon.ogg")))

;;; Colored, themeable bricks that make up the environment

(defparameter *levels* 
  '((:dec :background "DarkSlateBlue" :brick "SlateBlue" :brick2 "hot pink" :wall "Black")
    (:tandy :background "black" :brick "red" :brick2 "gray40" :wall "gray20")
    (:sun :background "saddle brown" :brick "cyan" :brick2 "cyan" :wall "black")))

(defparameter *level* :dec)

(defun level-color (&optional (part :brick))
  (let ((level (assoc *level* *levels*)))
    (getf (rest level) part)))

(define-block brick 
    :tags '(:brick)
    :part :brick
    :color (level-color))

(define-method damage brick (points) nil)

(define-method initialize brick (&optional color)
  (super%initialize self)
  (setf %color color))

(defmacro defbrick (name &body body)
  `(define-block (,name :super "XALCYON:BRICK")
     ,@body))

(defun is-brick (thing)
  (and (blockyp thing)
       (has-tag thing :brick)))

(define-method draw brick ()
  (draw-box %x %y %width %height :color (level-color %part)))

;;; Sparkle clouds

(define-block spark 
  :width 3 :height 3
  :collision-type nil)

(define-method initialize spark ()
  (later 1.5 (exit self)))

(define-method draw spark ()
  (dotimes (n 8)
    (draw-box (+ %x (random 40))  (+ %y (random 40)) (+ 1 (random 5)) (+ 1 (random 4))
	      :color (random-choose '("white" "cyan" "yellow" "orange" "red")))))

(define-method update spark ()
  (move-toward self (random-direction) (+ 4 (random 6))))

(defun make-sparks (x y &optional (n 5))
  (dotimes (z n)
    (drop-block *world* 
		(new spark) 
		(+ x (random 30)) (+ y (random 30)))))

;;; A bullet

(defun is-bullet (thing)
  (has-tag thing :bullet))

(defun is-player-bullet (thing)
  (and (is-bullet thing)
       (has-tag thing :player)))

(defun is-enemy-bullet (thing)
  (and (is-bullet thing)
       (has-tag thing :enemy)))

(define-block bullet 
  :height 5 :width 5
  :tags '(:bullet))

(define-method update bullet ()
  (move-forward self 4))

(define-method collide bullet (thing)
  (cond 
    ;; hit enemies with player bullets
    ((and (is-player-bullet self)
	  (is-enemy thing))
     (damage thing 1))
    ;; allow player bullets to pass through trail
    ;; (and through the player)
    ((and (is-player-bullet self)
	  (or (is-trail thing)
	      (is-robot thing)))
     nil)
    ((is-trail thing)
     (play-sound self "bonux3")
     (destroy self))
    ((and (is-enemy thing)
	  (not (is-player-bullet thing)))
     nil)
    (t (when (has-method :damage thing)
	 (damage thing 1)
	 (destroy self)))))

(define-method initialize bullet (heading &rest tags)
  (super%initialize self)
  (setf %heading heading)
  (when tags
    (dolist (tag tags)
      (add-tag self tag))))

(define-method draw bullet ()
  (draw-circle %x %y 2.5 
	       :color (random-choose 
		       (if (is-player-bullet self)
			   '("white" "cyan")
			   '("yellow" "red")))
	       :type :solid))

;;; Corruption glitches that spread

(defparameter *corruption-images*
  (defresource 
      (:name "corruption1" :type :image :file "corruption1.png")
      (:name "corruption2" :type :image :file "corruption2.png")
    (:name "corruption3" :type :image :file "corruption3.png")
    (:name "corruption4" :type :image :file "corruption4.png")
    (:name "corruption5" :type :image :file "corruption5.png")
    (:name "corruption6" :type :image :file "corruption6.png")))

(defparameter *corruption-sounds*
  (defresource 
      (:name "pip1" :type :sample :file "pip1.wav" :properties (:volume 40))
      (:name "pip2" :type :sample :file "pip2.wav" :properties (:volume 40))
    (:name "pip3" :type :sample :file "pip1.wav" :properties (:volume 40))))

(defparameter *glitch-sounds*
  (defresource 
      (:name "blurp" :type :sample :file "blurp.wav" :properties (:volume 40))
      (:name "blurp2" :type :sample :file "blurp2.wav" :properties (:volume 40))
      (:name "blop" :type :sample :file "blop.wav" :properties (:volume 40))))

(define-block glitch
  (tags :initform '(:enemy))
  (image :initform (random-choose *corruption-images*))
  (speed :initform (+ 0.1 (random 0.2)))
  (overlay-color :initform nil))

(define-method damage glitch (points)
  (make-sparks (- %x 20) (- %y 20) 2)
  (play-sound self (random-choose *corruption-sounds*))
  (destroy self))

(define-method collide glitch (thing)
  (when (is-robot thing)
    (damage thing 4)
    (destroy self)))

(define-method set-overlay glitch ()
  (setf %overlay-color (random-choose '("cyan" "magenta" "yellow" "orange"))))

(define-method clear-overlay glitch ()
  (setf %overlay-color nil))

;; (define-method initialize glitch ()
;;   (super%initialize self)
;;   (later 4.0 (spread self)))

(define-method creep glitch ()
  (when (< (distance-to-player self) 350)
    (point-at-thing self (player))
    (percent-of-time 8 (play-sound self (random-choose *glitch-sounds*)))
    (move-forward self 1)))

(define-method update glitch ()
  (percent-of-time 3 (change-image self (random-choose *corruption-images*)))
  (creep self)
  (percent-of-time 3 
    (set-overlay self)
    (later 20 (clear-overlay self))))

(define-method draw glitch ()
  (super%draw self)
  (set-blending-mode :additive2)
  (when %overlay-color
    (draw-box %x %y %width %height
     :alpha 0.2
     :color %overlay-color)))

;;; Monitor enemy

(defresource
    (:name "monitor" :type :image :file "monitor.png")
    (:name "monitor2" :type :image :file "monitor2.png"))

(define-block monitor 
  (hit-points :initform 1) fleeing
  (direction :initform (random-choose '(:up :down :right :left)))
  (tags :initform '(:monitor :enemy))
  (image :initform "monitor2"))

(define-method choose-new-direction monitor ()
  (setf %direction
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:up :down :right :left))
	    ;; otherwise turn left
	    (getf '(:up :left :left :down :down :right :right :up)
		  (or %direction :up)))))

(define-method flee monitor ()
  (setf %heading (+ pi (heading-to-player self)))
  (percent-of-time 5 (drop self (new glitch)))
  (move-forward self 3.2))

(define-method stop-fleeing monitor ()
  (setf %fleeing nil))

(define-method hunt monitor ()
  (let ((dist (distance-to-player self)))
    ;; hunt for player
    (if (< dist 160)
	(progn 
	  (setf %heading (heading-to-player self))
	  (move-forward self 2)
	  ;; if close enough, fire and run away 
	  (when (< dist 90)
	    (fire self (heading-to-player self))
	    (setf %fleeing t)
	    (later 1.4 (stop-fleeing self))
	    (play-sound self (defresource :name "magenta-alert"
					  :type :sample :file "magenta-alert.wav" 
					  :properties (:volume 60)))))
	;; patrol
	(progn (percent-of-time 2 (choose-new-direction self))
	       (move-toward self %direction 2)))))

(define-method update monitor ()
  (if %fleeing 
      (flee self)
      (hunt self)))

(define-method collide monitor (thing)
  (when (not (is-enemy thing))
    (when (is-robot thing)
      (damage thing 1))
    (restore-location self)
    ;; (when %fleeing (setf %fleeing nil))
    (choose-new-direction self)))

(defparameter *slam-sounds*
  (defresource 
      (:name "slam1" :type :sample :file "slam1.wav" :properties (:volume 52))
      (:name "slam2" :type :sample :file "slam2.wav" :properties (:volume 52))
    (:name "slam3" :type :sample :file "slam3.wav" :properties (:volume 52))))

(define-method damage monitor (points)
  (make-sparks (- %x 16) (- %y 16))
  (play-sound self (defresource :name "xplod"
			    :type :sample :file "xplod.wav" 
			    :properties (:volume 60)))
  (play-sound self (random-choose *slam-sounds*))
  (destroy self))

(define-method fire monitor (direction)
  (multiple-value-bind (x y) (center-point self)
    (drop-block *world* (new bullet (heading-to-player self)) x y)))

(defparameter *bounce-sounds*
  (defresource 
      (:name "boop1" :type :sample :file "boop1.wav" :properties (:volume 20))
      (:name "boop2" :type :sample :file "boop2.wav" :properties (:volume 20))
      (:name "boop3" :type :sample :file "boop3.wav" :properties (:volume 20))))

;;; Positronic trail

(defun is-trail (thing)
  (has-tag thing :trail))

(define-block trail
  (tags :initform '(:trail))
  (height :initform 4)
  (width :initform 4))

(define-method draw trail ()
  (draw-box %x %y %width %height :color (random-choose '("yellow" "yellow" "goldenrod"))))

(define-method initialize trail ()
  (later 2.5 (destroy self)))

(define-method collide trail (thing)
  (when (is-enemy-bullet thing)
    (destroy thing)))

;;; The player

(defresource
  (:name "robot" :type :image :file "robot.png"))
 
(defun is-robot (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

(defparameter *button-bindings* 
  '((:joystick (:right-trigger :button-down) "fire .")
    (:joystick (:right-trigger :button-up) "stop-firing .")
    (:joystick (:left-bumper :button-down) "boost .")
    (:joystick (:left-bumper :button-up) "stop-boosting .")))
    ;; (:joystick (:x :button-down) "act .")
    ;; (:joystick (:y :button-down) "cancel .")))

(define-block robot 
  (dead :initform nil)
  (ready :initform t)
  ;; we want to catch the beginning of firing, even if the input
  ;; polling in `update' misses it. (see below)
  (default-events :initform '(((:space) (fire))))
  (image :initform "robot")
  (height :initform 16)
  (width :initform 16)
  (energy :initform 4)
  (tags :initform '(:robot))
  (speed :initform 2))

;; (define-method draw robot ()
;;   (super%draw self)
;;   (when (not %dead)
;;     (multiple-value-bind (x0 y0) (center-point self)
;;       (multiple-value-bind (x y) (step-in-direction x0 y0 %direction 4)
;; 	(draw-circle x y 3 :color "red" :type :solid)))))

(defresource 
    (:name "zap" :type :sample :file "zap.wav" :properties (:volume 30))
    (:name "paz" :type :sample :file "paz.wav" :properties (:volume 30))
    (:name "talk" :type :sample :file "talk.wav" :properties (:volume 20)))

(define-method increase-energy robot (n)
  (incf %energy n))

(define-method reload robot ()
  (setf %ready t))

(define-method fire robot (heading)
  (when (and %ready (not %dead))
    (setf %ready nil)
    (later 12 (reload self))
    (play-sound self "zap")
    (drop self (new bullet heading :player)
	  (/ %width 2) (/ %height 2))))

(define-method damage robot (points)
  (when (not %dead)
    (play-sound self (defresource :name "deathx" :type :sample :file "deathx.wav" :properties (:volume 100)))
    (setf %dead t)
    (change-image self (defresource :name "skull" :type :image :file "skull.png"))))

(define-method collide robot (thing)
  (when (is-brick thing)
    (restore-location self)))

(define-method aim robot (angle)
  (setf %heading angle))

(define-method drop-trail-maybe robot ()
  (decf %energy)
  (when (zerop %energy)
    (drop self (new trail) 6 6)
    (setf %energy 2)))

(define-method update robot ()
  (when (not %dead)
    (when (left-analog-stick-pressed-p)
      (aim self (left-analog-stick-heading))
      (move-forward self 3)
      (drop-trail-maybe self))
    (when (right-analog-stick-pressed-p)
      (fire self (right-analog-stick-heading)))))

;;; The reactor

(define-world reactor
  (background-color :initform (level-color :background))
  (grid-size :initform 16)
  (grid-width :initform 64)
  (grid-height :initform 64))

(define-block reactor-turtle)

(define-method draw-wall reactor-turtle (segments &optional (size 32))
  (dotimes (n segments)
    (let ((brick (new brick)))
      (drop self brick)
      (resize brick size size)
      (move-forward self (+ size 0.02)))))

(define-method draw-square reactor-turtle (size)
  (dotimes (n 4)
    (draw-wall self size)
    (turn-right self 90)))

(define-method skip-wall reactor-turtle (segments)
  (move-forward self (+ 0.02 (* 32 segments))))

(define-method draw-room reactor-turtle (size)
  (dotimes (n 4)
    (draw-wall self (- size 2))
    (skip-wall self 2)
    (draw-wall self 2)
    (turn-right self 90)))
    
(define-method run reactor-turtle ()
  (draw-square self 31))
  ;; (skip-wall self 10)
  ;; (turn-right self 90)
  ;; (skip-wall self 10)
  ;; (turn-left self 90)
  ;; (draw-room self 10)
  ;; (turn-left self 90)
  ;; (skip-wall self 3)
  ;; (draw-room self 6))

(define-method build reactor ()
  (setf %window-scrolling-speed 4)
  (let ((*quadtree* %quadtree))
    (with-fields (grid-width grid-height) self
      (move-window-to self 0 0)
      (let ((turtle (new reactor-turtle)))
      	(drop self turtle)
      	(run turtle)
      	(discard-block self turtle)))
    ;; (dotimes (n 5)
    ;;   (add-block self (new glitch) 
    ;; 		 (+ 100 (random 800))
    ;; 		 (+ 100 (random 800))))
    (dotimes (n 50)
      (add-block self (new monitor) 
    		 (+ 100 (random 800))
    		 (+ 100 (random 800))))))
    ;; (dotimes (n 3)
    ;;   (add-block self (new biclops) 
    ;; 		 (+ 400 (random 500))
    ;; 		 (+ 400 (random 500))))))

(defun xalcyon ()
  (let ((robot (new robot))
	(reactor (new reactor)))
    (set-location robot 110 110)
    (bind-event reactor '(:escape) :reset)
    (new universe 
	 :player robot
	 :world reactor)
    (build reactor)
    (play-music "wraparound" :loop t)))

(define-method reset reactor ()
  (xalcyon))

;;; xalcyon.lisp ends here
