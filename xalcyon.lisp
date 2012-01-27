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

;; We should define a package to hold all the names in our project.
;; Common Lisp "packages" are akin to namespaces in other languages.

(defpackage :xalcyon 
  (:use :blocky :common-lisp))

;; The ":use" statement above means that we will be using names from
;; Blocky as well as those from Common Lisp itself.

;; Now we start using our new package.

(in-package :xalcyon)

;; Let's set a few global variables to configure the game session.
;; Global variables have asterisks surrounding the name *like-this*

(setf *screen-width* 800)
(setf *screen-height* 600)
(setf *window-title* "MicroXALCYON")
(setf *use-antialiased-text* nil)
(setf *use-nominal-screen-size* t) ;; scale the image to the window-size
(setf *frame-rate* 30)

;; Now we define some variables of our own with `defvar'.

(defvar *xalcyon-font* "sans-mono-bold-16") ;; one of the included fonts

(defvar *score* 0)

;; Let's define a simple object to display a flashing numeric score.

(define-block score-display points) 

(define-method initialize score-display (&optional (points 100))
  ;; %foo means (field-value :foo self), i.e. this.foo in other languages
  (setf %points points)
  ;; send the message :DESTROY to SELF after 1.3 seconds
  (later 1.3 (destroy self)))

(define-method draw score-display ()
  (draw-string (prin1-to-string %points)
	       %x %y :color (random-choose '("magenta" "cyan" "yellow" "red" "white"))
		     :font *xalcyon-font*))

(defun score (&optional (points 50) x y)
  (incf *score* points)
  (when (and (numberp x)
	     (numberp y))
    (add-block *world* (new score-display points) x y)))

(defun reset-score ()
  (setf *score* 0))
  
(defparameter *soundtrack* 
  (defresource 
    (:name "beatup" :type :music :file "beatup.ogg")
    (:name "defmacron" :type :music :file "defmacron.ogg")
    (:name "ompula" :type :music :file "ompula.ogg")
    (:name "wraparound" :type :music :file "wraparound.ogg")
    (:name "xalcyon" :type :music :file "xalcyon.ogg")))

;;; Colored, themeable bricks that make up the environment

(defparameter *levels* 
  '((:dec :background "DarkSlateBlue" :brick "SlateBlue" :brick2 "hot pink" :wall "Black")
    (:tandy :background "black" :brick "red" :brick2 "gray40" :wall "gray20")
    (:sun :background "saddle brown" :brick "cyan" :brick2 "cyan" :wall "black")))

(defparameter *level* :tandy)

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

;;; Breakable blocks

(defbrick glass
  :part :brick2
  :color (level-color :brick2))

(define-method damage glass (points)
  (play-sound self (random-choose '("shatter" "shatter2")))
  (destroy self))

(defresource 
    (:name "shatter" :type :sample :file "shatter.wav" :properties (:volume 40))
    (:name "shatter2" :type :sample :file "shatter2.wav" :properties (:volume 40)))

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

;;; Biclops man enemy

(defresource
    (:name "biclops" :type :image :file "biclops.png"))

(define-block biclops
  (image :initform "biclops")
  (tags :initform '(:biclops :enemy))
  (stomped :initform nil)
  (hit-points :initform 3))

(defparameter *biclops-sounds*
  (defresource 
      (:name "gond1" :type :sample :file "gond1.wav" :properties (:volume 40))
      (:name "gond2" :type :sample :file "gond2.wav" :properties (:volume 70))))

(define-method stomp biclops ()
  (play-sound self "gond1")
  (move-toward self (direction-to-player self) 12)
  (setf %stomped t)
  (later 12 (unstomp self)))

(define-method collide biclops (thing)
  (when (is-robot thing)
    (damage thing 1))
  (when (is-brick thing)
    (restore-location self)))

(define-method damage biclops (points)
  (play-sound self "gond2")
  (make-sparks %x %y 10)
  (score 5200 %x %y)
  (destroy self))

(define-method unstomp biclops ()
  (setf %stomped nil))

(define-method update biclops ()
  (when (< (distance-to-player self) 220)
    (when (not %stomped)
      (stomp self))))

;;; An enemy bullet 

(defun is-bullet (thing)
    (has-tag thing :bullet))

(define-block bullet 
  :height 5 :width 5
  :tags '(:bullet))

(define-method update bullet ()
  (move-forward self 2))

(define-method collide bullet (thing)
  (when (not (has-tag thing :enemy))
    (if (is-trail thing)
	(progn 
	  (play-sound self "bonux3")
	  (destroy self))
	(unless (is-bullet thing)
	  (when (has-method :damage thing)
	    (damage thing 1)
	    (destroy self))))))

(define-method initialize bullet (heading)
  (super%initialize self)
  (setf %heading heading))

(define-method draw bullet ()
  (draw-circle %x %y 2.5 :color (random-choose '("yellow" "red"))
	       :type :solid))

;;; Monitor enemy

(defresource
    (:name "monitor" :type :image :file "monitor.png")
    (:name "monitor2" :type :image :file "monitor2.png"))

(define-block monitor 
  (hit-points :initform 1)
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

(define-method update monitor ()
  (if (< (distance-to-player self) 160)
      (progn (setf %direction (direction-to-player self))
	     (percent-of-time 2
	       (fire self (+ (heading-to-player self) 10))
			     ;; a little randomness to sometimes lead player

;	       (later 2 (move-toward self %direction 0.2))
	       (play-sound self (defresource :name "magenta-alert"
					 :type :sample :file "magenta-alert.wav" 
					 :properties (:volume 40)))))
      (move-toward self %direction 0.5)))

(define-method collide monitor (thing)
  (when (is-robot thing)
    (damage thing 1))
  (restore-location self)
  (choose-new-direction self))

(define-method damage monitor (points)
  (make-sparks (- %x 16) (- %y 16))
  (play-sound self (defresource :name "xplod"
			    :type :sample :file "xplod.wav" 
			    :properties (:volume 60)))
  (play-sound self (random-choose *slam-sounds*))
  (score 700 %x %y)
  (destroy self))

(define-method fire monitor (direction)
  (multiple-value-bind (x y) (center-point self)
    (drop-block *world* (new bullet (heading-to-player self)) x y)))

;;; A bouncing ball to break bricks with

(defun is-ball (thing)
  (has-tag thing :ball))

(defparameter *bounce-sounds*
  (defresource 
      (:name "boop1" :type :sample :file "boop1.wav" :properties (:volume 20))
      (:name "boop2" :type :sample :file "boop2.wav" :properties (:volume 20))
      (:name "boop3" :type :sample :file "boop3.wav" :properties (:volume 20))))

(define-block ball 
  :height 5 :width 5
  ;; having them expire after 100 bounces seems to make a good balance
  ;; between usability and preventing runaway or "stuck" pong balls.
  :bounces 100
  :tags '(:ball)
  :direction :right)

(define-method update ball ()
  (move-toward self %direction 4))

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

(define-method collide ball (thing)
  ;; take a thing-specific action
  (unless (or (is-trail thing) 
	      (is-robot thing))
    (when (has-method :damage thing)
      (damage thing 1)
      (destroy self))))

(define-method initialize ball (direction)
  (assert (keywordp direction))
  (initialize%%block self)
  (setf %direction direction))

(define-method draw ball ()
  (draw-circle %x %y 2.6 :color (random-choose '("white" "cyan"))
	       :type :solid))

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
  (when (is-bullet thing)
    (destroy thing)))

;;; Corruption that spreads

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

(define-block glitch
  (image :initform (random-choose *corruption-images*))
  (speed :initform (+ 0.1 (random 0.2)))
  (overlay-color :initform nil))

(define-method damage glitch (points)
  (make-sparks (- %x 20) (- %y 20) 2)
  (play-sound self (random-choose *corruption-sounds*))
  (score 110 %x %y)
  (destroy self))

(define-method collide glitch (thing)
  (when (is-robot thing)
    (damage thing 4)
    (destroy self)))

(define-method set-overlay glitch ()
  (setf %overlay-color (random-choose '("cyan" "magenta" "yellow" "orange"))))

(define-method clear-overlay glitch ()
  (setf %overlay-color nil))

(define-method initialize glitch ()
  (super%initialize self)
  (later 4.0 (spread self)))

(define-method spread glitch ()
  (when (< (distance-to-player self) 350)
    (multiple-value-bind (x y)
	(step-in-direction %x %y (random-direction) 16)
      (add-block *world* (new glitch) x y)))
  (if (zerop (random 2))
      (later 3.0 (spread self))
      (later 5.0 (spread self))))

(define-method update glitch ()
  (percent-of-time 3 (change-image self (random-choose *corruption-images*)))
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

;;; The player

(defresource
  (:name "robot" :type :image :file "robot.png"))
 
(defun is-robot (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

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

(define-method draw robot ()
  (super%draw self)
  (when (not %dead)
    (multiple-value-bind (x0 y0) (center-point self)
      (multiple-value-bind (x y) (step-in-direction x0 y0 %direction 4)
	(draw-circle x y 3 :color "red" :type :solid)))))

(define-method move robot (direction)
  (unless (holding-shift)
    (decf %energy)
    (when (zerop %energy)
      (drop self (new trail) 6 6)
      (setf %energy 4))
    (move-toward self direction %speed)))

(defresource 
    (:name "zap" :type :sample :file "zap.wav" :properties (:volume 30))
    (:name "paz" :type :sample :file "paz.wav" :properties (:volume 30))
    (:name "talk" :type :sample :file "talk.wav" :properties (:volume 20)))

(define-method increase-energy robot (n)
  (incf %energy n))

(define-method reload robot ()
  (setf %ready t))

(define-method fire robot (&optional direction)
  (when (and %ready (not %dead))
    (setf %ready nil)
    (later 12 (reload self))
    (play-sound self "zap")
    (multiple-value-bind (x0 y0) (center-point self)
      (multiple-value-bind (x y) (step-in-direction x0 y0 (or direction %direction) 12)
	(add-block *world* (new ball (or direction %direction)) x y)))))

(define-method damage robot (points)
  (when (not %dead)
    (play-sound self (defresource :name "deathx" :type :sample :file "deathx.wav" :properties (:volume 100)))
    (setf %dead t)
    (change-image self (defresource :name "skull" :type :image :file "skull.png"))))

(define-method collide robot (thing)
  ;; (setf %collided-with thing)
  (when (is-brick thing)
    (restore-location self)))

(defun holding-down-arrow ()
  (or (keyboard-down-p :kp2)
      (keyboard-down-p :down)))

(defun holding-up-arrow ()
  (or (keyboard-down-p :kp8)
      (keyboard-down-p :up)))

(defun holding-left-arrow ()
  (or (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defun holding-space ()
  (keyboard-down-p :space))     

(define-method aim robot (dir)
  (setf %direction dir))

(define-method update robot ()
  (when (not %dead)
    (let ((direction
	    (cond 
	      ((holding-down-arrow) :down)
	      ((holding-up-arrow) :up)
	      ((holding-left-arrow) :left)
	      ((holding-right-arrow) :right))))
      (when direction
	(move self direction))
      (if (holding-space)
	  (fire self %direction)
	  (when direction 
	    (aim self direction))))))

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
  (draw-square self 31)
  (skip-wall self 10)
  (turn-right self 90)
  (skip-wall self 10)
  (turn-left self 90)
  (draw-room self 10)
  (turn-left self 90)
  (skip-wall self 3)
  (draw-room self 6))

(define-method build reactor ()
  (let ((*quadtree* %quadtree))
    (with-fields (grid-width grid-height) self
      (move-window-to self 0 0)
      (let ((turtle (new reactor-turtle)))
      	(drop self turtle)
      	(run turtle)
      	(discard-block self turtle)))
    (dotimes (n 5)
      (add-block self (new glitch) 
    		 (+ 100 (random 800))
    		 (+ 100 (random 800))))
    (dotimes (n 20)
      (add-block self (new monitor) 
		 (+ 100 (random 800))
		 (+ 100 (random 800))))
    (dotimes (n 3)
      (add-block self (new biclops) 
		 (+ 400 (random 500))
		 (+ 400 (random 500))))))

(define-method reset reactor ()
  (xalcyon))

(define-method draw reactor ()
  (draw%%world self)
  (multiple-value-bind (top left right bottom)
      (window-bounding-box self)
    (let ((x (+ left (dash 5)))
	  (y (- bottom (font-height *xalcyon-font*) (dash 2)))
	  (label (format nil "~d" *score*)))
      (draw-string label 
		   x y :color "white"
		   :font *xalcyon-font*))))

;; Last, we define the startup function (whose name should be the same
;; as the package.)

(defun xalcyon ()
  (let ((robot (new robot))
	(reactor (new reactor)))
    (set-location robot 110 110)
    (bind-event reactor '(:escape) :reset)
    (new universe 
	 :player robot
	 :world reactor)
    (reset-score) 
    (build reactor)
    (play-music (random-choose *soundtrack*) :loop t)))

;;; xalcyon.lisp ends here
