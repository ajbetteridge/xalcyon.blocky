;;; xalcyon.lisp --- a multidirectional sci-fi shooter

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

(setf *screen-width* 800)
(setf *screen-height* 600)
(setf *window-title* "Xalcyon")
(setf *use-antialiased-text* nil)
(setf *scale-output-to-window* nil)
(setf *frame-rate* 30)

(defvar *xalcyon-font* "sans-mono-bold-16") 

(defun is-enemy (thing) 
  (has-tag thing :enemy))

(defun is-target (thing)
  (has-tag thing :target))

;;; Some sounds


(defparameter *vox-sounds*
  (defresource 
      (:name "vox-energy" :type :sample :file "vox-energy.wav" :properties (:volume 180))
      (:name "vox-hazard" :type :sample :file "vox-hazard.wav" :properties (:volume 180))
      (:name "vox-message" :type :sample :file "vox-message.wav" :properties (:volume 180))
      (:name "vox-radiation" :type :sample :file "vox-radiation.wav" :properties (:volume 180))
      (:name "vox-repair" :type :sample :file "vox-repair.wav" :properties (:volume 180))
    (:name "vox-restore" :type :sample :file "vox-restore.wav" :properties (:volume 180))
      (:name "vox-shield-warning" :type :sample :file "vox-shield-warning.wav" :properties (:volume 180))))

(defresource 
    (:name "xplod"
     :type :sample :file "xplod.wav" 
     :properties (:volume 90)))

(defparameter *bounce-sounds*
  (defresource 
      (:name "boop1" :type :sample :file "boop1.wav" :properties (:volume 20))
      (:name "boop2" :type :sample :file "boop2.wav" :properties (:volume 20))
    (:name "boop3" :type :sample :file "boop3.wav" :properties (:volume 20))))

(defparameter *doorbell-sounds*
  (defresource 
      (:name "doorbell" :type :sample :file "doorbell.wav" :properties (:volume 20))
      (:name "doorbell2" :type :sample :file "doorbell2.wav" :properties (:volume 20))
    (:name "doorbell3" :type :sample :file "doorbell3.wav" :properties (:volume 20))))

(defparameter *slam-sounds*
  (defresource 
      (:name "slam1" :type :sample :file "slam1.wav" :properties (:volume 52))
      (:name "slam2" :type :sample :file "slam2.wav" :properties (:volume 52))
    (:name "slam3" :type :sample :file "slam3.wav" :properties (:volume 52))))

(defparameter *whack-sounds*
  (defresource 
      (:name "whack1" :type :sample :file "whack1.wav" :properties (:volume 82))
      (:name "whack2" :type :sample :file "whack2.wav" :properties (:volume 82))
    (:name "whack3" :type :sample :file "whack3.wav" :properties (:volume 82))))

(defparameter *bonux-sounds*
  (defresource 
      (:name "bonux1" :type :sample :file "bonux1.wav" :properties (:volume 12))
      (:name "bonux2" :type :sample :file "bonux2.wav" :properties (:volume 12))
    (:name "bonux3" :type :sample :file "bonux3.wav" :properties (:volume 12))))
 
;;; Musical accompaniment
 
(defparameter *soundtrack* 
  (defresource 
    (:name "beatup" :type :music :file "beatup.ogg")
    (:name "vixon" :type :music :file "vixon.ogg")
    (:name "xmrio" :type :music :file "xmrio.ogg")
    (:name "phong" :type :music :file "phong.ogg")
    (:name "defmacron" :type :music :file "defmacron.ogg")
    (:name "ompula" :type :music :file "ompula.ogg")
    (:name "wraparound" :type :music :file "wraparound.ogg" :properties (:volume 200))
    (:name "xalcyon" :type :music :file "xalcyon.ogg")))

;;; Colored, themeable bricks that make up the environment

(defparameter *themes* 
  '((:dec :background "DarkSlateBlue" :brick "SlateBlue" :brick2 "hot pink" :wall "Black")
    (:tandy :background "black" :brick "red" :brick2 "gray40" :wall "gray20")
    (:vax :background "gray20" :brick "gray80" :brick2 "gray40" :wall "gray20")
    (:command :background "MediumBlue" :brick "yellow" :brick2 "gray40" :wall "gray20")
    (:maynard :background "gray10" :brick "goldenrod" :brick2 "gray40" :wall "gray20")
    (:zerk :background "black" :brick "DeepSkyBlue" :brick2 "cyan" :wall "black")))

(defun random-theme () (random-choose (mapcar #'car *themes*)))

(defparameter *theme* :vax)

(defun theme-color (&optional (part :brick))
  (let ((theme (assoc *theme* *themes*)))
    (getf (rest theme) part)))

(define-block brick 
    :tags '(:brick)
    :part :brick
    :color (theme-color))

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
  (draw-box %x %y %width %height :color (theme-color %part)))

;;; Collectible "chips" which are the XP/currency/score all rolled into one

(defresource 
    (:name "chip" :type :image :file "chip.png")
    (:name "chip1" :type :sample :file "chip.wav" :properties (:volume 100)))

(define-block chip 
  :image "chip" 
  :speed (+ 1.5 (random 0.5))
  :heading (random (* 2 pi))
  :tags '(:chip)
  :value (+ 1 (random 5)))

(defun is-chip (thing)
  (has-tag thing :chip))

(define-method update chip () 
  (when (< (distance-to-player self) 100)
    (point-at-thing self (player)))
  (move-forward self %speed))

(define-method collide chip (thing)
  (cond
    ((or (is-robot thing)
	 (is-trail thing))
     (incf (field-value :chips (player)) %value)
     (recharge (player) 14)
     (play-sound self "chip1")
     (destroy self))
    ((or (is-chip thing)
	 (is-bullet thing))
     nil)
    (t 
     (restore-location self)
     (setf %heading (or (percent-of-time 85 (- pi %heading))
			(- 1 %heading))))))

(defun drop-chips (thing &key (value-multiplier 1) (count (random 3)))
  (dotimes (n count)
    (let ((chip (new chip)))
      (setf (field-value :value chip)
	    (truncate (* value-multiplier (field-value :value chip))))
      (drop thing chip (random 10) (random 10)))))

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

(defparameter *plasmon-images*
  (defresource 
      (:name "plasmon1" :type :image :file "plasmon1.png")
      (:name "plasmon2" :type :image :file "plasmon2.png")
    (:name "plasmon3" :type :image :file "plasmon3.png")
    (:name "plasmon4" :type :image :file "plasmon4.png")
    (:name "plasmon5" :type :image :file "plasmon5.png")))

(defresource
    (:name "plasmon-player" :type :image :file "plasmon-player.png"))

(defparameter *plasmon-sounds*
  (defresource 
      (:name "pip1" :type :sample :file "pip1.wav" :properties (:volume 40))
      (:name "pip2" :type :sample :file "pip2.wav" :properties (:volume 40))
    (:name "pip3" :type :sample :file "pip1.wav" :properties (:volume 40))))

(defun is-bullet (thing)
  (has-tag thing :bullet))

(defun is-player-bullet (thing)
  (and (is-bullet thing)
       (has-tag thing :player)))

(defun is-enemy-bullet (thing)
  (and (is-bullet thing)
       (not (is-player-bullet thing))))

(define-block bullet 
  :radius 3
  :speed 4.2
  :timer 60
  :blend :alpha
  :growth-rate nil
  :tags '(:bullet))

(define-method draw bullet ()
  (draw-circle %x %y %radius 
	       :color (random-choose 
		       (if (is-player-bullet self)
			   '("green" "yellow")
			   '("yellow" "red")))
	       :type :solid))
	      
(define-method update bullet ()
  (decf %timer)
  (if (zerop %timer) 
      (destroy self)
      (move-forward self %speed)))

(define-method collide bullet (thing)
  (cond 
    ;; hit enemies with player bullets
    ((and (is-player-bullet self)
	  (is-enemy thing))
     (damage thing 1)
     (destroy self))
    ;; allow player bullets to pass through trail
    ;; (and through the player)
    ((and (is-player-bullet self)
	  (or (is-trail thing)
	      (is-shield thing)
	      (is-player-bullet thing)
	      (is-robot thing)))
     nil)
    ;; ;; enemy bullets cannot pass trail
    ;; ((is-trail thing)
    ;;  (play-sound self "bonux3")
    ;;  (destroy self))
    ;; enemy bullets don't hurt enemies
    ;; or other enemy bullets
    ((or (is-enemy thing)
	 (is-enemy-bullet thing))
     nil)
    ;; player bullets do not hurt enemy bullets
    ((or 
      (and (is-player-bullet self)
	   (is-enemy-bullet thing))
      (and (is-enemy-bullet self)
	   (is-player-bullet thing)))
     nil)
    ((is-brick thing)
	 ;; (message "BULLET HIT: ~S" 
	 ;; 	  (list (object-name (find-super thing))
	 ;; 		(field-value :part thing)
	 ;; 		(field-value :color thing)
	 ;; 		(field-value :x thing)
	 ;; 		(field-value :y thing)))
     (destroy self))
    ;; by default, just damage whatever it is
    (t (when (has-method :damage thing)
	 (damage thing 1)
	 (destroy self)))))

(define-method initialize bullet (heading &key tags speed radius timer)
  (super%initialize self)
  (setf %heading heading)
  (when speed (setf %speed speed))
  (when timer (setf %timer timer))
  (when radius (setf %radius radius))
  (when tags
    (dolist (tag tags)
      (add-tag self tag))))

;;; Radioactive corruption glitches that creep after you

(defun is-glitch (thing)
  (has-tag thing :glitch))

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
      (:name "blop" :type :sample :file "blop.wav" :properties (:volume 40))))

(defresource (:name "munch1" :type :sample :file "munch1.wav" :properties (:volume 60)))
(defresource (:name "bigboom" :type :sample :file "bigboom.wav" :properties (:volume 60)))

(define-block glitch
  (tags :initform '(:enemy :glitch))
  (image :initform (random-choose *corruption-images*))
  (speed :initform 1)
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

(define-method creep glitch ()
  (point-at-thing self (player))
  (move-forward self %speed)
  (when (< (distance-to-player self) 420)
    (percent-of-time 2 
      (play-sound self "munch1")
      (let ((size (min 120 (* %height 1.2))))
	(resize self size size))
      (incf %speed 0.3))))

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
    (:name "monitor2" :type :image :file "monitor2.png")
    (:name "monitor3" :type :image :file "monitor3.png"))

(define-block monitor 
  (hp :initform 2) fleeing
  (direction :initform (random-choose '(:up :down)))
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
;  (percent-of-time 5 (drop self (new glitch)))
  (move-forward self 3.2))

(define-method stop-fleeing monitor ()
  (setf %fleeing nil))

(define-method hunt monitor ()
  (let ((dist (distance-to-player self)))
    ;; hunt for player
    (if (< dist 250)
	(progn 
	  (setf %heading (heading-to-player self))
	  (move-forward self 2)
	  ;; if close enough, fire and run away 
	  (when (< dist 190)
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
  (setf %image 
	(ecase %hp
	  (2 "monitor2")
	  (1 (random-choose '("monitor" "monitor3")))
	  (0 "monitor")))
  (if %fleeing 
      (flee self)
      (hunt self)))

(define-method collide monitor (thing)
  (when (not (or (is-enemy thing)
		 (is-chip thing)))
    (when (is-robot thing)
      (damage thing 1))
    (restore-location self)
    ;; (when %fleeing (setf %fleeing nil))
    (choose-new-direction self)))

(define-method damage monitor (points)
  (decf %hp)
  (play-sound self (random-choose *whack-sounds*))
  (when (zerop %hp)
    (make-sparks (- %x 16) (- %y 16))
    (play-sound self "xplod")
    (drop-chips self)
    (destroy self)))

(define-method fire monitor (direction)
  (multiple-value-bind (x y) (center-point self)
    (drop self (new bullet (heading-to-player self)))
    (dotimes (n 2)
      (drop self (new bullet 
		      (+ (heading-to-player self) -1.5 (random 3.2))
		      :timer 100)))))

;;; Carriers

(defresource (:name "carrier" :type :image :file "carrier.png"))

(defparameter *wreckage-images*
  (defresource
      (:name "wreckage1" :type :image :file "wreckage1.png")
      (:name "wreckage2" :type :image :file "wreckage2.png")
    (:name "wreckage3" :type :image :file "wreckage3.png")
    (:name "wreckage4" :type :image :file "wreckage4.png")))

(define-block wreckage
  :tags '(:enemy)
  :heading (random (* 2 pi))
  :speed (+ 1 (random 2.5))
  :image (random-choose *wreckage-images*))

(define-method update wreckage ()
  (move-forward self %speed))

(define-method collide wreckage (thing)
  (when (is-robot thing)
    (damage thing 1))
  (when (is-brick thing)
    (destroy self)))

(define-block carrier
  (tags :initform '(:enemy))
  (direction :initform (random-choose '(:up :down :left :right)))
  (hp :initform 15)
  (image :initform "carrier"))

(define-method update carrier ()
  (when (> 550 (distance-to-player self))
    (percent-of-time 20 
      (drop self (new glitch) 40 40)))
  (move-toward self %direction 1))

(define-method collide carrier (thing)
  (when (is-robot thing)
    (damage thing 1))
  (unless (is-enemy thing)
    (restore-location self)
    (setf %direction (random-choose '(:up :down)))))

(define-method damage carrier (points)
  (decf %hp points)
  (if (plusp %hp)
      (play-sound self (random-choose *whack-sounds*))
      (progn 
	(play-sound self "bigboom")
	(play-sound self "xplod")
	(multiple-value-bind (x y) (center-point self)
	  (make-sparks x y 20)
	  (dotimes (n 5)
	    (drop self (new wreckage) (random 50) (random 100)))
	  (dotimes (n 8)
	    (drop self (new bullet (random (* 2 pi)) :radius 8) (random 50) (random 100)))
	  (destroy self)))))

;;; Positronic trail to gather items / block bullets

(defun is-trail (thing)
  (has-tag thing :trail))

(define-block trail
  (tags :initform '(:trail))
  (height :initform 4)
  (width :initform 4))

(define-method draw trail ()
  (draw-box %x %y %width %height :color (random-choose '("yellow" "yellow" "goldenrod"))))

(define-method initialize trail ()
  (later 2.7 (destroy self)))

(define-method collide trail (thing)
  (unless (is-glitch thing)
    (when (is-enemy thing)
      (destroy thing))))

;;; Player shield

(defun is-shield (thing)
  (has-tag thing :shield))

(defparameter *shield-hums*
  (defresource 
      (:name "shield-hum1" :type :sample :file "shield-hum1.wav" :properties (:volume 30))
      (:name "shield-hum2" :type :sample :file "shield-hum2.wav" :properties (:volume 30))))

(defresource 
    (:name "shield-bounce"
     :type :sample :file "shield-bounce.wav" 
     :properties (:volume 90)))

(defparameter *shield-images*
  (defresource
      (:name "shield1" :type :image :file "shield1.png")
      (:name "shield2" :type :image :file "shield2.png")
    (:name "shield3" :type :image :file "shield3.png")))

(define-block shield :image "shield3" :tags '(:shield))

(define-method update shield ()
  (percent-of-time 8 (play-sample (random-choose *shield-hums*)))
  (setf %image (random-choose *shield-images*)))

(define-method collide shield (thing)
  (when (is-enemy-bullet thing)
    (play-sample "doorbell")
    (destroy thing)))

;;; The player

(defresource
  (:name "robot" :type :image :file "robot.png")
  (:name "win" :type :image :file "win.png")
  (:name "lose" :type :image :file "lose.png"))

(define-block win :image "win")
(define-block lose :image "lose")

(defun is-robot (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

(define-block robot 
  (dead :initform nil)
  (ready :initform t)
  ;; we want to catch the beginning of firing, even if the input
  ;; polling in `update' misses it. (see below)
  (image :initform "robot")
  (height :initform 16)
  (width :initform 16)
  (hp :initform 3)
  (speech-timer :initform 0)
  (energy :initform 100)
  (recharge-timer :initform 0)
  (trail-timer :initform 2)
  (chips :initform 0)
  (item :initform :shield)
  (shields :initform nil)
  (tags :initform '(:robot))
  (speed :initform 2))

(define-method say robot (sample)
  (with-fields (speech-timer) self
    (when (zerop speech-timer)
      (play-sample sample)
      (setf speech-timer 280))))

(define-method initialize robot ()
  (super%initialize self)
  (bind-event self '(:joystick :button-down :l2) :raise-shields)
  (bind-event self '(:joystick :button-up :l2) :lower-shields))
  ;; (bind-event self '(:joystick :button-down :r2) :start-firing)
  ;; (bind-event self '(:joystick :button-up :r2) :stop-firing))

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

;; give the player a few extra 
(define-method bounding-box robot ()
  (let ((margin 2))
    (values (+ %y margin) 
	    (+ %x margin)
	    (+ %x %width (- margin))
	    (+ %y %height (- margin)))))

(defparameter *shield-spread* (radian-angle 150))
(defparameter *shield-units* 12)
(defparameter *shield-distance* 50)

(define-method raise-shields robot ()
  (with-fields (energy heading) self
    (when (plusp energy)
      (let (shields)
	(dotimes (n *shield-units*)
	  (push (new shield) shields)
	  (drop self (first shields))
	  (setf %shields shields))))))

(define-method update-shields robot ()
  (with-fields (shields heading) self
    (when shields
      (charge self 0.36)
      (if (zerop %energy)
	  (lower-shields self)
	  (let ((angle (- heading (/ *shield-spread* 2)))
		(delta (/ *shield-spread* *shield-units*)))
	    (dolist (shield shields)
	      (multiple-value-bind (x y) (step-toward-heading self angle *shield-distance*)
		(move-to shield (- x 4) (- y 4))
		(incf angle delta))))))))

(define-method lower-shields robot ()
  (with-fields (shields) self
    (when shields
      (mapc #'destroy shields)
      (setf shields nil))))

(define-method reset robot ()
  (xalcyon))

(define-method reload robot ()
  (setf %ready t))

(defun player-bullet (heading)
  (new bullet heading :speed 7 :tags '(:player)))

(define-method charge robot (amount)
  (assert (plusp amount))
  (setf %energy 
	(max 0 (decf %energy amount))))

(define-method recharge robot (amount)
  (assert (plusp amount))
  (setf %energy 
	(min 100 (incf %energy amount))))

(define-method fire robot (heading)
  (when (and %ready (not %dead) (not (zerop %energy)))
    (charge self 1)
    (setf %ready nil)
    (later 8 (reload self))
    (play-sound self "zap")
    (drop self (player-bullet heading)
	  (/ %width 2) (/ %height 2))))

(define-method damage robot (points)
  (when (not %dead)
    (play-sound self (defresource :name "deathx" :type :sample :file "deathx.wav" :properties (:volume 100)))
    (setf %dead t)
    (let ((sign (new lose)))
      (drop self sign 32 32))
;      (center sign))
    (percent-of-time 50
      (let ((message (random-choose '("vox-shield-warning" "vox-repair" "vox-hazard"))))
	(later 1.4 (say self message))))
    (change-image self (defresource :name "skull" :type :image :file "skull.png"))))

(define-method win robot ()
  (play-music "vixon")
  (let ((sign (new win)))
    (drop self sign 32 32)))
;l    (center sign)))

(define-method collide robot (thing)
  (when (is-enemy thing)
    (damage self 1))
  (when (is-brick thing)
    (restore-location self)))

(define-method aim robot (angle)
  (setf %heading angle))

(define-method drop-trail-maybe robot ()
  (decf %trail-timer)
  (when (zerop %trail-timer)
    (drop self (new trail) 6 6)
    (setf %trail-timer 2)))

(define-method auto-recharge robot ()
  (with-fields (dead recharge-timer) self
    (when (zerop recharge-timer)
      (setf recharge-timer 10)
      (recharge self 2))
    (decf recharge-timer)))

(define-method update robot ()
  (when (not %dead)
    (when (plusp %speech-timer)
      (decf %speech-timer))
    (when (< %energy 20)
      (say self "vox-energy"))
    (auto-recharge self)
    (update-shields self)
    (when (left-analog-stick-pressed-p)
      (aim self (left-analog-stick-heading))
      (move-forward self 3.5)
      (drop-trail-maybe self))
    (if (right-analog-stick-pressed-p)
	(progn (aim self (right-analog-stick-heading))
	       (when (joystick-button-pressed-p :r2)
		 (fire self (right-analog-stick-heading))))
	(when (joystick-button-pressed-p :r2)
	  (fire self %heading)))))
	
;;; Stationary bases that generate enemies

(defresource (:name "base" :type :image :file "generator.png"))

(define-block base :image "base" :ready nil :timer 70 :tags '(:enemy) :hp 15)

(define-method update base ()
  (when (< (distance-to-player self) 240)
    (decf %timer)
    (when (zerop %timer)
      (setf %timer 100)
      (percent-of-time 6
	(play-sample "vox-radiation")
	(dotimes (n 3)
	  (drop self (new glitch))))
      (drop self (new monitor)))))

(define-method damage base (points)
  (decf %hp)
  (play-sound self (random-choose *whack-sounds*))
  (when (zerop %hp)
    (make-sparks (- %x 16) (- %y 16))
    (play-sound self "xplod")
    (drop-chips self :value-multiplier 10 :count 8)
    (destroy self)))

;;; The reactor

(define-world reactor
  level-clear
  (background-color :initform (theme-color :background))
  (grid-size :initform 32)
  (grid-width :initform 32)
  (grid-height :initform 32))

(define-block reactor-turtle :image "robot" :x 10 :y 10)

(define-method draw reactor-turtle ())

(define-method draw-wall reactor-turtle (segments &optional (size 32))
  (dotimes (n segments)
    (let ((brick (new brick)))
      (drop self brick)
      (resize brick size size)
      (move-forward self (+ size 0.02)))))

(define-method draw-square reactor-turtle (size &optional (segment-size 32))
  (dotimes (n 4)
    (draw-wall self size segment-size)
    (turn-right self 90)))

(define-method skip-wall reactor-turtle (segments)
  (move-forward self (+ 0.02 (* 32 segments))))

(define-method draw-room reactor-turtle (size &optional (segment-size 32))
  (let ((gap (1+ (random 2))))
    (drop self (new base)
	  (+ 32 (random 64))
	  (+ 32 (random 64)))
    (dotimes (n 4)
      (draw-wall self (- size gap) segment-size)
      (skip-wall self 2)
      (draw-wall self 2 segment-size)
      (turn-right self 90))))

(define-method draw-base reactor-turtle ()
  (setf %heading 0)
  (percent-of-time 40 
    (draw-room self (+ 7 (random 3)) (+ 8 (random 7))))
  (skip-wall self 8))
    
(define-method run reactor-turtle ()
  (draw-square self 31)
  (move-to self 90 90)
  (dotimes (n 3)
    (draw-base self))
  (move-to self 90 300)
  (dotimes (n 3)
    (draw-base self))
  (move-to self 90 600))

(define-method build reactor (level)
  (setf *theme* (random-theme))
  (setf %window-scrolling-speed 4)
  (let ((*quadtree* %quadtree))
    (with-fields (grid-width grid-height) self
      (move-window-to self 0 0)
      (let ((turtle (new reactor-turtle)))
      	(drop self turtle)
      	(run turtle)
      	(destroy turtle)))))

(define-method draw reactor ()
  (draw%%world self)
  ;; heads up display
  (multiple-value-bind (top left right bottom)
      (window-bounding-box self)
    (with-field-values (energy chips item) (player)
    (let* ((font *xalcyon-font*)
	   (line-height (font-height font))
	   (x (+ left (dash 5)))
	   (y (- bottom line-height (dash 2)))
	   (label (format nil "ENERGY: ~3,2f      CHIP: ~d       ITEM: ~A" 
			  energy chips item))
	   (bar-width 200))
      ;; draw energy bar 
      (draw-box x y bar-width line-height :color "gray30")
      (when (plusp energy)
	(draw-box x y (* 2 energy) line-height 
		  :color (cond 
			   ((>= energy 85) "green")
			   ((>= energy 70) "yellow")
			   ((>= energy 40) "orange")
			   ((>= 20 energy) "red")
			   (t "orange"))))
      ;; show stats
      (draw-string label 
		   (+ x 200 (dash)) y 
		   :color "white"
		   :font *xalcyon-font*)))))

(define-method update reactor ()
  (update%%world self)
  (unless %level-clear
    (unless (block finding
	      (loop for sprite being the hash-keys in %sprites do
		(when (is-enemy sprite)
		  (return-from finding t))))
      (setf %level-clear t)
      (win %player))))
    
(defun xalcyon ()
  (let ((robot (new robot))
	(reactor (new reactor)))
    (set-location robot 60 60)
    (bind-event reactor '(:escape) :reset)
    (new universe 
	 :player robot
	 :world reactor)
    (build reactor 1)))
    ;; (play-music (random-choose '("phong" "beatup" "wraparound" "defmacron")) :loop t)
    ;; (play-music "beatup" :loop t)))

(define-method reset reactor ()
  (xalcyon))

;;; xalcyon.lisp ends here
