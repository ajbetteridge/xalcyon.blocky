;;; xalcyon.lisp --- a multidirectional arcade shooter for dual analog sticks

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

;; (add-to-list 'imenu-generic-expression '("Blocks" "^(define-block[:space:]+\\(\\[:word:]+\\)" 1))

(defpackage :xalcyon 
  (:use :blocky :common-lisp))

(in-package :xalcyon)

(defvar *level* 0)

(setf *author* "David T. O'Toole <dto@ioforms.org> http://dto.github.com/notebook/")
(setf *screen-width* 1280)
(setf *screen-height* 720)
;; (setf *nominal-screen-width* 640)
;; (setf *nominal-screen-height* 360)
(setf *default-texture-filter* :nearest)
;(setf *font-texture-filter* :nearest)
(setf *scale-output-to-window* nil) 
(setf *resizable* t)
(setf *window-title* "Xalcyon")
(setf *use-antialiased-text* nil)
(setf *frame-rate* 30)
(setf *dt* 20)

(defparameter *xalcyon-font* "sans-bold-12") 

(defun is-enemy (thing) 
  (has-tag thing :enemy))

(defun is-target (thing)
  (has-tag thing :target))

;;; Some sounds and images

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

(defparameter *vox-sounds*
  (defresource 
      (:name "robovoxx" :type :sample :file "robovoxx.wav" :properties (:volume 130))
      (:name "vox-energy" :type :sample :file "vox-energy.wav" :properties (:volume 180))
      (:name "vox-hazard" :type :sample :file "vox-hazard.wav" :properties (:volume 180))
    (:name "vox-message" :type :sample :file "vox-message.wav" :properties (:volume 180))
    (:name "vox-radiation" :type :sample :file "vox-radiation.wav" :properties (:volume 180))
    (:name "vox-repair" :type :sample :file "vox-repair.wav" :properties (:volume 180))
    (:name "vox-restored" :type :sample :file "vox-restored.wav" :properties (:volume 180))
      (:name "vox-shield-pickup" :type :sample :file "vox-shield-pickup.wav" :properties (:volume 180))
      (:name "vox-bomb-pickup" :type :sample :file "vox-bomb-pickup.wav" :properties (:volume 180))
      (:name "vox-base-destroyed" :type :sample :file "vox-base-destroyed.wav" :properties (:volume 180))
      (:name "vox-shutdown" :type :sample :file "vox-shutdown.wav" :properties (:volume 180))
      (:name "vox-you-lose" :type :sample :file "vox-you-lose.wav" :properties (:volume 180))
      (:name "vox-winning" :type :sample :file "vox-winning.wav" :properties (:volume 180))
      (:name "vox-shield-warning" :type :sample :file "vox-shield-warning.wav" :properties (:volume 180))
      (:name "vox-mission-completed" :type :sample :file "vox-mission-completed.wav" :properties (:volume 180))
      (:name "vox-now-entering" :type :sample :file "vox-now-entering.wav" :properties (:volume 180))
      (:name "vox-proceed-to" :type :sample :file "vox-proceed-to.wav" :properties (:volume 180))
      (:name "vox-sector" :type :sample :file "vox-sector.wav" :properties (:volume 180))
      (:name "vox-virus-detected" :type :sample :file "vox-virus-detected.wav" :properties (:volume 180))))

(defparameter *vox-digits*
  (defresource
      (:name "vox-0" :type :sample :file "vox-0.wav" :properties (:volume 180))
      (:name "vox-1" :type :sample :file "vox-1.wav" :properties (:volume 180))
    (:name "vox-2" :type :sample :file "vox-2.wav" :properties (:volume 180))
    (:name "vox-3" :type :sample :file "vox-3.wav" :properties (:volume 180))
    (:name "vox-4" :type :sample :file "vox-4.wav" :properties (:volume 180))
    (:name "vox-5" :type :sample :file "vox-5.wav" :properties (:volume 180))
    (:name "vox-6" :type :sample :file "vox-6.wav" :properties (:volume 180))
    (:name "vox-7" :type :sample :file "vox-7.wav" :properties (:volume 180))
    (:name "vox-8" :type :sample :file "vox-8.wav" :properties (:volume 180))
    (:name "vox-9" :type :sample :file "vox-9.wav" :properties (:volume 180))
    (:name "vox-10" :type :sample :file "vox-10.wav" :properties (:volume 180))))

(defparameter *vox-letters*
  (defresource
      (:name "vox-alpha" :type :sample :file "vox-alpha.wav" :properties (:volume 180))
      (:name "vox-beta" :type :sample :file "vox-beta.wav" :properties (:volume 180))
    (:name "vox-gamma" :type :sample :file "vox-gamma.wav" :properties (:volume 180))
    (:name "vox-delta" :type :sample :file "vox-delta.wav" :properties (:volume 180))
    (:name "vox-epsilon" :type :sample :file "vox-epsilon.wav" :properties (:volume 180))))

(defparameter *letters* '(:alpha :beta :gamma :delta :epsilon))

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
 
(defparameter *dance-tracks* 
  (defresource 
    (:name "beatup" :type :music :file "beatup.ogg")
    (:name "melcrov8r" :type :music :file "melcrov8r.ogg"  :properties (:volume 80))
    (:name "wraparound" :type :music :file "wraparound.ogg" :properties (:volume 200))))

(defparameter *ambient-tracks*
  (defresource
    (:name "remembering-xalcyon" :type :music :file "remembering-xalcyon.ogg")
    (:name "xioforms" :type :music :file "xioforms.ogg")
    (:name "xiomacs" :type :music :file "xiomacs.ogg")
    (:name "phong" :type :music :file "phong.ogg" :properties (:volume 50))
    (:name "xmrio" :type :music :file "xmrio.ogg")
    (:name "rappy" :type :music :file "rappy.ogg")
    (:name "invec" :type :music :file "invec.ogg")
    (:name "theme3" :type :music :file "theme3.ogg")
    (:name "xalcyon" :type :music :file "xalcyon.ogg")
    (:name "vedex" :type :music :file "vedex.ogg")
    (:name "ompula" :type :music :file "ompula.ogg")))
    
(defparameter *special-tracks*
  (defresource 
    (:name "suspiria-synthesis" :type :music :file "suspiria-synthesis.ogg")
      (:name "vixon" :type :music :file "vixon.ogg")
      (:name "nexttime" :type :music :file "nexttime.ogg")))

(defparameter *soundtrack*
  (append *dance-tracks* *ambient-tracks*))

;;; Colored, themeable bricks that make up the environment

(defparameter *themes* 
  '((:dec :background "DarkSlateBlue" :brick "SlateBlue" :brick2 "hot pink" :wall "Black")
    (:tandy :background "gray20" :brick "red" :brick2 "gray30" :wall "gray20")
    (:vax :background "gray20" :brick "gray50" :brick2 "gray30" :wall "gray20")
    (:command :background "black" :brick "gold" :brick2 "gray40" :wall "gray20")
    (:surround :background "DarkOliveGreen" :brick "GreenYellow" :brick2 "gray40" :wall "gray20")
    (:maynard :background "saddle brown" :brick "DarkOrange" :brick2 "gray40" :wall "gray20")
    (:zerk :background "black" :brick "maroon2" :brick2 "cyan" :wall "black")))

(defun random-theme () (random-choose (mapcar #'car *themes*)))

(defparameter *theme* :vax)

(defun theme-color (&optional (part :brick))
  (let ((theme (assoc *theme* *themes*)))
    (getf (rest theme) part)))

(define-block brick 
  :tags '(:brick)
  :part :brick
  :collision-type :passive
  :color (theme-color))

(define-method layout brick ())

(define-method bounding-box brick ()
  ;; shrink bounding box by 1 px to prevent adjacent bricks from
  ;; resting contact
  (with-field-values (x y height width) self
    (values (+ 0.1 y) (+ 0.1 x)
	    (+ -0.1 x width)
	    (+ -0.1  y height))))

(define-method damage brick (points) nil)

(define-method initialize brick (&optional color)
  (initialize%super self)
  (resize self 16 16)
  (setf %color color))

(defmacro defbrick (name &body body)
  `(define-block (,name :super "XALCYON:BRICK")
     ,@body))

(defun is-brick (thing)
  (and (blockyp thing)
       (has-tag thing :brick)))

(define-method draw brick ()
  (draw-box %x %y %width %height :color (theme-color %part)))

;;; Electric death paddles!

(define-block paddle :tags '(:paddle) :phase (random pi) :heading 0.0)

(define-method initialize paddle ()
  (initialize%super self)
  (resize self 100 12))

(define-method draw paddle ()
  (let ((index (truncate (* 100 (sin %phase)))))
    (when (< (distance-to-player self) 400)
      (draw-box 0 (+ %y 5) 10000 2
		:color (random-choose '("red" "yellow" "magenta"))))
    (draw-box %x %y %width %height :color (percent-gray index))))

(define-method update paddle ()
  (let ((speed
	  (if (< (distance-to-player self)
		 380)
	      (level-value 9 11 13 13)
	      (level-value 5 6 7 8))))
    (incf %phase (/ speed 100))
    (forward self speed)))

(define-method collide paddle (thing)
  (when (is-brick thing)
    (restore-location self)
    (setf %heading (- %heading pi)))
  (when (is-robot thing)
    (damage thing 1)))

;;; Deadly burning gas clouds

(defparameter *vent-sounds*
  (defresource 
      (:name "geiger1" :type :sample :file "geiger1.wav" :properties (:volume 40))
      (:name "geiger2" :type :sample :file "geiger2.wav" :properties (:volume 40))
    (:name "geiger3" :type :sample :file "geiger3.wav" :properties (:volume 40))
    (:name "geiger4" :type :sample :file "geiger4.wav" :properties (:volume 40))))

(defparameter *vent-images*
  (defresource 
      (:name "vent" :type :image :file "vent.png")
      (:name "vent2" :type :image :file "vent2.png")
      (:name "vent3" :type :image :file "vent3.png")
      (:name "vent4" :type :image :file "vent4.png")
      (:name "vent5" :type :image :file "vent5.png")))

(define-block cloud 
  :timer 300
  :collision-type :passive
  :tags '(:cloud)
  :image "vent")

(define-method draw cloud ()
  (with-field-values (x y width height image) self
    (let ((jitter (random 10)))
      (when (> jitter 7)
	(incf %heading (random-choose '(-0.3 0.2))))
      (set-blending-mode :additive2)
      (draw-image image
		  (- x jitter)
		  (- y jitter)
		  :width (+ width jitter)
		  :height (+ height jitter)
		  :opacity 1)
      (dotimes (n 4) 
	(draw-box (+ x -5 (random height))
		  (+ y -5 (random width))
		  (+ 5 (random 8))
		  (+ 5 (random 8))
		  :color (random-choose '("white" "magenta"))
		  :alpha 0.7)))))

(define-method initialize cloud (&optional (size (+ 16 (random 32))))
  (initialize%super self)
  (resize self size size))

(define-method update cloud ()
  (forward self 1)
  (decf %timer)
  (when (evenp %timer)
    (setf %image (random-choose *vent-images*)))
  (when (< (distance-to-player self) 300)
    (percent-of-time 4 (play-sample (random-choose *vent-sounds*))))
  (unless (plusp %timer)
    (destroy self)))

(defparameter *vent-hole-images*
  (defresource 
      (:name "vent-hole1" :type :image :file "vent-hole1.png")
      (:name "vent-hole2" :type :image :file "vent-hole2.png")
      (:name "vent-hole3" :type :image :file "vent-hole3.png")))

(define-block vent 
  :image "vent-hole1" :tags '(:enemy :vent) 
  :hp 22
  :timer 0)

(define-method update vent ()
  (with-fields (timer) self 
    (when (plusp timer)
      (percent-of-time 25 
	(play-sample "magenta-alert")
	(drop self (new 'bullet (heading-to-player self)) 20 20))
      (decf timer))
    (when (zerop timer)
      (percent-of-time 3
	(when (< (distance-to-player self) 350)
	  (setf timer 20))))
    (percent-of-time 2.35
      (percent-of-time 4 (drop self (new "XALCYON:ROOK") 140 140))
      (drop self (new "XALCYON:CLOUD") 40 40))))

(define-method damage vent (points)
  (play-sample (random-choose *corruption-sounds*))
  (decf %hp)
  (unless (plusp %hp)
    (make-explosion self)
    (play-sample "bigboom")
    (drop-chips self :value-multiplier 32)
    (destroy self)))

(define-method draw vent ()
  (with-field-values (x y height width) self
    (draw-box x y width height :color "black")
    (let ((border (+ 5 (random 20))))
      (draw-box (+ x border)
		(+ y border)
	        (- width (* 2 border))
		(- height (* 2 border))
		:color 
		(if (< %hp 8)
		    (random-choose '("yellow" "orange" "white"))
		    (random-choose '("yellow" "orange" "magenta" "HotPink" "red"))))
      (set-blending-mode :additive2)
      (draw-image (random-choose *vent-hole-images*) 
		  %x %y))))

(define-method collide vent (thing)
  (if (is-brick thing)
      (destroy self)
      (when (is-player-bullet thing)
	(damage self 1)
	(play-sample (random-choose *whack-sounds*)))))
	  
;;; Breakable block barriers that pass enemies and enemy bullets but
;;; not player bullets or the player

(defun is-barrier (thing)
  (has-tag thing :barrier))

(defparameter *barrier-hp* 12)

(define-block barrier 
  :hp *barrier-hp*
  :color "magenta"
  :tags '(:barrier))

(define-method draw barrier ()
  (percent-of-time 15 (setf %color (random-choose '("cyan" "DeepSkyBlue"))))
  (with-field-values (x y width height hp) self
    (let ((edge (max 0 (* 0.5 (- width (* width (/ hp *barrier-hp*)))))))
      (draw-box x y width height :color (random-choose '("red" "yellow")))
      (draw-box (+ x edge) (+ y edge)
		(- width (* edge 2))
		(- height (* edge 2))
		:color %color))))

(define-method damage barrier (points)
  (assert (plusp points))
  (decf %hp points)
  (make-sparks %x %y 1)
  (play-sample "shield-bounce")
  (when (not (plusp %hp))
    (destroy self)))

(define-method bounding-box barrier ()
  ;; shrink bounding box by 1 px to prevent adjacent bricks from
  ;; resting contact
  (with-field-values (x y height width) self
    (values (+ 0.1 y) (+ 0.1 x)
	    (+ -0.1 x width)
	    (+ -0.1  y height))))

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
  (forward self %speed))

(define-method collide chip (thing)
  (cond
    ((or (is-robot thing)
	 (is-trail thing))
     (incf (field-value :chips (player)) %value)
     (recharge (player) 20)
     (play-sound self "chip1")
     (destroy self))
    ((is-brick thing) 
     (restore-location self)
     (setf %heading (or (percent-of-time 85 (- pi %heading))
			(- 1 %heading))))))

(defun drop-chips (thing &key (value-multiplier 1) (count (random 3)))
  (dotimes (n count)
    (let ((chip (new 'chip)))
      (setf (field-value :value chip)
	    (truncate (* value-multiplier (field-value :value chip))))
      (drop thing chip (random 10) (random 10)))))

;;; Sparkle explosion cloud fx

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
		(new 'spark) 
		(+ x (random 30)) (+ y (random 30)))))

;;; Versatile bullets

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
      (forward self %speed)))

(define-method collide bullet (thing)
  (cond 
    ;; let bullets pass through clouds
    ((has-tag thing :cloud)
     nil)
    ;; let enemy bullets pass through barriers
    ((and (is-barrier thing)
	  (is-enemy-bullet self))
     nil)
    ;; don't get hung up on collectibles
    ((or (is-powerup thing)
	 (is-chip thing))
     nil)
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
    ;; by default, just damage whatever it is
    (t (when (has-method :damage thing)
	 (damage thing 1)
	 (destroy self)))))

(define-method initialize bullet (heading &key tags speed radius timer)
  (initialize%super self)
  (setf %heading heading)
  (when speed (setf %speed speed))
  (when timer (setf %timer timer))
  (when radius (setf %radius radius))
  (setf %height 
	(setf %width
	      (* 1.8 %radius)))
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
  (forward self %speed)
  (when (< (distance-to-player self) 460)
    (percent-of-time 2 
      (play-sound self "munch1")
      (let ((size (* %height 1.3)))
	(resize self size size))
      (incf %speed 0.3))))

(define-method update glitch ()
  (percent-of-time 3 (change-image self (random-choose *corruption-images*)))
  (creep self)
  (percent-of-time 3 
    (set-overlay self)
    (later 20 (clear-overlay self))))

(define-method draw glitch ()
  (draw%super self)
  (set-blending-mode :additive2)
  (when %overlay-color
    (draw-box %x %y %width %height
     :alpha 0.2
     :color %overlay-color)))

;;; The "monitor", a roving enemy that fires a spread of bullets then dashes away

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
  (forward self 3.2))

(define-method stop-fleeing monitor ()
  (setf %fleeing nil))

(defresource (:name "magenta-alert"
	      :type :sample :file "magenta-alert.wav" 
	      :properties (:volume 60)))

(define-method hunt monitor ()
  (let ((dist (distance-to-player self)))
    ;; hunt for player
    (if (< dist 250)
	(progn 
	  (setf %heading (heading-to-player self))
	  (forward self 2)
	  ;; if close enough, fire and run away 
	  (when (< dist 190)
	    (fire self (heading-to-player self))
	    (setf %fleeing t)
	    (later 1.4 (stop-fleeing self))
	    (play-sound self "magenta-alert")))
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
		 (is-powerup thing)
		 (is-chip thing)
		 ;; allow monitor to pass through barriers
		 (is-barrier thing)))
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
    (percent-of-time 70 (drop self (random-powerup)))
    (destroy self)))

(define-method fire monitor (direction)
  (multiple-value-bind (x y) (center-point self)
    (drop self (new 'bullet (heading-to-player self)))
    (dotimes (n (level-value 0 1 2 3))
      (drop self (new 'bullet 
		      (+ (heading-to-player self) -1.5 (random 3.2))
		      :timer 100)))))

;;; Biclops

(defresource (:name "biclops" :type :image :file "biclops.png"))

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
  (forward self (level-value 1 1.5 2 2.3 3)))

(define-method collide wreckage (thing)
  (when (is-robot thing)
    (damage thing 1))
  (when (is-brick thing)
    (make-sparks %x %y 2)
    (destroy self)))

(defun is-biclops (thing)
  (has-tag thing :biclops))

(define-block biclops
  (tags :initform '(:enemy :biclops))
  (direction :initform (random-choose '(:up :down :left :right)))
  (hp :initform 17)
  (image :initform "biclops"))

(define-method update biclops ()
  (when (> (level-value 300 400 500) (distance-to-player self))
    (percent-of-time (level-value 0.8 1 1.2 1.3)
      (drop self (new 'glitch) 10 2)))
  (move-toward self %direction (level-value 1 1.8 2.5 3)))

(define-method collide biclops (thing)
  (when (is-robot thing)
    (damage thing 1))
  (unless (is-enemy thing)
    (restore-location self)
    (setf %direction (random-choose '(:up :down :left :right)))))

(define-method damage biclops (points)
  (decf %hp points)
  (if (plusp %hp)
      (play-sound self (random-choose *whack-sounds*))
      (progn 
	(play-sound self "bigboom")
	(play-sound self "xplod")
	(multiple-value-bind (x y) (center-point self)
	  (make-sparks x y 3)
	  (percent-of-time (level-value 10 20 30) (make-explosion self 4))
	  (dotimes (n 5)
	    (drop self (new 'wreckage) (random 30) (random 30)))
	  (dotimes (n 8)
	    (drop self (new 'bullet (random (* 2 pi))) (random 50) (random 100)))
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
      (damage thing 1))))

;;; Player shield

(defun is-shield (thing)
  (has-tag thing :shield))

(defparameter *shield-hums*
  (defresource 
      (:name "shield-hum1" :type :sample :file "shield-hum1.wav" :properties (:volume 20))
      (:name "shield-hum2" :type :sample :file "shield-hum2.wav" :properties (:volume 20))))

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
  (percent-of-time 4 (play-sample (random-choose *shield-hums*)))
  (setf %image (random-choose *shield-images*)))

(define-method collide shield (thing)
  (when (is-enemy-bullet thing)
    (play-sample "doorbell")
    (destroy thing)))

;;; Sticky bombs

(defun is-bomb (thing)
  (has-tag thing :bomb))

(defparameter *bomb-images*
  (defresource 
      (:name "bomb1" :type :image :file "bomb1.png")
      ;; deliberate repeat
      (:name "bomb1" :type :image :file "bomb1.png")
      (:name "bomb2" :type :image :file "bomb2.png")
    (:name "bomb3" :type :image :file "bomb3.png")
    (:name "bomb4" :type :image :file "bomb4.png")))

(defparameter *explosion-images*
  (defresource
      (:name "bomb-flash1" :type :image :file "bomb-flash1.png")
      (:name "bomb-flash2" :type :image :file "bomb-flash2.png")
    (:name "bomb-flash3" :type :image :file "bomb-flash3.png")
    (:name "explosion" :type :image :file "explosion.png")
    (:name "explosion2" :type :image :file "explosion2.png")))

(define-block explosion :timer (+ 20 (random 12)) :image "explosion")

(define-method update explosion ()
  (decf %timer)
  (if (zerop %timer)
      (destroy self)
      (progn
	(setf %image (random-choose *explosion-images*))
	(percent-of-time 4 (play-sample "explode"))
	(resize self (+ 16 (random 16)) (+ 16 (random 16)))
	(move-toward self (random-direction) (+ 6 (random 8))))))

(define-method collide explosion (thing)
  (when (is-brick thing) 
    (restore-location self))
  (unless (is-rook thing)
    (damage thing 2)))

(defun make-explosion (thing &optional (size 8))
  (multiple-value-bind (x y) (center-point thing)
    (dotimes (n size)
      (add-object (world) (new 'explosion) x y))))
  
(defresource
    (:name "mine" :type :image :file "bomb.png")
    (:name "bomb-ammo" :type :image :file "bomb-ammo.png")
    (:name "shield-ammo" :type :image :file "shield-ammo.png")
    (:name "energy-ammo" :type :image :file "energy-ammo.png")
    (:name "powerup" :type :sample :file "powerup.wav")
    (:name "bombs-away" :type :sample :file "bombs-away.wav" :properties (:volume 70))
    (:name "power" :type :sample :file "power.wav")
    (:name "powerdown" :type :sample :file "powerdown.wav")
    (:name "countdown" :type :sample :file "countdown.wav" :properties (:volume 40))
    (:name "explode" :type :sample :file "explode.wav" :properties (:volume 100)))

(define-block bomb :timer 0 :countdown 5 
  :image "bomb4" :target nil
  :stopped nil :speed 4
  :origin nil)

(define-method explode bomb ()
  (make-explosion self)
  (destroy self))

(define-method initialize bomb (heading &key origin)
  (initialize%super self)
  (setf %image "bomb4")
  (setf %origin origin)
  (aim self heading))

(define-method collide bomb (thing)
  (cond 
    ;; player bullets destroy bombs
    ((is-player-bullet thing)
     (explode self))
    ;; bombs should not stick to who fired them
    ((and %origin 
	  (is-enemy %origin)
	  (is-enemy thing))
     nil)
    ;; enemy bombs stop at player and trail
    ((and %origin 
	  (is-enemy %origin)
	  (or (is-robot thing)
	      (is-trail thing)))
     (setf %stopped t))
    ;; stick to enemies
    ((is-enemy thing)
     (setf %target thing))
    ;; stop at walls
    ((or (is-brick thing)
	 (is-barrier thing))
     (restore-location self))))

(define-method update bomb () 
  (if %target
      ;; stick to target once touched
      (multiple-value-bind (x y) (center-point %target)
	(move-to self x y))
      ;; move in straight line to find target
      (unless %stopped
	(forward self %speed)))
  ;; possibly explode and/or update timer
  (with-fields (countdown timer image) self
    (if (zerop countdown)
	(explode self)
	(if (plusp timer)
	    (decf timer)
	    (progn
	      (setf timer 20)
	      (play-sample "countdown")
	      (decf countdown)
	      (setf image (nth countdown *bomb-images*)))))))

;;; A bomber guy who shoots bombs at you

(defresource 
    (:name "rook" :type :image :file "rook.png")
    (:name "rook2" :type :image :file "rook2.png"))

(defun is-rook (thing)
  (has-tag thing :rook))

(define-block rook 
  :image "rook2" 
  :hp 10
  :tags '(:rook :enemy)
  :timer 0
  :fleeing nil)

(define-method damage rook (points)
  (decf %hp)
  (play-sound self (random-choose *whack-sounds*))
  (when (zerop %hp)
    (make-explosion self 5)
    (play-sound self "xplod")
    (drop-chips self :value-multiplier 5)
    (percent-of-time 40 (drop self (random-powerup)))
    (destroy self)))

(define-method fire rook (heading)
  (drop self (new 'bomb heading :origin self)))

(define-method update rook ()
  (with-fields (timer) self
    (setf timer (max 0 (1- timer))) 
    (let ((dir (heading-to-player self))
	  (dist (distance-to-player self)))
      (cond 
	;; shoot bomb then set flag to run away
	((and (< dist 280) 
	      (zerop timer))
	 ;; don't always fire
	 (percent-of-time 65 
	   (play-sample "robovoxx")
	   (fire self dir))
	 (aim self (- dir 0.52))
	 (setf timer 90))
	;; begin approach after staying still
	((and (< dist 570) (zerop timer))
	 (aim self dir)
	 (forward self 2))
	;; run away fast
	((and (< dist 420) (plusp timer))
	 (aim self (- %heading 0.03))
	 (percent-of-time (level-value 0 1.0 1.3) 
	   (play-sample "magenta-alert")
	   (drop self (new 'bullet (heading-to-player self)) 14 14))
	 (forward self 3))
	;; otherwise do nothing
	))))

(define-method collide rook (thing)
  (cond 
    ((or (is-brick thing) (is-enemy thing))
     (restore-location self)
     (setf %timer 40))
    ((is-robot thing)
     (damage thing 1))))

;;; Powerup items

(defun is-powerup (thing)
  (has-tag thing :powerup))

(define-block bomb-ammo :image "bomb-ammo" :tags '(:powerup))

(define-method collide bomb-ammo (thing)
  (when (is-robot thing)  
    (play-sample "powerup")
    (play-sample "vox-bomb-pickup")
    (recharge thing 100)
    (equip thing :bomb)
    (destroy self)))

(define-block shield-ammo :image "shield-ammo" :tags '(:powerup))

(define-method collide shield-ammo (thing)
  (when (is-robot thing)
    (play-sample "powerup")
    (play-sample "vox-shield-pickup")
    (recharge thing 100)
    (equip thing :shield)
    (destroy self)))

(define-block energy-ammo :image "energy-ammo" :tags '(:powerup))

(define-method collide energy-ammo (thing)
  (when (is-robot thing)
    (play-sample "vox-restored")
    (recharge thing 100)
    (destroy self)))

(defun random-powerup ()
  (clone (random-choose '("XALCYON:ENERGY-AMMO" "XALCYON:BOMB-AMMO" "XALCYON:SHIELD-AMMO"))))

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
  (hp :initform 1)
  (glide-heading :initform nil)
  (speech-timer :initform 0)
  (bomb-loaded :initform t)
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

(define-method speak-symbol robot (thing)
  (cond 
    ((typep thing '(integer 0 10))
     (play-sample (nth thing *vox-digits*)))
    ((member thing *letters*)
     (play-sample (nth (position thing *letters*) *vox-letters*)))))

(define-method announce robot (letter &optional (level *level*))
  (play-sample "vox-sector")
      (later 1.0 (speak-symbol self letter))
  (if (eq :epsilon letter)
      (later 2.5 (speak-symbol self level))
      (later 2.0 (speak-symbol self level))))

(define-method initialize robot ()
  (block%initialize self)
  (bind-event self '(:joystick :left-trigger :button-down) :activate-extension)
  (bind-event self '(:joystick :left-trigger :button-up) :deactivate-extension))

(define-method draw robot ()
  (draw%super self)
  (when (not %dead)
    (multiple-value-bind (x y) (step-toward-heading self %heading 20)
      (draw-circle x y 3 :color (random-choose '("cyan" "white")) :type :solid))))

(defresource 
    (:name "zap" :type :sample :file "zap.wav" :properties (:volume 30))
    (:name "paz" :type :sample :file "paz.wav" :properties (:volume 30))
    (:name "talk" :type :sample :file "talk.wav" :properties (:volume 20)))

;; give the player a few extra pixels of hitbox room
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
	  (push (new 'shield) shields)
	  (drop self (first shields))
	  (setf %shields shields))))))

(define-method update-shields robot ()
  (with-fields (shields heading) self
    (when shields
      (charge self 0.3)
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

(define-method throw-bomb robot ()
  (when (and %bomb-loaded 
	     (> %energy 40))
    (charge self 30)
    (play-sample "bombs-away")
    (drop self (new 'bomb %heading))
    (setf %bomb-loaded nil)))

(define-method load-bomb robot ()
  (setf %bomb-loaded t))

(define-method activate-extension robot ()
  (ecase %item
    (:shield (raise-shields self))
    (:bomb (throw-bomb self))))

(define-method deactivate-extension robot ()
  (ecase %item
    (:shield (lower-shields self))
    (:bomb (load-bomb self))))

(define-method equip robot (item)
  (setf %item item)
  (when %shields (lower-shields self)))

(define-method reset robot ()
  (xalcyon))

(define-method reload robot ()
  (setf %ready t))

(defun player-bullet (heading)
  (new 'bullet heading :speed 7 :tags '(:player)))

(define-method charge robot (amount)
  (assert (plusp amount))
  (setf %energy 
	(max 0 (decf %energy amount))))

(define-method recharge robot (amount)
  (assert (plusp amount))
  (setf %energy 
	(min 100 (incf %energy amount))))

(define-method fire robot (heading)
  (when (and %ready 
	     (not %dead) 
	     (> %energy 2))
    (charge self 1.0)
    (setf %ready nil)
    (later 8 (reload self))
    (play-sound self "zap")
    (drop self (player-bullet heading)
	  (/ %width 2) (/ %height 2))))

(define-method damage robot (points)
  (when (not %dead)
    (play-sound self (defresource :name "deathx" :type :sample :file "deathx.wav" :properties (:volume 100)))
    (setf %dead t)
    (let ((sign (new 'lose)))
      (drop self sign 32 32)
      (center sign))
    (play-music "nexttime")
    (percent-of-time 70
      (let ((message (random-choose '("vox-shield-warning" "vox-hazard" "vox-shutdown" "vox-you-lose"))))
	(later 1.4 (say self message))))
    (change-image self (defresource :name "skull" :type :image :file "skull.png"))))

(define-method win robot ())
  ;; (play-music "vixon")
  ;; (let ((sign (new 'win)))
  ;;   (drop self sign 32 32)
  ;;   (center sign)
  ;;   (later 5.0 (destroy sign))))

(define-method collide robot (thing)
  (cond
    ((has-tag thing :cloud)
     (damage self 1))
    ((is-enemy thing)
     (damage self 1))
    ((is-barrier thing)
     (damage self 1))
    ((and (is-brick thing))
       (restore-location self))))
     ;;   (setf %glide-heading (round %heading (/ pi 2))))
     ;; ;; possibly glide along wall
     ;; (incf %glide-heading
     ;; 	   (if (< %x (field-value :x thing))
     ;; 	       0.2 -0.2)))
    ;; (t (setf %glide-heading nil))))

(define-method aim robot (angle)
  (setf %heading angle))

(define-method drop-trail-maybe robot ()
  (decf %trail-timer)
  (when (zerop %trail-timer)
    (drop self (new 'trail) 6 6)
    (setf %trail-timer 2)))

(define-method auto-recharge robot ()
  ;; don't recharge while firing or using shield
  (unless (or (joystick-button-pressed-p :right-trigger)
	      (joystick-button-pressed-p :left-trigger))
    (with-fields (dead recharge-timer) self
      (when (zerop recharge-timer)
	(setf recharge-timer 8)
	(recharge self 3.1))
      (decf recharge-timer))))

(define-method update robot ()
  (when (not %dead)
    (when (plusp %speech-timer)
      (decf %speech-timer))
    (when (< %energy 20)
      (say self "vox-energy"))
    (auto-recharge self)
    (update-shields self)
    (when (left-analog-stick-pressed-p)
      (let ((heading (left-analog-stick-heading)))
	(aim self heading)
	;; possibly glide along wall
	(move self (or %glide-heading heading) 3.5)
;	(drop-trail-maybe self)
	))
    (if (right-analog-stick-pressed-p)
	(progn (aim self (right-analog-stick-heading))
	       (when (joystick-button-pressed-p :right-trigger)
		 (fire self (right-analog-stick-heading))))
	(when (joystick-button-pressed-p :right-trigger)
	  (fire self %heading)))))
	
;;; Stationary bases that generate enemies

(defresource (:name "base" :type :image :file "generator.png"))

(define-block base :image "base" :ready nil :timer 70 :tags '(:enemy) :hp 15)

(define-method update base ()
  (when (< (distance-to-player self) (level-value 280 340 400))
    (decf %timer)
    (when (zerop %timer)
      (setf %timer (level-value 100 100 130 150))
      (percent-of-time 8
	(percent-of-time 10 (play-sample "vox-radiation"))
	(dotimes (n 3)
	  (drop self (new 'glitch))))
      (drop self (new 'monitor)))))

(define-method damage base (points)
  (decf %hp)
  (make-sparks %x %y 1)
  (play-sound self (random-choose *whack-sounds*))
  (when (zerop %hp)
    (make-sparks (- %x 16) (- %y 16))
    (make-explosion self)
    (play-sound self "bigboom")
    (play-sound self "xplod")
    (play-sound self "vox-base-destroyed")
    (drop-chips self :value-multiplier 10 :count 8)
    (destroy self)))

;;; The reactor

(define-world reactor
  (level-clear :initform nil)
  (enemy-count :initform nil)
  (background-color :initform (theme-color :background)))

(defparameter *wall-thickness* 20)

(defun unit (&optional (units 1)) 
  (* units *wall-thickness*))

(define-method draw-wall reactor (length)
  (let (bricks)
    ;; a lazy approach.
    ;; lay down some bricks to measure
    (dotimes (n length)
      (let ((brick (new 'brick)))
	(drop self brick)
	(push brick bricks)
	(resize brick *wall-thickness* *wall-thickness*)
	(forward self (unit))))
    ;; now replace them with one brick
    (when bricks
      (multiple-value-bind (top left right bottom)
	  (find-bounding-box bricks)
	(mapc #'destroy bricks)
	(let ((big-brick (new 'brick)))
	  (drop self big-brick)
	  (move-to big-brick left top)
	  (resize big-brick (- right left) (- bottom top)))))))
  
(define-method draw-barrier reactor (length)
  (dotimes (n length)
    (let ((barrier (new 'barrier)))
      (drop self barrier)
      (resize barrier *wall-thickness* *wall-thickness*)
      (forward self (unit)))))

(define-method draw-square reactor (size)
  (dotimes (n 4)
    (draw-wall self size)
    (turn-right self)))

(define-method draw-base reactor (size0 &optional (bases 1))
  (let ((size (+ 6 size0)))
    (dotimes (n bases)
      (drop self (new 'base)
	    (unit (+ 2 (random (- size 2))))
	    (unit (+ 2 (random (- size 2))))))
    (let ((gap (+ 2 (random 2))))
      (dotimes (n 4)
	(draw-wall self (- size gap 2))
	(draw-barrier self gap)
	(draw-wall self 2)
	(draw-wall self 2)
	(turn-right self)))))

(define-method draw-bunker reactor (size0 &optional (monitors 5))
  (let ((size (+ 3 size0)))
    (let ((gap (+ 3 (random 2))))
      (dotimes (n 4)
	(draw-wall self (- size gap 2))
	(forward self (unit gap))
	(draw-wall self 2)
	(draw-wall self 2)
	(turn-right self))
      (dotimes (n monitors)
	(drop self (new 'monitor) 
	      (unit (+ 2 (random size0)))
	      (unit (+ 2 (random size0))))))))

(define-method draw-solid-room reactor (width height)
  (draw-wall self width)
  (turn-right self)
  (draw-wall self height)
  (turn-right self)
  (draw-wall self width)
  (turn-right self)
  (draw-wall self height)
  (turn-right self))

(defun wall-around (world)
  (with-fields (height width) world
    (let ((unit (unit)))
      (with-border 32
       (with-new-world 
	 (paste (world) world unit unit)
	 (draw-solid-room (world) 
			  (truncate (/ width unit))
			  (truncate (/ height unit))))))))

;;; Level generation and sequencing

(defparameter *level-types* '(:alpha :beta :gamma :delta :epsilon))

(defun level-value (&rest args)
  (if (<= (length args) *level*)
      (nth (1- (length args)) args)
      (nth *level* args)))

(defun generate-mission ()
  (let ((mission nil)
	(types *level-types*))
    (dotimes (difficulty (length *level-types*))
      (let ((type (random-choose types)))
	(setf types (remove type types))
	(push (list type difficulty) mission)))
    (reverse mission)))

(define-method build-theme reactor ()
  (setf *theme* (random-theme))
  (setf %background-color (theme-color :background))
  (setf %window-scrolling-speed 5)
  (move-window-to self 0 0))

(define-method build-alpha reactor ()
  (with-world-prototype self
    (with-new-world 
      (draw-solid-room (world) 32 32))))
		       
;; (define-method build-alpha reactor ()
;;   (with-world-prototype self
;;     (wall-around 
;;      (with-border (+ 160 (random 80))
;;       (stack-horizontally 
;;        (with-border (+ 30 (random 80))
;; 	(with-new-world (draw-bunker (world) (+ 2 (random 6)) 
;; 				     (level-value 1 1 2 5 6))))
;;        (with-border (+ 160 (random 90))
;; 	(with-new-world (draw-bunker (world) (+ 4 (random 6))
;; 				     (level-value 1 2 4 5 8)))))))))

;;; Adding a HUD to the view
	       	     
(define-method draw-overlays reactor ()
  ;; heads up display
  (when %player
    (multiple-value-bind (top left right bottom)
	(window-bounding-box self)
      (with-field-values (energy chips item) %player
	(with-field-values (enemy-count) self
	  (let* ((font *xalcyon-font*)
		 (line-height (font-height font))
		 (x (+ left (dash 5)))
		 (y (- bottom line-height (dash 2)))
		 (label (format nil "energy: ~3,2f  chip: ~d  item: ~a  enemy: ~d  |  press F1 for setup, CTRL-R to reset" 
				energy chips item enemy-count))
		 (bar-width 120))
	    ;; draw background
	    (draw-box left (- y 6) *gl-screen-width* (* 4 line-height) :color "black")
	    ;; draw energy bar 
	    (draw-box x y bar-width line-height :color "gray30")
	    (when (plusp energy)
	      (draw-box x y (* 1.2 energy) line-height 
			:color (cond 
				 ((>= energy 85) "chartreuse")
				 ((>= energy 70) "yellow")
				 ((>= energy 40) "orange")
				 ((>= 20 energy) "red")
				 (t "orange"))))
	    ;; show stats
	    (draw-string label 
			 (+ x bar-width (dash)) y 
			 :color "white"
			 :font *xalcyon-font*)))))))
  
(define-method update reactor ()
  (update%super self)
  (setf %background-color (theme-color :background))
  (unless %paused
    (when %player
      (unless %level-clear
	(let ((enemy-count 0))
	  (loop for object being the hash-keys in %objects do
	    (when (is-enemy object)
	      (incf enemy-count)))
	  (when (zerop enemy-count)
	    (setf %level-clear t)
	    (win %player))
	  (setf %enemy-count enemy-count))))))

(define-method add-player reactor ()
  (let ((player (new 'robot)))
    (set-player self player)
    (multiple-value-bind (x y) (center-point self)
      (add-object self player x y))))

(defvar *setup* nil)
(defvar *game* nil)

(define-method show-setup-screen reactor ()
  (start-alone *setup*))
    
;;; configuring joystick buttons

(define-block (button-chooser :super "BLOCKY:LIST")
  (orientation :initform :horizontal)
  (capturing :initform nil)
  (button-symbol :initform nil)
  (button-number :initform 0))

(define-method begin-capturing button-chooser (symbol)
  (message "Now capturing. Press the joystick button you wish to use for ~a" symbol)
  (setf %capturing t)
  (setf %button-symbol symbol))

(define-method draw button-chooser ()
  (mapc #'draw %inputs)
  (when %capturing
    (draw-focus (second %inputs))))

(define-method capture button-chooser (event)
  (when (and %capturing (is-raw-joystick-event event))
    (let ((button-number (second event)))
      (assert (integerp button-number))
      (setf %capturing nil)
      (grab-focus self) 
      ;; update gui and message output
      (message "Captured joystick button ~d as ~a." 
	       button-number %button-symbol)
      (set-value (second %inputs) button-number)
      (setf %button-number button-number))))

(define-method evaluate button-chooser ()
  (cons (evaluate (second %inputs)) %button-symbol))

(define-method initialize button-chooser (button-symbol &optional (button-number 0))
  (initialize%super self
		    (new 'action-button 
			 :label (format nil "capture button ~a now" button-symbol)
			 :arguments (list button-symbol)
			 :target self
			 :method :begin-capturing)
		    (new 'integer
			 :value button-number
			 :label "use button number"))
  (assert (keywordp button-symbol))
  (add-hook '*event-hook* #'(lambda (event) (capture self event)))
  (setf %button-symbol button-symbol)
  (freeze self))

;;; Joystick axis configurator

(defvar *flipper* nil)

(defparameter *axis-image-size* 60)

(defparameter *axis-bar-size* 100)

(define-block (axis-chooser :super "BLOCKY:LIST")
    (orientation :initform :horizontal)
    axis-name axis-number)

(define-method initialize axis-chooser (name number)
  (setf %axis-name name)
  (setf %axis-number number)
  (initialize%super self
		    (new 'symbol
			 :value name
			 :label "for this direction")
		    (new 'integer
			 :value number
			 :label "use axis number")
		    (new 'integer
			 :value 0
			 :label "(current value)"))
  (freeze self))

(define-method layout axis-chooser ()
  (layout-horizontally self))

(define-method draw axis-chooser ()
  (mapc #'draw %inputs))

(define-method update axis-chooser ()
  (set-value (third %inputs) (joystick-axis-raw-value (evaluate (second %inputs)))))

(define-method evaluate axis-chooser ()
  (evaluate (second %inputs)))

;;; The configurator dialog

(defparameter *xalcyon-saved-variables* 
  '(*user-joystick-profile* *joystick-dead-zone*))

(define-block (button-config :super :list))

(define-method initialize button-config ()
  (apply #'initialize%super 
	 self
	 (list
	  (new 'button-chooser :left-trigger)
	  (new 'button-chooser :right-trigger)
	  (new 'axis-chooser :left-stick-horizontal 0)
	  (new 'axis-chooser :left-stick-vertical 1)
	  (new 'axis-chooser :right-stick-horizontal 3)
	  (new 'axis-chooser :right-stick-vertical 2)
	  (new 'integer :value 6000 :label "joystick dead zone for values below")
	  (new 'action-button 
	       :label "save changes"
	       :method :save-changes
	       :target self)
	  (new 'messenger)
	  (new 'action-button 
	       :label "return to game"
	       :method :return-to-game
	       :target self)
	  ))
  (freeze self))

(define-method return-to-game button-config ()
  (show-game-screen *flipper*))

(define-method save-changes button-config ()
  (let* ((results (mapcar #'evaluate %inputs))
	 (buttons (list (first results)
			(second results)))
	 (left-stick (list (third results)
			   (fourth results)))
	 (right-stick (list (fifth results)
			    (sixth results))))
    (message "Saving joystick configuration for xalcyon...")
    (setf *user-joystick-profile* 
	  (list :buttons buttons
		:left-analog-stick left-stick
		:right-analog-stick right-stick))
    (setf *joystick-dead-zone* (seventh results))
    (blocky:save-variables *xalcyon-saved-variables*)
    (message "Saving joystick configuration for xalcyon... Done.")
    (message "Press ESCAPE or the 'return to game' button to continue playing.")))

;;; the game screen

(defparameter *xalcyon-copyright-notice*
  "Xalcyon (version 0.1a) is (C) Copyright 2006-2012 by David T. O'Toole
<dto@ioforms.org> http://dto.github.com/notebook/ 

This is free software. Program code is distributed under GNU GPLv3;
audiovisual materials are under Creative Commons license. See the
included file called `COPYING' for complete license information.
")

(define-world setup)

(define-method initialize setup ()
  (initialize%super self)
  (with-world self 
    (let ((box (new 'text (concatenate 'string *xalcyon-copyright-notice*))))
      (resize-to-scroll box 80 7)
      (end-of-line box)
      (add-block self (new 'list (new 'button-config) box) 90 30)
      (setf %background-color "gray20")
      (setf *message-history* nil)
      (message "Welcome to the joystick configuration screen for Xalcyon.")
      (message "Use your keyboard and mouse with the controls above.")
      (message "Please note, your controller must be plugged in before you start Xalcyon.")
      (message "Also, many controllers have an 'analog mode' button that must be toggled")
      (message "for the analog stick movements to register with the game.")
      (message "Press ESCAPE (or use the 'return to game' button) to resume playing."))))

(define-method show-game-screen setup ()
  (start-alone *game*))

;;; starting up the game.

(defun xalcyon ()
  (let ((robot (new 'robot))
	(reactor (new 'reactor)))
    ;; 	(setup (new 'setup)))
    ;; (setf *setup* setup)
    ;; (setf *game* reactor)
    (with-world reactor
      (bind-event reactor '(:r :control) :reset)
      ;; (bind-event reactor '(:f1) :show-setup-screen)
      ;; (bind-event setup '(:escape) :show-game-screen)
      (let ((letter (random-choose '(:alpha)))); :beta :gamma :delta :epsilon))))
	(build-theme reactor)
	(build-alpha reactor)
	(trim reactor)
	(add-object reactor robot 60 60)
	(set-player reactor robot)
	(start-alone reactor)))))

(define-method reset reactor ()
  (xalcyon))

;;; xalcyon.lisp ends here
