(ns dfac.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


; Holds current ellipses in format:
; #{{x y} {:h 0-255 :s 0-255 :v 0-255 :object "name-of-shape"}}
; hash-map used so that no two objects may be stored with the same
; x-y coordinates
(def objects (atom (hash-map)))

(def test-arcs (hash-map))

(def test-ellipses (hash-map {10 20} {:h 0 :g 255 :v 255 :object "ellipse"}))

(defn getx
  "Input: {x y} {:h 0-255 :g 0-255 :v 0-255 :object \"name of object\"}
   Output: x"
  [ellipse]
  (-> ellipse key first first))

(defn gety
  "Input: {x y} {:h 0-255 :g 0-255 :v 0-255 :object \"name of object\"}
   Output: y"
  [ellipse]
  (-> ellipse key first second))

(defn connect-ellipses
  "Connect from->to. Order matters"
  [from to]
  (q/fill 0 0 255)
  (q/arc (/ (+ (getx from) (getx to)) 2) (- (/ (+ (gety to) (gety from)) 2) 27) (- (getx to) (getx from)) 100 q/PI q/TWO-PI)
  (q/arc (/ (+ (getx from) (getx to)) 2) (+ (/ (+ (gety to) (gety from)) 2) 27) (- (getx to) (getx from)) 100 0 q/PI)
  )

(defn display-ellipse [ellipse]
  (q/ellipse (getx ellipse) (gety ellipse) 50 50))

(defn display-ellipses [ellipses]
  (loop [remaining (rest ellipses)
         current (first ellipses)]
    (if current
      (do (display-ellipse current)
          (recur (rest remaining) (first remaining))))))

(defn setup []
  ; Set background to white
  (q/background 255)
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/fill 28 255 255)
  (q/ellipse 300 500 50 50)
  (q/ellipse 900 500 50 50)
  (q/fill 0 0 255)
  (q/stroke 358 255 255)
  (q/arc 50 50 100 10 q/PI q/TWO-PI)
  (connect-ellipses (first {{300 500} {}}) (first {{900 500} {}}))
  (q/stroke 358 100 100)
  (q/line 87 35 102 50)
  (q/line 87 65 102 50)
  (q/ellipse 50 50 100 50)
  ; Setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :objects (atom #{})})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ;(q/background 255)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (if (q/key-pressed?) (do (q/background 255) (display-ellipses @objects)))
  (let [angle (:angle state)
        color (:color state)
        x (q/mouse-x)
        y (q/mouse-y)]
    (if (q/mouse-pressed?) (do (q/ellipse x y 50 50)
                               (swap! objects conj {{x, y} {:h color :s 255 :v 255 :object "ellipse"}})))))


(q/defsketch quil-test
  :title "You spin my circle right round"
  :size [1000 1000]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
