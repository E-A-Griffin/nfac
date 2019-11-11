(ns dfac.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Holds current ellipses in format:
;; {"x-y" {:name \"name\" :x x :y y :h 0-255 :s 0-255 :v 0-255}}
(def objects (atom {}))

;; Radius of ellipses
(def r 50)

(defn map-entry
  "Create a singular map-entry from a key (k) and a value (v)"
  [k v]
  (clojure.lang.MapEntry. k v))

(defn abs
  "Returns absolute value of number"
  [n] (if (neg? n) (- n) n))

(defn getx
  "Input: {\"x-y\" {:name \"name\" :x x :y y :h 0-255 :s 0-255 :v 0-255}}
   Output: x"
  [ellipse]
  (-> ellipse val :x))

(defn gety
  "Input: {\"x-y\" {:name \"name\" :x x :y y :h 0-255 :s 0-255 :v 0-255}}
   Output: y"
  [ellipse]
  (-> ellipse val :y))

(defn getname
  "Input: {\"x-y\" {:name \"name\" :x x :y y :h 0-255 :s 0-255 :v 0-255}}
  Output: \"name\""
  [ellipse]
  (-> ellipse val :name))

(defn selected?
  "Returns true if ellipse has been \"selected\", false otherwise."
  [ellipse] (-> ellipse val :selected true?))

(defn get-selected
  "Takes collection of ellipses in and returns first ellipse
  that returns true for \"(selected? ellipse)\" as a map-entry."
  [ellipses]
  (->> ellipses (some #(if (selected? %) %)) (apply map-entry)))

(defn capture-ellipse
  "Returns ellipse within radius of x and y if one exists
  nil otherwise."
  [ellipses x y]
  (loop [remaining (rest ellipses)
         current (first ellipses)]
    (when remaining
      (if (and (<= (abs (- (getx current) x)) 50)
               (<= (abs (- (gety current) y)) 50))
        (apply map-entry (assoc-in current [1 :selected] true))
        (recur (rest remaining) (first remaining))))))

(defn add->
  "Draw arrow from left to right at specified point and angle (radians)."
  [x y angle]
  (q/with-translation [x y]
    (q/with-rotation [angle]
      (q/line 0 0 20 20)
      (q/line 0 0 20 -20))))

(defn display-ellipse [ellipse]
  (let [x    (getx ellipse)
        y    (gety ellipse)
        name (getname ellipse)]
  ;; Generate color based on coordinates
  (q/fill (mod (+ x y) 255) (+ 100 (mod x 155)) 255)
  (q/ellipse x y r r)
  (q/fill 0)
  (q/rect-mode :center)
  (q/text-align :center)
  ;; 5 Used as arbitrary adjustment for text height
  (q/text name x (+ y 5) r (/ r 2))))

(defn display-ellipses
  "Draw all ellipses passed as arguments to screen."
  [ellipses]
  (loop [remaining (rest ellipses)
         current (first ellipses)]
    (if current
      (do (display-ellipse current)
          (recur (rest remaining) (first remaining))))))

(defn erase-char [x y] (q/with-stroke [0 0 255] (q/with-fill [0 0 255] (q/rect x y 5 5))))

(defn put-letter-transition
  "Prompts user for single character input, then prints above or below
  arrow to signify transition from one state to another on character.
  Returns character from input."
  [x y angle]
  (prn (q/raw-key))
  (q/rect-mode :center)
  (q/text-align :center)
  ;; Get-key & print
  (q/with-fill 0 0 0
    (q/with-translation [x (- y 10)]
      (q/with-rotation [(+ q/PI angle)]
        (q/text (str (q/raw-key)) 0 0))))
  ;; Allow user to confirm with enter or erase with backspace
  (when (= (str (q/raw-key)) "Backspace") (erase-char x y) (recur x y angle)))
  ;; Use background color text box to erase text if backspace pressed

(defn connect-ellipses
  "Connect two ellipses via a line with direction. b->a? specifies whether to
  connect from below to above (if true), a->b? specifies whether to connect
  from above to below (if true). In the event that the ellipses have the same
  y-values and varying x-values, b->a? determines whether the ellipses will
  be connected right->left and a->b? determines whether the ellipses will be
  connected left->right"
  ([from to f->t? t->f?]
   (let [xt             (getx to)
         yt             (gety to)
         xf             (getx from)
         yf             (gety from)
         from-greater?  (> xf xt)

         ;; Calculate slope if non-infinite, otherwise set slope to nil
         slope          (if (not (= xf xt)) (/ (- yf yt) (- xf xt)))
         angle          (if slope (q/atan slope) (* 1.5 q/PI))
         angle-offset   (cond
                          (> xf xt) 0
                          (= xf xt) (if (< yf yt) 0 q/PI)
                          (< xf xt) q/PI)

         mid-x          (/ (+ xt xf) 2)
         mid-y          (/ (+ yt yf) 2)
         one-fourth-x   (/ (+ mid-x (if from-greater? xt xf)) 2)
         one-fourth-y   (/ (+ mid-y (if from-greater? yt yf)) 2)
         three-fourth-x (/ (+ mid-x (if from-greater? xf xt)) 2)
         three-fourth-y (/ (+ mid-y (if from-greater? yf yt)) 2)
         f->t-x         (if from-greater? one-fourth-x three-fourth-x)
         f->t-y         (if from-greater? one-fourth-y three-fourth-y)
         t->f-x         (if from-greater? three-fourth-x one-fourth-x)
         t->f-y         (if from-greater? three-fourth-y one-fourth-y)]
     (q/line xf yf xt yt)

     ;; Rotate arrows +/-90 degrees if x values are the same
     (if (= xf xt)
       (do (when f->t?
             (add-> f->t-x f->t-y (+ angle-offset angle)))
           (when t->f?
             (add-> t->f-x t->f-y (+ (- angle-offset q/PI) angle))))
       (do (when f->t?
             (add-> f->t-x f->t-y (+ angle-offset angle))
             (put-letter-transition f->t-x f->t-y (+ q/PI angle)))
           (when t->f?
             (add-> t->f-x t->f-y (+ (- angle-offset q/PI) angle)))))

    (display-ellipses [from to])))
  ;; Connect ellipses both ways
  ([from to] (connect-ellipses from to true true)))

(defn setup []
  ;; Set background to white
  (q/background 255)
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ;;(connect-ellipses (first {"q" {:name "q" :x 150 :y 300}}) (first {"q235" {:name "q235" :x 50 :y 300}}))
  ;;(connect-ellipses (first {"q" {:name "q" :x 150 :y 300}}) (first {"q235" {:name "q235" :x 50 :y 400}}))
  ;;(connect-ellipses (first {"q" {:name "q" :x 600 :y 600}}) (first {"q235" {:name "q235" :x 600 :y 400}}))
  ;;(connect-ellipses (first {"q" {:name "q" :x 275 :y 500}}) (first {"q235" {:name "q235" :x 475 :y 350}}))
  ;; Failing
  ;;(connect-ellipses (first {"q" {:name "q" :x 575 :y 500}}) (first {"q235" {:name "q235" :x 475 :y 300}}))
  ;;(connect-ellipses (first {"q" {:name "q" :x 375 :y 600}}) (first {"q235" {:name "q235" :x 375 :y 750}}))
  ;;(connect-ellipses (first {"q" {:name "q" :x 300 :y 400}}) (first {"q235" {:name "q235" :x 375 :y 300}}))
  (q/stroke 358 100 100)
  {:color 0
   :angle 0
   :mode :create})

(defn update-state [state]
  ;; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :mode (:mode state)})

(defn draw-state [state]
  ;; Clear the sketch by filling it with light-grey color.
  ;; Set circle color.
  (q/fill (:color state) 255 255)
  ;; Calculate x and y coordinates of the circle.
  (let [color (:color state)
        x (q/mouse-x)
        y (q/mouse-y)]

    (case (:mode state)
      ;; If mouse-pressed & in create mode, create new state
      :create (when-let
                    ;; Format: Single-entry map of the form
                    ;;         k: "x-y"
                    ;;         v: {:name "name" :x x :y y :h 0-255 :s 0-255 :v 0-255}
                  [ellipse (if (q/mouse-pressed?)
                                (array-map (str \q (count @objects))
                                 {:name (str \q (count @objects))
                                  :x x :y y
                                  :h color :s 255 :v 255}))]

                  ;; Prevent click from being processed as multiple clicks which
                  ;; would create multiple states for a single click
                  (Thread/sleep 50)
                  (display-ellipse (first ellipse))
                  (swap! objects conj ellipse))
      ;; If ellipse clicked & in transition mode, mark ellipse as "selected" and
      ;; switch to connect mode
      :transition (when-let
                      ;; Format: 2-entry vector
                      ;; ["name" {:x x :y y :h 0-255 :s 0-255 :v 0-255}]
                      [captured
                       (and (q/mouse-pressed?) (capture-ellipse @objects x y))]
                    (swap! objects assoc (first captured) (second captured))

                    ;; First state selected, switch mode so that next
                    ;; state selected will initiate connecting states via
                    ;; transition function.
                    (swap! (q/state-atom) assoc-in [:mode] :connect)

                    ;; Prevent single click from being processed as duplicate clicks
                    (Thread/sleep 50))

      ;; If first ellipse selected previously, new ellipse clicked, & in
      ;; connect mode, connect ellipses with arrow
      :connect (when-let
                   ;; Format: 2-entry vector
                   ;; ["name" {:x x :y y :h 0-255 :s 0-255 :v 0-255}]
                   [to (if (q/mouse-pressed?) (capture-ellipse @objects x y))]
                 (let [from (get-selected @objects)]
                   (when-not (= from to)
                     (connect-ellipses from to true false)
                     ;; Strip :selected from to in @objects
                     (swap! objects assoc (key from) (dissoc (val from) :selected))
                     (swap! (q/state-atom) assoc-in [:mode] :poll)
                     (Thread/sleep 50)))))
      ;;:poll (if (q/key-pressed?)
              ;;(connect-ellipses from to true false)))

    (if (q/key-pressed?)
      (case (q/raw-key)
        \t (swap! (q/state-atom) assoc-in [:mode] :transition)
        \c (swap! (q/state-atom) assoc-in [:mode] :create)))))


(q/defsketch quil-test
  :title "Deterministic Finite Automaton Creator"
  :size [800 800]
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
