(ns dfac.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Holds current ellipses in format:
;; {"x-y" {:name "name" :x x :y y :h 0-255 :s 0-255 :v 0-255
;;         :out {"out-state1" '(\a \b \c)
;;               "out-state2" '(\d \e \f)}
;;         :in  {"in-state1"  '(\a \b)
;;               "in-state2"  '(\c)}}}
;;
;; :x and :y represent cartesian coordinate values
;;
;; :h, :s, and :v are the numeric values to represent the color
;; of the ellipse/state in hsv-mode
;;
;; :out contains a map of strings for keys, which represent the names
;; of ellipses/states the given ellipse/state has a transition function to
;; and lists of characters representing characters to traverse the given
;; function on
;;
;; :in contains a map of strings for keys, which represent the names
;; of ellipses/states the given ellipse/state has a receiving (inward)
;; transition function for and lists of characters representing characters ;; to traverse the given function on
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

(defn pos-exp
  "Takes positive exponent b^p"
  [b p]
  (if (and (integer? b) (integer? p) (>= p 0))
    (reduce * (repeat p b))))

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

(defn from?
  "Returns true if ellipse was selected first, during transition mode.
  Useful for returning the first member of a pair of ellipses/states to be
  joined via a transition function."
  [ellipse] (-> ellipse val :from true?))

(defn to?
  "Returns true if ellipse was selected second, during connect mode.
  Useful for returning the second member of a pair of ellipses/states to be
  joined via a transition function."
  [ellipse] (-> ellipse val :to true?))

(defn get-selected
  "Takes collection of ellipses in and returns first ellipse
  that returns true for \"(selected? ellipse)\" as a map-entry."
  [ellipses]
  (->> ellipses (some #(if (selected? %) %)) (apply map-entry)))

(defn get-from
  "Returns first ellipse from collection that returns true
  for (from? ellipse). Returns ellipse that was selected first,
  during transition mode."
  [ellipses]
  (->> ellipses (some #(if (from? %) %)) (apply map-entry)))

(defn get-to
  "Returns first ellipse from collection that returns true
  for (to? ellipse). Returns ellipse that was selected second,
  during transition mode."
  [ellipses]
  (->> ellipses (some #(if (to? %) %)) (apply map-entry)))

(defn capture-ellipse
  "Returns ellipse within radius of x and y if one exists
  nil otherwise."
  [ellipses x y]
  (loop [remaining (rest ellipses)
         current (first ellipses)]
    (when remaining
      (if (and (<= (abs (- (getx current) x)) 50)
               (<= (abs (- (gety current) y)) 50))
        current
        (recur (rest remaining) (first remaining))))))

(defn add->
  "Draw arrow from left to right at specified point and angle (radians)."
  [x y angle]
  (q/with-translation [x y]
    (q/with-rotation [angle]
      (q/line 0 0 20 20)
      (q/line 0 0 20 -20))))

(defn display-ellipse
  "Draw ellipse passed as argument to screen"
  [ellipse]
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

(defn erase-char
  "Unfinished/needs testing: erases character at [x y] by
  drawing a white box around character"
  [x y] (q/with-stroke [0 0 255] (q/with-fill [0 0 255] (q/rect x y 5 5))))

(defn put-letter-transition
  "Prompts user for single character input, then prints above or below
  arrow to signify transition from one state to another on character.
  Returns character from input."
  [x y ch angle disp]
  (q/rect-mode :center)
  (q/text-align :center)
  ;; Get-key & print
  (q/with-fill 0 0 0
    (q/with-translation [x y]
      (q/with-rotation [(+ q/PI angle)]
        (q/text (str ch) 0 (- 0 disp))))))

(defn connect-ellipses
  "Connect two ellipses via a line with direction. f->t? specifies whether to
  connect from 'from' to 'to' (if true), a->b? specifies whether to connect
  from 'to' to 'from' (if true). Draws current value of (q/raw-key) above
  transition function arrow. Returns current value of (q/raw-key)"
  ([from to f->t? t->f?]
   (let [xt             (getx to)
         yt             (gety to)
         xf             (getx from)
         yf             (gety from)
         from-greater?  (> xf xt)

         transition-ch  (q/raw-key)
         ;; Weight to displace transition-ch
         ;; when printing above
         transition-w   (inc (count (-> from val :out (get (key to)))))

         ;; Character weight displacement/offset from arrow
         displacement   (* 15 transition-w)

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
             (add-> f->t-x f->t-y (+ angle-offset angle))
             (put-letter-transition f->t-x f->t-y transition-ch (+ q/PI angle) displacement))
           (when t->f?
             (add-> t->f-x t->f-y (+ (- angle-offset q/PI) angle))
             (put-letter-transition t->f-x t->f-y transition-ch (+ q/PI angle) displacement)))
       (do (when f->t?
             (add-> f->t-x f->t-y (+ angle-offset angle))
             (put-letter-transition f->t-x f->t-y transition-ch (+ q/PI angle) displacement))
           (when t->f?
             (add-> t->f-x t->f-y (+ (- angle-offset q/PI) angle))
             (put-letter-transition t->f-x t->f-y transition-ch (+ q/PI angle) displacement))))

     (display-ellipses [from to])
     transition-ch))
  ;; Connect ellipses 'from'->'to'
  ([from to] (connect-ellipses from to true false)))

(defn setup []
  ;; Set background to white
  (q/background 255)
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/stroke 358 100 100)
  {:color 0
   :angle 0
   :mode :create})

(defn update-state [state]
  ;; Update sketch state by changing color
  {:color (mod (+ (:color state) 0.7) 255)
   :mode (:mode state)})

(defn draw-state [state]
  ;; Set circle color.
  (q/fill (:color state) 255 255)
  ;; Calculate x and y coordinates of the circle.
  (let [color (:color state)
        x (q/mouse-x)
        y (q/mouse-y)]

    (case (:mode state)
      ;; If mouse-pressed & in create mode, create new state
      :create (when-let
                  ;; Format: Map-Entry of the form
                  ;;
                  ;; k: "x-y"
                  ;; v: {:name "name" :x x :y y :h 0-255 :s 0-255 :v 0-255}
                  [ellipse (if (q/mouse-pressed?)
                                (map-entry (str \q (count @objects))
                                 {:name (str \q (count @objects))
                                  :x x :y y
                                  :h color :s 255 :v 255}))]

                  ;; Prevent click from being processed as multiple clicks
                  ;; which would create multiple states/ellipses for a
                  ;; single click
                  (Thread/sleep 50)
                  (display-ellipse ellipse)
                  (swap! objects conj ellipse))
      ;; If ellipse clicked & in transition mode, mark ellipse as "from"
      ;; and switch to connect mode
      :transition (when-let
                      [captured
                       (and (q/mouse-pressed?) (capture-ellipse @objects x y))]
                    (swap! objects assoc (key captured) (assoc (val captured) :from true))

                    ;; First/'from' state selected, switch mode so that
                    ;; next/'to' state may be selected
                    (swap! (q/state-atom) assoc-in [:mode] :connect)

                    ;; Prevent single click from being processed as
                    ;; multiple clicks which would create multiple
                    ;; states/elipses for a single click
                    (Thread/sleep 50))

      ;; If first ellipse selected previously, new ellipse clicked, & in
      ;; connect mode, set respective ellipses to 'from' and 'to' and move
      ;; to poll mode while waiting for the user to input a character for
      ;; the transition function.
      :connect (when-let
                   [to (if (q/mouse-pressed?) (capture-ellipse @objects x y))]
                 (let [from (get-from @objects)]
                   (when-not (= from to)
                     (swap! objects assoc (key to) (assoc (val to) :to true))
                     (swap! (q/state-atom) assoc-in [:mode] :poll)
                     (Thread/sleep 50))))
      ;; Once user enters a character for the transition function. Create
      ;; transition function between 'from' selected in transition mode and
      ;; 'to' selected in connect mode.
      :poll (when (q/key-pressed?)
              ;; Create transition function
              (let [from (get-from @objects)
                    to   (get-to @objects)
                    transition-ch (connect-ellipses from to)]
                ;; Strip 'from' and 'to' of their respective labels
                ;; indicating they're ready to be connected and update
                ;; 'from' and 'to' so that their new transition function
                ;; is listed in each's :out/:in map.
                (swap! objects assoc (key from) (update-in (dissoc (val from) :from) [:out (getname to)] conj transition-ch))
                (swap! objects assoc (key to) (dissoc (val to) :to))

                ;; Return to create mode
                (swap! (q/state-atom) assoc-in [:mode] :create))))

    (if (q/key-pressed?)
      (case (q/raw-key)
        \t (swap! (q/state-atom) assoc-in [:mode] :transition)
        \c (swap! (q/state-atom) assoc-in [:mode] :create)

        ;; Test cases for connecting different angles between states
        \~ (do (connect-ellipses (first {"q0" {:name "q0" :x 150 :y 300}})
                                 (first {"q1" {:name "q1" :x 50 :y 300}}))
               (connect-ellipses (first {"q2" {:name "q2" :x 150 :y 300}})
                                 (first {"q3" {:name "q3" :x 50 :y 400}}))
               (connect-ellipses (first {"q4" {:name "q4" :x 600 :y 600}})
                                 (first {"q5" {:name "q5" :x 600 :y 400}}))
               (connect-ellipses (first {"q6" {:name "q6" :x 275 :y 500}})
                                 (first {"q7" {:name "q7" :x 475 :y 350}}))
               (connect-ellipses (first {"q8" {:name "q8" :x 575 :y 500}})
                                 (first {"q9" {:name "q9" :x 475 :y 300}}))
               (connect-ellipses (first {"q10" {:name "q10" :x 375 :y 600}})
                                 (first {"q11" {:name "q11" :x 375 :y 750}}))
               (connect-ellipses (first {"q12" {:name "q12" :x 300 :y 400}})
                                 (first {"q13" {:name "q13" :x 375 :y 300}})))
        nil))))


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
