;; Primary driver for nfac. Contains main sketch which allows the user to
;; create NFA and makes calls to handle user input from mouse and keyboard.
(ns nfac.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [nfac.objects :refer [objects]]
            [nfac.front-to-back :as f->b]
            [nfac.load :as ld]
            [nfac.save :as sv]
            [nfac.test-string :as t-str]
            [nfac.nfa-spec]
            ;; Back-end
            [automata.nfa :as b]))

;; Holds current states in format:
;; {"x-y" {:name "name" :x x :y y
;;         :out {"out-state1" '(\a \b \c)
;;               "out-state2" '(\d \e \f)}
;;         :in  {"in-state1"  '(\a \b)
;;               "in-state2"  '(\c)}}}
;;
;; :x and :y represent cartesian coordinate values
;;
;; :out contains a map of strings for keys, which represent the names
;; of ellipses/states the given ellipse/state has a transition function to
;; and lists of characters representing characters to traverse the given
;; function on
;;
;; :in contains a map of strings for keys, which represent the names
;; of ellipses/states the given ellipse/state has a receiving (inward)
;; transition function for and lists of characters representing characters ;; to traverse the given function on

;; Radius of ellipses
(def r 50)

(def color-map
  {:dark-mode {:background 50
               :stroke-size 2
               :stroke-h 255
               :stroke-s 0
               :stroke-v 255}
   :light-mode {:background 255
                :stroke-size 1
                :stroke-h 255
                :stroke-s 0
                :stroke-v 0}})

(def shift-code 38)

(declare display-nfa)


(defn redraw-sketch!
  "Redraw current sketch to reflect changes"
  []
  (q/background ((color-map (q/state :theme)) :background))
  (display-nfa @objects)
  (println "redrawing"))

(defn set-theme!
  "Set theme based on current mode"
  [color-map mode]
  ;; Don't perform redundant work
  (when (not= mode (q/state :theme))
    (let [mode-map (color-map mode)]
      (q/background (mode-map :background))
      (q/stroke-weight (mode-map :stroke-size))
      (q/stroke (mode-map :stroke-h)
                (mode-map :stroke-s)
                (mode-map :stroke-v))
      (swap! (q/state-atom) assoc-in [:theme] mode)
      (redraw-sketch!))))

(defn map-entry
  "Create a singular map-entry from a key (k) and a value (v)"
  [k v]
  (clojure.lang.MapEntry. k v))

(defn getx
  "Input: {\"x-y\" {:name \"name\" :x x :y y}}
   Output: x"
  [state]
  (-> state val :x))

(defn gety
  "Input: {\"x-y\" {:name \"name\" :x x :y y}}
   Output: y"
  [state]
  (-> state val :y))

(defn getname
  "Input: {\"x-y\" {:name \"name\" :x x :y y}}
  Output: \"name\""
  [state]
  (-> state val :name))

(defn selected?
  "Returns true if state has been \"selected\", false otherwise."
  [state] (-> state val :selected true?))

(defn from?
  "Returns true if state was selected first, during transition mode.
  Useful for returning the first member of a pair of states/states to be
  joined via a transition function."
  [state] (-> state val :from true?))

(defn to?
  "Returns true if state was selected second, during connect mode.
  Useful for returning the second member of a pair of ellipses/states to be
  joined via a transition function."
  [state] (-> state val :to true?))

(defn final?
  "Returns true if state selected represents a final state."
  [state] (-> state val :final? true?))

(defn transition-exists?
  "Returns whether or not transition already exists. Useful for
  preventing user from creating duplicate transition functions."
  [from to transition-ch]
  (->> (-> from val :out (get (getname to)))
           (some #(when (= % transition-ch) %)) char?))

(defn get-selected
  "Takes collection of states in and returns first state
  that returns true for \"(selected? state)\" as a map-entry."
  [states]
  (->> states (some #(when (selected? %) %)) (apply map-entry)))

(defn get-from
  "Returns first state from collection that returns true
  for (from? state). Returns state that was selected first,
  during transition mode."
  [states]
  (->> states (some #(when (from? %) %)) (apply map-entry)))

(defn get-to
  "Returns first state from collection that returns true
  for (to? state). Returns state that was selected second,
  during transition mode."
  [states]
  (->> states (some #(when (to? %) %)) (apply map-entry)))

(defn get-transition-count
  "Takes in two states e1 & e2 and returns the count for how
  many transition functions exist from e1->e2."
  [e1 e2]
  (-> e1 val :out (get (getname e2)) count))

(defn get-disp
  "Takes in two states e1 & e2 and returns the displacement for how
  far to display next character to represent transition function from
  default placement location."
  [e1 e2]
  (-> (get-transition-count e1 e2) inc (* 15) ))

(defn draw-start-state-sym
  "Draw triangle indicating that ellipse/state is the start of NFA."
  [state]
  (let [x (- (getx state) (/ r 2))
        y (gety state)]
    (q/no-fill)
    (q/triangle
     (- x (/ r 2)) (- y (/ r 2)) (- x (/ r 2)) (+ y (/ r 2)) x y)))

(defn draw-final-state-sym
  "Draw smaller ellipse inside ellipse to represent final state."
  [state]
  (q/no-fill)
  (q/ellipse (+ (getx state) 0.5) (+ (gety state) 0.5) (* r 0.85) (* r 0.85)))

(defn purge-transitions
  [states delete]
  (let [del-k (key delete)]
    (loop [[k v] (update-in
                  (update-in (first states) [1 :out]
                             dissoc del-k) [1 :in] dissoc del-k)
           remaining (rest states)
           new-map   {}]
      (if (empty? remaining)
        (assoc new-map k v)
        (recur (update-in
                (update-in (first remaining) [1 :out]
                           dissoc del-k) [1 :in] dissoc del-k)
               (rest remaining) (assoc new-map k v))))))

(defn nested-rename-key
  "Replace old-k with new-k inside the values (maps) of :in and :out
  for each state."
  [m old-k new-k]
  (into {}
        (map
         #(assoc-in (assoc-in % [1 :out]
                              (-> % val :out (set/rename-keys {old-k new-k})))
                    [1 :in] (-> % val :out (set/rename-keys {old-k new-k})))
         m)))

(defn capture-state
  "Returns state within radius of x and y if one exists
  nil otherwise."
  [states x y]
  (loop [remaining (rest states)
         current (first states)]
    (when remaining
      (if (and (<= (q/abs (- (getx current) x)) 50)
               (<= (q/abs (- (gety current) y)) 50))
        current
        (recur (rest remaining) (first remaining))))))

(defn add->
  "Draw arrow from left to right at specified point and angle (radians)."
  [x y angle]
  (q/with-translation [x y]
    (q/with-rotation [angle]
      (q/line 0 0 20 20)
      (q/line 0 0 20 -20))))

(defn add-loop
  "Draw loop connecting an ellipse/state to itself."
  [x y]
  (q/no-fill)
  (q/with-translation [x (- y (* r 0.7))]
    (q/arc 0 0 (* r 0.7) (* r 0.8) (* -5 q/QUARTER-PI) q/QUARTER-PI)
    (add-> -15 13 (* 2.8 q/HALF-PI))))

(defn display-state
  "Draw state passed as argument to screen"
  ([state & {:keys [saturation spec?]
             :or   {saturation (+ 100 (mod (getx state) 130))
                    spec?      true}}]
   (when spec? (spec/valid? :nfac.nfa-spec/nfa state))
   (let [x    (getx state)
         y    (gety state)
         name (getname state)]
     ;; Generate hue based on coordinates
     (q/fill (mod (+ x y) 255) saturation 255)
     (q/ellipse x y r r)
     (q/no-fill)
     (when (final? state) (q/ellipse (+ x 0.5) (+ y 0.5) (* r 0.8) (* r 0.8)))
     (q/fill 0)
     (q/rect-mode :center)
     (q/text-align :center)
     ;; 5 Used as arbitrary adjustment for text height
     (q/text name x (+ y 5) r (/ r 2)))))

(defn display-states
  "Draw all ellipses passed as arguments to screen."
  [states]
  ;; Check all states once rather than calling spec/valid?
  ;; each time display-state is invoked
  (spec/valid? :nfac.nfa-spec/nfa states)
  (loop [remaining (rest states)
         current (first states)]
    (when current
      (display-state current :spec? false)
      (recur (rest remaining) (first remaining)))))

(defn put-letter-transition
  "Prints user entered character above arrow to signify transition from one
   state to another on character. Returns character from input."
  [x y ch angle disp]
  (q/rect-mode :center)
  (q/text-align :center)
  ;; Get-key & print
  (let [mode-map (color-map (q/state :theme))]
   (q/fill
    (mode-map :stroke-h)
    (mode-map :stroke-s)
    (mode-map :stroke-v)))
    (q/with-translation [x y]
      (q/with-rotation [angle]
        (q/text (str ch) 0 (- 0 disp)))))

(defn connect-states
  "Connect two states via a line with direction. f->t? specifies whether to
  connect from 'from' to 'to' (if true), a->b? specifies whether to connect
  from 'to' to 'from' (if true). Draws current value of transition-ch above
  transition function arrow. Returns current value of transition-ch.
  Re-draw? is true when an NFA is intended to be re-drawn after having
  been erased previously. This parameter is necessary to ensure that
  no duplicate transition characters are printed."
  ([from to f->t? t->f? transition-ch displacement]
   (let [xt             (getx to)
         yt             (gety to)
         xf             (getx from)
         yf             (gety from)
         from-greater?  (> xf xt)

         ;; Calculate slope if non-infinite, otherwise set slope to nil.
         ;; When slope is nil, set angle to 3pi/2
         slope          (when (not (= xf xt)) (/ (- yf yt) (- xf xt)))
         angle          (if slope (q/atan slope) (* 1.5 q/PI))
         angle-offset   (cond
                          (> xf xt) 0
                          (= xf xt) (if (< yf yt) 0 q/PI)
                          (< xf xt) q/PI)

         ;; Position-based values
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

     (do (when f->t?
           (add-> f->t-x f->t-y (+ angle-offset angle))
           (put-letter-transition f->t-x f->t-y transition-ch angle
                                  displacement))
         (when t->f?
           (add-> t->f-x t->f-y (+ (- angle-offset q/PI) angle))
           (put-letter-transition t->f-x t->f-y transition-ch angle
                                  displacement)))

     (display-states [from to])
     transition-ch))
  ([from to transition-ch displacement]
   ;; Case: connect state to itself to form a loop.
   (if (= from to)
     (do (add-loop (getx from) (gety from))
         (display-state from)
         (put-letter-transition (getx from) (- (gety from) r) transition-ch
                                0 displacement)
         transition-ch)
     (connect-states from to true false transition-ch displacement)))
  ;; Connect states 'from'->'to'. Two arg forms checks for and handles
  ;; loops as well. Call 4-arity version of connect-states with current value of q/raw-key
  ;; (or \λ if enter is pressed) and displacement between transition function and placement
  ;; of character (based on existence of other transition functions between the states).
  ([from to]
   (let [raw-key (q/raw-key)]
     (connect-states from to
                     (if (= raw-key \newline) \λ raw-key)
                     (get-disp from to)))))

(defn display-nfa
  "Draw transitions between states/ellipses based on values of :out
  inside each state. If state has no transition functions, display
  state alone. Note, flips order of transition characters on each draw.
  Since order doesn't matter, this is left alone."
  [states]
  ;; Check that states are valid
  (spec/explain :nfac.nfa-spec/nfa states)
  ;; Draw triangle indicating which ellipse represents initial state
  (when-let [start-key (f->b/get-start-node states)]
    (draw-start-state-sym (map-entry start-key (get states start-key))))
  (doseq [from states]
    (if-not (empty? (-> from val :out))
      ;; If state contains any transition functions, draw-them
      (doseq [to (-> from val :out)]
        (let [chars (val to)]
          (doseq [transition-ch (val to)]
              (let [to-state (map-entry (key to) (get @objects (key to)))
                    displacement (* 15 (- (count chars)
                                          (.indexOf chars transition-ch)))]
                (connect-states
                 from to-state transition-ch displacement)))))
      ;; Else draw state alone
      (display-state from :spec? false))))


(defn create-logo
  "Used to recreate states to represent NFAC logo."
  []
  (let [n (map-entry "n" {:name "n" :x 95  :y 300})
        f (map-entry "f" {:name "f" :x 195 :y 350})
        a (map-entry "a" {:name "a" :x 295 :y 300})
        c (map-entry "c" {:name "c" :x 395 :y 350})]
    (draw-start-state-sym n)
    (swap! objects conj n)
    (display-state n)
    (swap! objects conj f)
    (display-state f)
    (swap! objects conj a)
    (display-state a)
    (swap! objects conj c)
    (display-state c)))

(defn check-for-keys []
  (prn "raw-key: " (q/raw-key))
  (prn "key-code: " (q/key-code))
  (prn "key-coded: " (q/key-coded? (q/raw-key)))
  (case (q/raw-key)
    ;; Swap state mode based on user input
    \t (swap! (q/state-atom) assoc-in [:mode] :transition)
    \c (swap! (q/state-atom) assoc-in [:mode] :create)
    \f (swap! (q/state-atom) assoc-in [:mode] :set-final)
    \0 (swap! (q/state-atom) assoc-in [:mode] :set-start)
    \d (swap! (q/state-atom) assoc-in [:mode] :delete)
    ;; Toggle between dark-mode ("n"ight-mode) and light-mode ("b"right-mode)
    \n (set-theme! color-map :dark-mode)
    \b (set-theme! color-map :light-mode)

    ;; Redraw sketch based on objects
    \newline (redraw-sketch!)

    ;; Save nfa to file
    \s (apply q/sketch sv/save-ske)
    ;; Load nfa from file
    \l (apply q/sketch ld/load-ske)

    \z (create-logo)

    ;; Enter test string
    \a (do (b/load-state (f->b/all-nodes-front->back @objects))
                         (apply q/sketch t-str/test-str-ske))

    ;; Exit sketch
    \e (q/exit)

    ;; Test cases for connecting different angles between states
    \~ (do (connect-states (first {"q0" {:name "q0" :x 150 :y 300}})
                           (first {"q1" {:name "q1" :x 50 :y 300}}))
           (add-loop 150 300)
           (connect-states (first {"q2" {:name "q2" :x 150 :y 300}})
                           (first {"q3" {:name "q3" :x 150 :y 400}}))
           (connect-states (first {"q4" {:name "q4" :x 600 :y 600}})
                           (first {"q5" {:name "q5" :x 600 :y 400}}))
           (connect-states (first {"q6" {:name "q6" :x 275 :y 500}})
                           (first {"q7" {:name "q7" :x 475 :y 350}}))
           (connect-states (first {"q8" {:name "q8" :x 575 :y 500}})
                           (first {"q9" {:name "q9" :x 475 :y 300}}))
           (connect-states (first {"q10" {:name "q10" :x 375 :y 600}})
                           (first {"q11" {:name "q11" :x 375 :y 750}}))
           (connect-states (first {"q12" {:name "q12" :x 300 :y 400}})
                           (first {"q13" {:name "q13" :x 375 :y 300}})))
    \[ (do (q/no-fill) (q/ellipse 365.5 450.5 (* r 0.85) (* r 0.85)))
    nil))

(defn setup []
  ;; Set background to white
  (q/background 255)
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/stroke 358 100 100)
  (reset! objects {})
  {:color 0
   :angle 0
   :mode :create
   ;; Default to light-mode
   :theme :light-mode})

(defn update-state [state]
  ;; Update sketch state by changing color
  {:color (mod (+ (state :color) 0.7) 255)
   :mode (state :mode)
   :theme (state :theme)})

(defn draw-state [state]
  ;; Set circle color.
  (q/fill (:color state) 255 255)
  ;; Calculate x and y coordinates of the circle.
  (let [x (q/mouse-x)
        y (q/mouse-y)]

    ;; Check for user key input and perform actions on sketch, state, and
    ;; objects based on input
    (when (and (q/key-pressed?) (not= (state :mode) :poll)) (check-for-keys))

    ;; TODO: check if still in use
    (when (get @objects :reload)
      (q/background ((color-map (q/state :theme)) :background))
      (swap! objects dissoc :reload)
      (display-nfa @objects))

    (case (:mode state)
      ;; If mouse-pressed & in create mode, create new state
      :create (when-let
                  ;; Format: Map-Entry of the form
                  ;;
                  ;; k: "name"
                  ;; v: {:name "name" :x x :y y}
                  [ellipse (when (q/mouse-pressed?)
                                   (map-entry (str \q (count @objects))
                                              {:name (str \q
                                                          (count @objects))
                                               :x x :y y}))]

                  ;; Prevent click from being processed as multiple clicks
                  ;; which would create multiple states/ellipses for a
                  ;; single click
                  (when (empty? @objects) (draw-start-state-sym ellipse))
                  (if (empty? @objects)
                    (swap! objects conj (assoc-in ellipse [1 :start?] true))
                    (swap! objects conj ellipse))
                  (prn @objects)
                  (display-state ellipse))
      ;; If state clicked & in transition mode, mark state as "from"
      ;; and switch to connect mode
      :transition (when-let
                      [captured
                       (and (q/mouse-pressed?) (capture-state @objects x y))]
                    (swap! objects assoc (key captured)
                           (assoc (val captured) :from true))

                    (display-state captured :saturation 70)

                    ;; First/'from' state selected, switch mode so that
                    ;; next/'to' state may be selected
                    (swap! (q/state-atom) assoc-in [:mode] :connect))


      ;; If first state selected previously, new state clicked, & in
      ;; connect mode, set respective states to 'from' and 'to' and move
      ;; to poll mode while waiting for the user to input a character for
      ;; the transition function.
      :connect (when-let
                   [to (when (q/mouse-pressed?)
                         (capture-state @objects x y))]

                 (let [from (get-from @objects)]
                   (display-state to :saturation 80)
                   (swap! objects assoc (key to) (assoc (val to) :to true))
                   (swap! (q/state-atom) assoc-in [:mode] :poll)))
      ;; Once user enters a character for the transition function. Create
      ;; transition function between 'from' selected in transition mode and
      ;; 'to' selected in connect mode.
      :poll (when (and (q/key-pressed?)
                       (not (q/key-coded? (q/raw-key))))
              ;; Create transition function
              (let [from (get-from @objects)
                    to   (get-to @objects)
                    transition-ch (connect-states from to)]
                ;; Strip 'from' and 'to' of their respective labels
                ;; indicating they're ready to be connected and update
                ;; 'from' and 'to' so that their new transition function
                ;; is listed in each's :out/:in map.
                (when-not (transition-exists? from to transition-ch)
                  (if-not (= from to)
                    (do
                        (swap! objects assoc (key from)
                               (update-in
                                ;; Strip :from pair from val
                                (dissoc (val from) :from)
                                ;; Add transition function to val
                                [:out (getname to)] conj transition-ch))
                        (swap! objects assoc (key to)
                               (update-in
                                ;; Strip :to pair from val
                                (dissoc (val to) :to)
                                ;; Add transition function to val
                                [:in (getname from)] conj transition-ch)))
                    (swap! objects assoc (key from)
                           (update-in
                            (update-in
                             ;; Strip :from & :to pairs from val
                             (dissoc (dissoc (val from) :from) :to)
                             ;; Add transition function to :out & :in since
                             ;; function is a loop
                             [:out (getname from)] conj transition-ch)
                            [:in (getname from)] conj transition-ch))))

                (q/delay-frame 70)
                ;; Return to create mode
                (swap! (q/state-atom) assoc-in [:mode] :create)))
      :set-final (when-let
                     [final (when (q/mouse-pressed?)
                              (apply map-entry (assoc-in (capture-state @objects x y) [1 :final?] true)))]
                   (swap! objects assoc (key final) (val final))
                   (swap! (q/state-atom) assoc-in [:mode] :create)
                   (display-state final))
      :delete (when-let
                  [delete (when (or (println "deleting") (q/mouse-pressed?))
                            (capture-state @objects x y))]
                (swap! objects purge-transitions delete)
                (swap! objects dissoc (key delete))
                (when-not (empty? @objects)
                  ;; Swap keys/names for deleted state and the last
                  ;; state in objects.
                  (if-let [last-o
                           (when (pos? (compare
                                        (key (last @objects)) (key delete)))
                             (last @objects))]
                    (do (swap! objects assoc (key delete)
                               (assoc (val last-o) :name (getname delete)))
                        (when (-> delete val :start? true?) (swap! objects assoc-in [(key delete) :start?] true))
                        (swap! objects dissoc (key last-o))
                        (swap! objects nested-rename-key
                               (key last-o) (key delete)))
                    (when-let [last-o (when (-> delete val :start? true?)
                                        (last @objects))]
                      (swap! objects assoc (key delete)
                             (assoc (val last-o) :name (getname delete)))
                      (swap! objects assoc-in [(key delete) :start?] true)
                      (swap! objects dissoc (key last-o))
                      (swap! objects nested-rename-key (key last-o) (key delete)))))
                (redraw-sketch!)
                (swap! (q/state-atom) assoc-in [:mode] :create))
      :set-start (when-let
                     [start (when (q/mouse-pressed?)
                              (capture-state @objects x y))]
                   ;; Purge old start state's signifier
                   ;; (change :start? to false)
                   (let [old-start-k (f->b/get-start-node @objects)]
                     (swap! objects assoc-in
                            [old-start-k :start?] false))
                   (swap! objects assoc-in [(key start) :start?] true)
                   (redraw-sketch!)
                   (swap! (q/state-atom) assoc-in [:mode] :create)))

    ;; Prevents single click from being processed as multiple clicks
    ;; which could create multiple states or connect a state to itself
    ;; when unintended.
    (when (q/mouse-pressed?) (q/delay-frame 70))))



(q/defsketch nfac
  :title "Non-Deterministic Finite Automaton Creator"
  :size [800 800]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

