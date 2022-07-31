;; Functions for allowing the user to input strings to be tested for
;; membership in language described by NFA.
(ns nfac.test-string
  (:require [quil.core :as q]
            [nfac.objects :refer [objects]]
            [automata.nfa :as b]))

(defn start?
  "Return name of start node"
  [state]
  (-> state val :start? true?))

(defn get-start-node-name
  "Return name of start node."
  [states]
  (->> states (some #(when (start? %) %)) first))


(defn prompt
  "Prompt displaying instructions for the user to enter test string(s)."
  []
  (q/text (str "Type in a string and press Enter to test membership in string "
               "or Alt/Option to test successive strings.") 0 0 600 90))


(defn setup
  "Function to be called upon initialization for test-string sketch."
  []
  (q/text-align :center)
  (q/text-size 20)
  (q/color-mode :hsb)
  (q/background 255)
  (q/fill 0)
  (prompt)
  (q/set-state! :text "" :mode :normal))

(defn draw-str
  "Display the current value of :text within state-atom for sketch."
  []
  (q/text
   (str \" (q/state :text) \") 300 110))

(defn draw-result
  "Display the result of whether the test string was accepted or
  rejected by the language described by the NFA."
  []
  (q/no-stroke)
  (q/with-fill 255 (q/rect 200 150 100 100))
  ;; FOUND IT >:)
  (let [result ((b/test-string (q/state :text) (get-start-node-name @objects))
                :result)]
    (if result
      ;; Green
      (q/with-fill [90 255 210] (q/text "accepted" 300 130))
      ;; Red
      (q/with-fill [0 255 255] (q/text "rejected" 300 130))))
  (q/stroke 0))

(defn key-released
  "Function to be called when key is released inside test-string sketch."
  []
  (if (= (q/state :mode) :final)
    (q/exit)
    (if (= (q/raw-key) \newline)
      (do
        (q/background 255)
        (prompt)
        (draw-str)
        (draw-result)
        (swap! (q/state-atom) assoc-in [:mode] :final))
      (do
        (if (= (q/key-as-keyword) :alt)

          (do (q/background 255)
              (draw-str)
              (draw-result)
              (swap! (q/state-atom) assoc-in [:text] ""))
          (when-let [text-size (when (and
                                      (not= (q/key-as-keyword) :shift)
                                      (not= (q/key-as-keyword) (keyword ""))
                                      (not= (q/key-as-keyword) :control))
                                 (count (q/state :text)))]
            (if (= (q/raw-key) \backspace)
              (do
                (swap! (q/state-atom) assoc-in [:text]
                       (subs (q/state :text) 0 (max 0 (dec text-size))))
                (q/background 255)
                (draw-str))
              (when (<= text-size 40)
                (swap! (q/state-atom) update-in [:text] str (q/raw-key))
                (q/background 255)
                (draw-str)))))
        (prompt)))))

;; Sketch for inputing strings for validation.
(def test-str-ske
  [:size [600 180]
   :title "Test-String(s): "
   :setup setup
   :key-released key-released])
