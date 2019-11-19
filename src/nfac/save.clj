;; Functions for saving an NFA to saved-nfas/
(ns nfac.save
  (:require [quil.core :as q]
            [clojure.java.io :as io]
            [nfac.save :as sv]
            [nfac.objects :refer [objects]]))

(defn setup
  "Function to be called upon initialization for save sketch."
  []
  (q/background 255)
  (q/text-align :center)
  (q/text-size 20)
  (q/color-mode :hsb)
  (q/set-state! :text "" :x 50 :y 50))

(defn on-close
  "Function to be called upon exit for save sketch."
  []
  (spit (str "saved-nfas/" (q/state :text) ".nfa") @objects))

(defn key-released
  "Function to be called when key is released inside save sketch."
  []
  ;; Ignore erroneous input
  (when-not (or (= (q/key-as-keyword) :shift) (= (q/key-as-keyword) :alt)
                (= (q/key-as-keyword) :control)
                (= (q/key-as-keyword) (keyword "")))
                ;;(= (q/raw-key) \newline))
    (q/with-fill [255 140 240]
      (if (= (q/raw-key) \newline)
        (q/exit)
        (let [text-size (count (q/state :text))]
          (if (= (q/raw-key) \backspace)
            (do (swap! (q/state-atom) assoc-in [:text]
                       (subs (q/state :text) 0 (max 0 (dec text-size))))
                (q/background 255)
                (q/text (str (q/state :text) ".nfa") 200 90))
            (when (<= text-size 20)
              (swap! (q/state-atom) update-in [:text] str (q/raw-key))
              (q/background 255)
              (q/text
               (str (q/state :text) ".nfa") 200 90))))))))

(def save-ske
  [:size [400 180]
   :title "Save File As: "
   :setup sv/setup
   :on-close sv/on-close
   :key-released sv/key-released])
