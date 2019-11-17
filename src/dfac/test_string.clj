(ns dfac.test-string
  (:require [quil.core :as q]
            [clojure.java.io :as io]
            [dfac.test-string :as t-str]
            [dfac.objects :refer [objects test-strings]]
            [clojure.set :as s]))

(defn setup
  "Function to be called upon initialization for test-string sketch."
  []
  (q/text-align :center)
  (q/text-size 20)
  (q/color-mode :hsb)
  (q/background 255)
  (q/fill 255 140 240)
  (q/text (str "Type in a string and press Enter to test membership in"
               " string or Alt/Option to test successive strings.")
          0 0 600 90)
  (q/set-state! :all-text [] :text "" :x 50 :y 50 :mode :normal))

(defn on-close
  "Function to be called upon exit for test-string sketch."
  []
  (reset! test-strings (q/state :all-text)))

(defn key-released
  "Function to be called when key is released inside test-string sketch."
  []
  (prn (q/key-as-keyword))
  (q/with-fill 0
    (if (= (q/raw-key) \newline)
      (do
        (swap! (q/state-atom) update-in [:all-text] conj (q/state :text))
        (q/exit)))
      (if (= (q/key-as-keyword) :alt)
        (do (prn (q/state :all-text))
            (swap! (q/state-atom) update-in [:all-text] conj (q/state :text))
            (swap! (q/state-atom) assoc-in [:text] "")
            (swap! (q/state-atom) assoc-in [:mode] :normal)
            (q/background 255))
        (if-let [text-size (if (and
                                (not= (q/key-as-keyword) :shift)
                                (not= (q/key-as-keyword) (keyword ""))
                                (not= (q/key-as-keyword) :control))
                             (count (q/state :text)))]
          (if (= (q/raw-key) \backspace)
            (do (swap! (q/state-atom) assoc-in [:text]
                       (subs (q/state :text) 0 (max 0 (dec text-size))))
                (q/background 255)
                (q/text (q/state :text) 300 110))
            (when (<= text-size 40)
              (swap! (q/state-atom) update-in [:text] str (q/raw-key))
              (q/background 255)
              (q/text
               (q/state :text) 300 110)))))
    (q/fill 255 140 240)
    (q/text (str "Type in a string and press Enter to test membership in"
                 " string or Alt/Option to test successive strings.")
            0 0 600 90)))

(def test-str-ske
  [:size [600 180]
   :title "Test-String(s): "
   :setup t-str/setup
   :on-close t-str/on-close
   :key-released t-str/key-released])
