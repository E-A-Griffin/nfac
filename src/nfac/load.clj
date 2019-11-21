;; Functions for loading an NFA from saved-nfas/
(ns nfac.load
  (:require [quil.core :as q]
            [clojure.java.io :as io]
            [nfac.load :as ld]
            [nfac.objects :refer [objects]]))

(defn print-save-files
  "Function to print save files inside of \"saved-nfas/\"."
  []
  (q/background 255)
  (let [files (vec (take 8 (drop (q/state :selected)
                                 (filter #(re-find #".*\.nfa" %) (.list (io/file "saved-nfas/"))))))]
    (doseq [file files]
      (if (= (.indexOf files file) 0)
        ;; Print pink
        (q/with-fill [255 140 240]
          (q/text file 0 20))
        (q/text file 0 (* 20 (inc (.indexOf files file))))))))

(defn setup
  "Function to be called upon initialization for load sketch."
  []
  (q/background 255)
  (q/text-align :left)
  (q/text-size 20)
  (q/color-mode :hsb)
  (q/set-state! :selected 0 :size (count (filter #(re-find #".*\.nfa" %) (.list (io/file "saved-nfas/")))) :x 50 :y 10)
  (q/fill 0)
  (print-save-files))

(defn on-close
  "Function to be called upon exit for load sketch."
  []
  (let [loaded-m
        (read-string
         (slurp
          (str "saved-nfas/"
               (nth
                (filter #(re-find #".*\.nfa" %)
                        (.list (io/file "saved-nfas/")))
                (q/state :selected)))))]
    ;; Very basic validation to ensure proper map was loaded to reset!
    ;; objects with
    (when (and (not (empty? loaded-m))
               (map? loaded-m) (every? string? (keys loaded-m))
               (every? map? (vals loaded-m)))
      (reset! objects loaded-m)
      (swap! objects assoc :reload true))))

(defn key-released
  "Function to be called when key is released inside load sketch."
  []
  (if (and (= (q/key-as-keyword) :down)
           (< (inc (q/state :selected)) (q/state :size)))
    (do
      (q/delay-frame 70)
      (swap! (q/state-atom) update :selected inc)
      (print-save-files))
    (if (and (= (q/key-as-keyword) :up)
             (not (zero? (q/state :selected))))
      (do
        (q/delay-frame 70)
        (swap! (q/state-atom) update :selected dec)
        (print-save-files))
      (when (= (q/raw-key) \newline)
        (q/exit)))))

(def load-ske
  [:size [400 180]
  :title "Load File As: "
  :setup ld/setup
  :on-close ld/on-close
  :key-released ld/key-released])

