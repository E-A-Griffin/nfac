(ns dfac.other-file)


    (if (= (:mode state) :create)
      (when (q/mouse-pressed?)
        (let
            ;; Format: {"x-y" {:name "name" :x x :y y :h 0-255 :s 0-255 :v 0-255}}
            [ellipse {(str \q (count @objects))
                       {:name (str \q (count @objects))
                        :x x :y y
                        :h color :s 255 :v 255}}]
          ;; Prevent click from being processed as multiple clicks which would
          ;; create multiple states for a single click
          (Thread/sleep 50)
          (display-ellipse (first ellipse))
          (swap! objects conj ellipse)))

      (if (= (:mode state) :transition)
        (when (q/mouse-pressed?)
          (if-let [captured (capture-ellipse @objects x y)]
            (do (swap! objects assoc
                       (first captured)    ;; k
                       (second captured))  ;; v

                ;; First state selected, switch mode so that next
                ;; state selected will initiate connecting states via
                ;; transition function.
                (swap! (q/state-atom) assoc-in [:mode] :connect)

                ;; Prevent single click from being processed as duplicate clicks
                (Thread/sleep 50))))

        (if (= (:mode state) :connect)
          (when (q/mouse-pressed?)
            (if-let [captured (capture-ellipse @objects x y)]
              (do (swap! objects assoc (first captured)
                       (second captured))
                  ;; Map of two ellipses to be connected
                  (apply connect-ellipses (get-selected-pair @objects))))))))
