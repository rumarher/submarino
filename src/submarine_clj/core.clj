(ns submarine-clj.core
  (:gen-class)
  (:require [clojure.main :refer [repl skip-whitespace]]
            [clojure.core.async :refer [go-loop >! <! go timeout]]
            [submarine-clj.mysubmarine :refer [turn-starboard-1
                                               turn-port-1
                                               roll-starboard-1
                                               roll-port-1
                                               ascend-1
                                               descend-1
                                               ascend-lot
                                               descend-lot
                                               turn-starboard-lot
                                               turn-port-lot
                                               roll-starboard-lot
                                               roll-port-lot
                                               tilt-down-1
                                               tilt-up-1
                                               tilt-up-lot
                                               tilt-up-lot
                                               tilt-down-lot
                                               inc-speed-1
                                               dec-speed-1
                                               submarine-status
                                               reset-submarine
                                               reset-coord
                                               reset-orientation
                                               reset-speed
                                               exit-loop
                                               update-game-engine]]))



(def repl-submarine-control-panel
  [:prompt #(printf "submarine order: ")
   :read   (fn [request-prompt request-exit]
             (or ({:line-start request-prompt :stream-end request-exit}
                  (skip-whitespace *in*))
                 (read-line)))
   :eval   (fn [user-input]
             (cond
               (= user-input "turn starboard")     (turn-starboard-1)
               (= user-input "turn port")          (turn-port-1)
               (= user-input "roll starboard")     (roll-starboard-1)
               (= user-input "roll port")          (roll-port-1)
	       (= user-input "ascend")             (ascend-1)
	       (= user-input "ascend lot")         (ascend-lot)
	       (= user-input "descend")            (descend-1)
	       (= user-input "descend lot")        (descend-lot)
               (= user-input "turn lot starboard") (turn-starboard-lot)
               (= user-input "turn lot port")      (turn-port-lot)
               (= user-input "roll lot starboard") (roll-starboard-lot)
               (= user-input "roll lot port")      (roll-port-lot)
	       (= user-input "tilt down")          (tilt-down-1)
	       (= user-input "tilt up")            (tilt-up-1)
	       (= user-input "tilt lot down")      (tilt-up-lot)
	       (= user-input "tilt lot up")        (tilt-down-lot)

               (= user-input "inc speed")          (inc-speed-1)
               (= user-input "dec speed")          (dec-speed-1)

               (= user-input "status")             (submarine-status)

	       (= user-input "reset all")          (reset-submarine)
	       (= user-input "reset coord")        (reset-coord)
	       (= user-input "reset orientation")  (reset-orientation)
	       (= user-input "reset speed")        (reset-speed)
               (= user-input "update")             (update-game-engine)
               (= user-input "exit")               (do
                                                     (reset! exit-loop true))
	       :else (println "Bad input!" user-input)))])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cond
    (= (first args) "manual"
       (println "Manual updating engine")
       (apply repl repl-submarine-control-panel))
    :else
    (go-loop []
      (<! (timeout 1000))
      (update-game-engine)
      (when-not @exit-loop (recur)))
    (go []
        (println "Bienvenido al submarino!")
        (apply repl repl-submarine-control-panel))))
