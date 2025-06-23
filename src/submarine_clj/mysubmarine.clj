(ns submarine-clj.mysubmarine
  (:gen-class)
  (:require [clojure.math :refer [sin cos PI]]
            [submarine-clj.mymath :refer [gen-vec3d
                                          gen-quaternion
                                          quat-conj
                                          vec3d-normalize
                                          quat-multiply
                                          quat-normalize
                                          vect-to-quat
                                          rotate-vector
                                          print-quaternion]]))


(def quat-submarine (gen-quaternion 1 0 0 0))
(def orientation-submarine (gen-vec3d 0 1 0))
(def depth (atom  0))

(def cordZ depth)
(def cordY (atom 0))
(def cordX (atom 0))

(def heading (atom 0))
(def pitch (atom 0))
(def roll (atom 0))

(defn heading-rad []  (* @heading  (/ PI 180)))
(defn pitch-rad []    (* @pitch    (/ PI 180)))
(defn roll-rad []     (* @roll     (/ PI 180)))

(def linear-speed (atom 0))

(def exit-loop (atom false))

(defn setQuaternionSubmarine
  "Set roll pitch yaw angles in radians to submarine quaternion"
  [roll pitch yaw]
  (let [cr (cos (* roll 0.5))  sr (sin (* roll 0.5))
        cp (cos (* pitch 0.5)) sp (sin (* pitch 0.5))
        cy (cos (* yaw 0.5))   sy (sin (* yaw 0.5))
        w (+ (* cr cp cy) (* sr sp sy))
        x (- (* sr cp cy) (* cr sp sy))
        y (+ (* cr sp cy) (* sr cp sy))
        z (- (* cr cp sy) (* sr sp cy))
        norm (quat-normalize w x y z)]
    (reset! (:w quat-submarine) (nth norm 0))
    (reset! (:x quat-submarine) (nth norm 1))
    (reset! (:y quat-submarine) (nth norm 2))
    (reset! (:z quat-submarine) (nth norm 3))
    (print-quaternion quat-submarine)))

(defn nudge
  "Pushes the submarine"
  []
  (reset! cordX (+ @cordX (* @linear-speed @(:x orientation-submarine))))
  (reset! cordY (+ @cordY (* @linear-speed @(:y orientation-submarine))))
  (reset! cordZ (+ @cordZ (* @linear-speed @(:z orientation-submarine)))))

(defn turn-port-1 []
  (let [new-yaw (mod (+ @heading 359) 360)]
    (reset! heading new-yaw)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; <-

(defn turn-starboard-1 []
  (let [new-yaw (mod (inc @heading) 360)]
    (reset! heading new-yaw)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; ->

(defn tilt-up-1 []
  (let [new-pitch (mod (+ @pitch 359) 360)]
    (reset! pitch new-pitch)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; ^

(defn tilt-down-1 []
  (let [new-pitch (mod (inc @pitch) 360)]
    (reset! pitch new-pitch)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; v

(defn roll-port-1 []
  (let [new-roll (mod (+ @roll 359) 360)]
    (reset! roll new-roll)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; <-

(defn roll-starboard-1 []
  (let [new-roll (mod (inc @roll) 360)]
    (reset! roll new-roll)
    (setQuaternionSubmarine (roll-rad) (pitch-rad) (heading-rad))
    (rotate-vector quat-submarine orientation-submarine))) ;; ->

(defn ascend-1 []
  (when-not (<= @depth 0)
    (reset! depth (dec @depth))))

(defn descend-1 []
  (reset! depth (inc @depth)))

(defn roll-port-lot [times]
  (dotimes [_ times] (roll-port-1)))

(defn roll-starboard-lot [times]
  (dotimes [_ times] (roll-starboard-1)))

(defn tilt-down-lot [times]
  (dotimes [_ times] (tilt-down-1)))

(defn tilt-up-lot [times]
  (dotimes [_ times] (tilt-up-1)))

(defn turn-port-lot [times]
  (dotimes [_ times] (turn-port-1)))

(defn turn-starboard-lot [times]
  (dotimes [_ times] (turn-starboard-1)))

(defn ascend-lot [times]
  (dotimes [_ times] (ascend-1)))

(defn descend-lot [times]
  (dotimes [_ times] (descend-1)))

(defn inc-speed-1 []
  (when-not (> @linear-speed 10)
    (reset! linear-speed (inc @linear-speed))))

(defn dec-speed-1 []
  (when-not (<= @linear-speed 0)
    (reset! linear-speed (dec @linear-speed))))

(defn update-game-engine []
  (nudge))

(defn reset-coord []
  (reset! cordX 0)
  (reset! cordY 0)
  (reset! cordZ 0))

(defn reset-orientation []
  (reset! heading 0)
  (reset! pitch   0)
  (reset! roll    0))

(defn reset-speed []
  (reset! linear-speed 0))

(defn reset-submarine []
  (reset-coord)
  (reset-orientation)
  (reset-speed))

(defn parse-user-input [user-input]
  (let [patt-tr #"^\s*(turn|roll)\s+(starboard|port)\s+(\d+)\s*$"
        patt-ad #"^\s*(ascend|descend)\s+(\d+)\s*$"
        patt-ti #"^\s*(tilt)\s+(up|down)\s+(\d+)\s*$"
        patt-full (re-pattern (str "(" patt-tr ")"
                                   "|(" patt-ad ")"
                                   "|(" patt-ti ")"))
        parsed-ui (vec (remove nil? (re-matches patt-full user-input)))
        the-order (nth parsed-ui 2 [])]
    (if (not-empty parsed-ui)
      (if (or (= "descend" the-order)
              (= "ascend" the-order))
        {:order the-order,
         :quantity (nth parsed-ui 3)}
        {:order the-order,
         :orientation (nth parsed-ui 3)
         :quantity (nth parsed-ui 4)})
      nil)))

(defn roll-status []
  (str (cond
         (and (> @roll 0)
              (< @roll 180)) "> "
         (and (> @roll 180)
              (< @roll 360)) "< "
         (= 0 @roll) "| ") @roll))

(defn submarine-status []
  (println (str "GPS: CordY = " @cordY ", CordX = " @cordX "\n"
                "Heading: " @heading "º, Depth = " @depth " m, Roll: " (roll-status)  "º\n"
                "Throttle: " @linear-speed " kt, Pitch = " @pitch "º")))
