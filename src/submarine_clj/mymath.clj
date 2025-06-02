(ns submarine-clj.mymath
  (:gen-class)
  (:require [clojure.math :refer [sqrt pow]]))

(defrecord Quaternion [w x y z])

(defrecord Vec3D [x y z])

(defn print-quaternion
  "Print the quaternion"
  [quaternion]
  (println "w:" @(:w quaternion) 
           "x:" @(:x quaternion)
           "y:" @(:y quaternion)
           "z:" @(:z quaternion)))

(defn gen-vec3d
  ([]
   (Vec3D. (atom 0) (atom 0) (atom 0)))
  ([x y z]
   (Vec3D. (atom x) (atom y) (atom z))))

(defn gen-quaternion
  ([]
   (Quaternion. (atom 0) (atom 0) (atom 0) (atom 0)))
  ([w x y z]
   (Quaternion. (atom w) (atom x) (atom y) (atom z))))

(defn vect-to-quat
  "Given a vector, generates a new quaternion with 0 for w"
  [v]
  (gen-quaternion 0 @(:x v) @(:y v) @(:z v)))


(defn quat-normalize
  "Normalizes quaternion components, its module shall be 1"
  [w x y z]
  (let [norm (sqrt (+ (pow w 2) (pow x 2) (pow y 2) (pow z 2)))]
    (list (/ w norm)
          (/ x norm)
          (/ y norm)
          (/ z norm))))

(defn quat-conj
  "Obtains a new conjugate quaternion."
  [q]
  (gen-quaternion @(:w q)
                  (- @(:x q))
                  (- @(:y q))
                  (- @(:z q))))

(defn vec3d-normalize
  "Normalizes 3D vector components, its module shall be 1"
  [x y z]
  (let [norm (sqrt (+ (pow x 2) (pow y 2) (pow z 2)))]
    (list (/ x norm)
          (/ y norm)
          (/ z norm))))

(defn quat-multiply
  "Multiplies and generates two quaternions"
  ([q1 q2]
   (gen-quaternion
    (- (* @(:w q1) @(:w q2)) (* @(:x q1) @(:x q2)) (* @(:y q1) @(:y q2)) (* @(:z q1) @(:z q2)))
    (+ (* @(:w q1) @(:x q2)) (* @(:x q1) @(:w q2)) (- (* @(:y q1) @(:z q2)) (* @(:z q1) @(:y q2))))
    (+ (- (* @(:w q1) @(:y q2)) (* @(:x q1) @(:z q2))) (* @(:y q1) @(:w q2)) (* @(:z q1) @(:x q2)))
    (+ (* @(:w q1) @(:z q2)) (- (* @(:x q1) @(:y q2)) (* @(:y q1) @(:x q2))) (* @(:z q1) @(:w q2)))))
  ([q1 q2 q3]
   (quat-multiply (quat-multiply q1 q2) q3)))

(defn rotate-vector
  "Applies rotation to a vector"
  [q orientation-vector]
  (let [q-vector (vect-to-quat orientation-vector)
        q-conj   (quat-conj q)
        q-result (quat-multiply q q-vector q-conj)
        norm-vec (vec3d-normalize @(:x q-result)
                                  @(:y q-result)
                                  @(:z q-result))]
    (reset! (:x orientation-vector) (nth norm-vec 0))
    (reset! (:y orientation-vector) (nth norm-vec 1))
    (reset! (:z orientation-vector) (nth norm-vec 2))))
