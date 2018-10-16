(ns fixpoint.interceptors
  (:require [manifold.deferred :as d]))

(defn normalize [i]
  (cond
    (nil? i)
    []

    (= {} i)
    []

    (seq? i)
    (vec i)

    (vector? i)
    i

    (fn? i)
    [{:enter i}]

    (map? i)
    [i]))


(defn chain
  ([]
   [])
  ([i1]
   (normalize i1))
  ([i1 i2]
   (let [i1 (normalize i1)
         i2 (normalize i2)]
     (into i1 i2)))
  ([i1 i2 & is]
   (reduce chain (chain i1 i2) is)))


(defn run-leave [i v idx]
  (if (contains? i idx)
    (let [status (if (instance? Throwable v) :error :leave)
          l (get-in i [idx status] identity)
          v' (try
               (l v)
               (catch Throwable t
                 t))]

      (if (d/deferrable? v')
        (-> (d/->deferred v')
            (d/catch #(run-leave i %      idx))
            (d/chain #(run-leave i % (dec idx))))
        (recur i v' (dec idx))))
    v))


(defn run-enter [i v idx]
  (if (contains? i idx)
    (let [e (:enter (get i idx) identity)
          v' (try
               (e v)
               (catch Throwable t
                 t))]
      (cond
        (instance? Throwable v')
        (run-leave i v' idx)

        (d/deferrable? v')
        (-> (d/->deferred v')
            (d/catch #(run-leave i %      idx))
            (d/chain #(run-enter i % (inc idx))))

        (reduced? v')
        (run-leave i (unreduced v') idx)

        :else
        (recur i v' (inc idx))))
    (run-leave i v (dec idx))))


(defn run [i v]
  (run-enter i v 0))
