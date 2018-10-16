(ns fixpoint.interceptors
  (:require [manifold.deferred :as d]))

;; standard function comp

(defn chain [i1 i2]
  {:enter (comp (:enter i1 identity)
                (:enter i2 identity))
   :leave (comp (:leave i2 identity)
                (:leave i1 identity))})

(defn run [interceptor value]
  (let [{:keys [enter leave]} interceptor]
    (-> value
        enter
        leave)))

;; normalized form

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
   (apply chain (chain i1 i2) is)))

(defn run-leave [i v idx]
  (if (contains? i idx)
    (let [l (:leave (get i idx) identity)]
      (recur i (l v) (dec idx)))
    v))

(defn run-enter [i v idx]
  (if (contains? i idx)
    (let [e (:enter (get i idx) identity)]
      (recur i (e v) (inc idx)))
    (run-leave i v (dec idx))))

(defn run [i v]
  (run-enter i v 0))

;; add error handling

(defn run-leave [i v idx]
  (if (contains? i idx)
    (let [status (if (instance? Throwable v) :error :leave)
          l (get-in i [idx status] identity)
          v' (try
               (l v)
               (catch Throwable t
                 t))]
      (recur i v' (dec idx)))
    v))

(defn run-enter [i v idx]
  (if (contains? i idx)
    (let [e (:enter (get i idx) identity)
          v' (try
               (e v)
               (catch Throwable t
                 t))]
      (if (instance? Throwable v')
        (run-leave i v' idx)
        (recur i v' (inc idx))))
    (run-leave i v (dec idx))))

;; async

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
            (d/chain #(run-enter i % (inc idx))))

        :else
        (recur i v' (inc idx))))
    (run-leave i v (dec idx))))

;; early return

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
            (d/chain #(run-enter i % (inc idx))))

        (reduced? v')
        (run-leave i (unreduced v') idx)

        :else
        (recur i v' (inc idx))))
    (run-leave i v (dec idx))))
