(ns fixpoint.interceptors-test
  (:require [clojure.test :refer :all]
            [fixpoint.interceptors :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [manifold.deferred :as d]))

(def gen-integer-fn
  (gen/elements
   [identity
    (constantly 0)
    (constantly 1)
    inc
    dec]))

(defn defer-val [x]
  (d/future x))

(def gen-interceptor
  (gen/let [enter gen-integer-fn
            leave gen-integer-fn]
    {:enter enter
     :leave leave}))

(defn constantly-error [_]
  (throw (ex-info "Error" {})))

(defn constantly-error-async [_]
  (let [d (d/deferred)]
    (d/error! d (ex-info "Async error." {:async true}))
    d))

(defspec chain-associative
  1000
  (prop/for-all [a gen-interceptor
                 b gen-interceptor
                 c gen-interceptor
                 x gen/int]
    (let [x (mod x 100)
          i1 (chain a (chain b c))
          i2 (chain (chain a b) c)]
      (= (run i1 x) (run i2 x)))))

(defspec chain-errors
  1000
  (prop/for-all [i (gen/vector gen-interceptor)
                 x gen/int]
    (let [t (ex-info "My error" {})
          i (chain {:error (constantly t)}
                   i
                   {:enter constantly-error})
          x (mod x 100)]
      (identical? t (run i x)))))

(defspec chain-error-not-caught
  1000
  (prop/for-all [i (gen/vector gen-interceptor)
                 x gen/int]
    (let [i (chain {:enter constantly-error} i)
          x (mod x 100)]
      (instance? Throwable (run i x)))))

(defspec chain-error-first
  1000
  (prop/for-all [i (gen/vector gen-interceptor)
                 x gen/int]
    (let [i (chain {:enter constantly-error}
                   i
                   {:error (constantly :error)})
          x (mod x 100)]
      (instance? Throwable (run i x)))))

(defspec chain-error-handler-same
  1000
  (prop/for-all [i (gen/vector gen-interceptor)
                 x gen/int]
    (let [t (ex-info "My error" {})
          i (chain i
                   {:enter constantly-error
                    :error (constantly t)}
                   i)
          x (mod x 100)]
      (identical? t (run i x)))))

(defn make-async [i]
  (-> i
      (update :enter #(comp defer-val %))
      (update :leave #(comp defer-val %))))

(defspec chain-async
  1000
  (prop/for-all [i (gen/not-empty (gen/vector gen-interceptor))
                 x gen/int]
    (let [ai (mapv make-async i)
          x (mod x 100)]
      (= (run i x) @(run ai x)))))

(defspec chain-async-one
  1000
  (prop/for-all [i (gen/not-empty (gen/vector gen-interceptor))
                 idx gen/int
                 x gen/int]
    (let [idx (mod idx (count i))
          ai (update i idx make-async)
          x (mod x 100)]
      (= (run i x) @(run ai x)))))

(defn make-reduced [i]
  (update i :enter #(comp reduced %)))

(defspec chain-early-exit
  1000
  (prop/for-all [a gen-interceptor
                 b gen-interceptor
                 x gen/int]
    (let [x (mod x 100)
          i1 (chain a)
          i2 (chain (make-reduced a) b)]
      (= (run i1 x) (run i2 x)))))

(defspec chain-error-async
  1000
  (prop/for-all [i (gen/vector gen-interceptor)]
    (let [e constantly-error-async]
      (:async (ex-data @(run (chain i e) 0))))))

(deftest error-order-test
  (let [i (chain {:error (fn [e] (if (:a (ex-data e))
                                   (ex-info "Wrong" {:wrong true})
                                   (ex-info "Right" {:right true})))}
                 {:enter (fn [_] (throw (ex-info "Error." {:a true})))
                  :error (fn [e] (ex-info "Transform" {}))})]
    (is (:right (ex-data (run i 0))))
    (is (not (:wrong (ex-data (run i 0)))))))

(deftest error-order-test-async
  (let [i (chain {:error (fn [e] (if (:a (ex-data e))
                                   (ex-info "Wrong" {:wrong true})
                                   (ex-info "Right" {:right true})))}
                 {:enter (fn [_] (defer-val 0))}
                 {:enter (fn [_] (throw (ex-info "Error." {:a true})))
                  :error (fn [e] (ex-info "Transform" {}))})]
    
    (is (:right (ex-data @(run i 0))))
    (is (not (:wrong (ex-data @(run i 0)))))))

(deftest error-order-test-async-leave
  (let [i (chain {:error (fn [e] (if (:a (ex-data e))
                                   (ex-info "Right" {:right true})
                                   (ex-info "Wrong" {:wrong true})))}
                 {:enter (fn [_] (defer-val 0))}
                 {:leave (fn [_] (throw (ex-info "Error" {:a true})))
                  :error (fn [e] (ex-info "Transform" {}))})]
    
    (is (:right (ex-data @(run i 0))))
    (is (not (:wrong (ex-data @(run i 0)))))))

(deftest error-order-test-async-leave2
  (let [i (chain {:error (fn [e] (if (:a (ex-data e))
                                   (ex-info "Wrong" {:wrong true})
                                   (ex-info "Right" {:right true})))}
                 {:enter (fn [_] (defer-val 0))}
                 {:error (fn [e] (ex-info "Transform" {}))}
                 {:leave (fn [_] (throw (ex-info "Error" {:a true})))})]
    
    (is (:right (ex-data @(run i 0))))
    (is (not (:wrong (ex-data @(run i 0)))))))
