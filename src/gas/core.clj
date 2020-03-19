(ns gas.core
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [quil.core :as q :include-macros true] ))

(def D 10.)
(defn mk-particle [x y vx vy] {:x x :y y :vx vx :vy vy})

(defn mk-particles [n w h d]
  (repeatedly n #(mk-particle (rand w) (rand h) (rand d) (rand d))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (* d (quot (f item) d)) #(cons item %)))
          {}
          items))

(defn collide-particle-lists
  "Detect collisions between 'collide-parts' and 'concat collide-parts parts'.
  Return a list of pairs of colliding particles."
  [collide-parts parts]
  (loop [cols collide-parts
         other (drop-while #(< D (- (:x (first cols)) (:x %))) parts)
         pairs (list)]
    (if (empty? cols)
      pairs
      (recur 
        (rest cols)
        (drop-while #(< D (- (:x (second cols)) (:x %))) parts)
        (concat pairs
                (map list 
                     (concat (take-while #(>= D (math/abs (- (:x %) (:x (first cols))))) other)
                             (take-while #(>= D (- (:x %) (:x (first cols)))) (rest cols)))
                     (repeat (first cols))))))))


(defn collisions
  "Yields a list of pairs of colliding particles"
  [buckets]
  (loop [bys (keys buckets)
         pairs (list)]
    (if (empty? bys)
      pairs
      (recur (rest bys)
             (concat pairs
                     (collide-particle-lists (buckets (first bys))
                                             (buckets (+ (first bys) D))))))))

(defn iterate-particle-sim [particles])

(defn init-particles-pairs
  "main?"
  [n w h]
  (let [ps (mk-particles n w h D)
        buckets (into {} (map (fn [[k v]] [k (sort-by :x v)])
                              (bucket-by ps :y D)))
        cols (collisions buckets)]
    {:particles ps :pairs cols :buckets buckets :stop false}))


;; ---------- Driver ---------------
(def WIDTH 500)
(def HEIGHT 500)

(defn update-particles [state]
  (let [ps (mk-particles (count (:particles state)) WIDTH HEIGHT D)
        buckets (into {} (map (fn [[k v]] [k (sort-by :x v)])
                              (bucket-by ps :y D)))
        cols (collisions buckets)]
  (assoc state :particles ps :pairs cols :buckets buckets)))

(defn run-sim [state] 
  (loop [] 
    (let [st @state]
    (when (not (:stop st))
      (print ".")
      (swap! state update-particles)
      (recur)))))

;; ---------- QUIL Viz -------------


(defn draw [state]
  (q/background 0)
  (q/no-fill)
;;  (q/stroke 255 0 0)
;;  (doseq [p (:particles state)]
;;    (q/ellipse (:x p) (:y p) D D))
  (q/stroke 0 255 0)
  (doseq [[p1 p2] (:pairs state)]
    (q/line (:x p1) (:y p1) (:x p2) (:y p2)))
  (q/stroke 0 0 255)
  (doseq [[v b] (:buckets state)]
    (q/stroke 0 v 255)
    (doseq [p b]
      (q/ellipse (:x p) (:y p) D D))))

(defn start-sketch [state]
  (q/sketch
    :host "host"
    :size [WIDTH HEIGHT]
    :draw #(draw @state)
    :on-close (fn [] (swap! state #(assoc % :stop true))
  )))

(defn start [n]
  (let [state (atom (init-particles-pairs n WIDTH HEIGHT))]
    (async/thread (run-sim state))
    (start-sketch state)))

