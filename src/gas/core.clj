(ns gas.core
  (:require [clojure.math.numeric-tower :as math]
            [quil.core :as q :include-macros true] ))

(def D 2.)
(defn mk-particle [x y vx vy] {:x x :y y :vx vx :vy vy})

(defn mk-particles [n w h v]
  (repeatedly n #(mk-particle (rand w) (rand h) (rand v) (rand v))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (quot (f item) d) #(cons item %)))
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
            (concat (take-while #(<= D (math/abs (- (:x %) (:x (first cols))))) other)
                    (take-while #(<= D (- (:x %) (:x (first cols)))) (rest cols)))
            (repeat (first cols))))))))


(defn collisions
  "Yields a list of pairs of colliding particles"
  [buckets]
  (loop [bys (keys buckets)
         pairs (list)]
    (println bys)
    (if (empty? bys)
      pairs
      (recur (rest bys)
             (collide-particle-lists (buckets (first bys))
                                     (buckets (+ (first bys) D)))))))

(defn foo
  "main?"
  []
  (let [ps (mk-particles 20 10 10 1)
        buckets (into {} (map (fn [[k v]] [k (sort-by :x v)])
                              (bucket-by ps :y D)))]
    (println (collisions buckets))))

;; ---------- QUIL Viz -------------

(defn draw [state]
  (q/background 0)
  (q/stroke 230)
  (q/no-fill)
  (doseq [p (:particles :state)]
    (q/ellipse (:x p) (:y p) D D))
  (doseq [[p1 p2] (:pairs :state)]
    (q/line (:x p1) (:y p1) (:x p2) (:y p2))))

(defn start-sketch [state]
  (q/sketch
    :host "host"
    :size [500 300]
    :draw #(draw @state)
 ;   :mouse-clicked reset-progress-bar)
  )

(defn progress-bar-iteration [state]
  ; Reset state to 0 once it reaches 100. Otherwise just increment it.
  (swap! state #(if (= % 100) 0 (inc %)))
  ; Schedule next iteration using random delay.
  (js/setTimeout (partial progress-bar-iteration state)
                 (rand-time-to-wait)))

(defn start-progress-bar []
  (let [state (atom 0)
        reset-progress-bar #(reset! state 0 )]
    (progress-bar-iteration state)
    (start-sketch state reset-progress-bar)))

;;(start-progress-bar)
