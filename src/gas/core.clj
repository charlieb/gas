(ns gas.core
  (:require [clojure.math.numeric-tower :as math]))

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
   (if (nil? cols)
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
    (print bys)
    (if (nil? bys)
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
