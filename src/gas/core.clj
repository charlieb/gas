(ns gas.core)


(defn mk-particle [x y vx vy] {:x x :y y :vx vx :vy vy})

(defn mk-particles [n w h v]
  (repeatedly n #(mk-particle (rand w) (rand h) (rand v) (rand v))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (quot (f item) d) #(cons item %)))
          {}
          items))

(defn adjacent-buckets [buckets d]
  (loop [bs (keys buckets)
         adjs []]
    (if bs
      (recur 
        (rest bs)
        (if (>= d (- (second bs) (first bs)))
          (if (and (nth bs 3) (>= d (- (nth bs 3) (second bs))))
            (cons (take 3 bs) adjs)
            (cons (take 2 bs) adjs))
          (if (and (nth bs 3) (>= d (- (nth bs 3) (second bs))))
            (cons (take 2 (rest bs) adjs))
            adjs)))
      adjs)))
        


(defn collisions [buckets])
(defn foo
  "main?"
  []
  (let [d 2 ; particle diameter
        ps (mk-particles 20 10 10 1)
        buckets (into {} (map (fn [[k v]] 
                                [k (bucket-by v :x d)])
                              (bucket-by ps :y d)))]
    (println (keys buckets))))
