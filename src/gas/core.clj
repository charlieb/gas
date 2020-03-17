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
         adjs (if (<= (- (second bs) (first bs)) d)
                {(first bs) (list (second bs))}
                {})]
    (println bs)
    (let [three (take 3 bs)]
      (println three)
      (if (not (= 3 (count (take 3 bs))))
        adjs
        (recur 
          (rest bs)
          (cond
            (<= (- (nth three 2) (first three)) (* 2 d))
            (assoc adjs (second three) (list (first three) (nth three 2)))

            (<= (- (nth three 2) (second three)) d)
            (assoc adjs (second three) (list (nth three 2)))

          :otherwise adjs))))))

(defn collisions
  "Yields a list of pairs of colliding particles"
  [buckets]
  (loop [bys (vals buckets)
         bxs (vals (buckets (first bys)))
         pairs []]
    (cond (nil? bys) pairs
          (nil? bxs) (recur (rest bys) (vals (buckets (second bys))) pairs)
          :otherwise 
          (recur bys (rest bxs)
                 (let [bx (first bxs)
                       by (first bys)
                       parts ((buckets by) bx)]
                   (reduce
                     (fn [prs part]
                       (let [d 2
                             right ((buckets d) (+ b-x d))
                             down  ((buckets (+ b-y d)) b-x)
                             dori  ((buckets (+ b-y d)) (+ b-x d))]))
                     pairs
                   parts))))))

(defn foo
  "main?"
  []
  let [d 2 ; particle diameter
        ps (mk-particles 20 10 10 1)
        buckets (into {} (map (fn [[k v]] 
                                [k (bucket-by v :x d)])
                              (bucket-by ps :y d)))]
    (println (keys buckets)))
