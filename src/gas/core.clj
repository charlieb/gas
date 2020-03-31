(ns gas.core
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m] ))

(def D 10.)
(def next_id -1)

(defn prnt [prefix x] (println prefix x) x)

(defrecord Vec2 [x y])
(defn v+ [v1 v2] (Vec2. (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))
(defn v- [v1 v2] (Vec2. (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))
(defn v* [v s] (Vec2. (* (:x v) s) (* (:y v) s)))
(defn vdiv [v s] (if (zero? s) (Vec2. 0 0)  (Vec2. (/ (:x v) s) (/ (:y v) s))))
(defn dot  [p1 p2] (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))))
(defn mag [v] (math/sqrt (+ (math/expt (:x v) 2) (math/expt (:y v) 2))))
(defn norm [v] (vdiv v (mag v)))

(defn px [v] (:x (:p v)))
(defn py [v] (:y (:p v)))
(defn vx [v] (:x (:v v)))
(defn vy [v] (:y (:v v)))
(defn fx [v] (:x (:f v)))
(defn fy [v] (:y (:f v)))

(defn mk-particle [x y vx vy] 
  (alter-var-root (var next_id) #(+ 1 %))
  {:id next_id :p (Vec2. x y) :v (Vec2. vx vy) :f (Vec2. 0 0)})

(defn mk-particles [n w h d]
  (repeatedly n #(mk-particle (rand w) (rand h) (- (* 0.5 d) (rand d)) (- (* 0.5 d) (rand d)))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (* d (quot (f item) d)) #(cons item %)))
          {}
          items))

(defn dist [p1 p2]
  (math/sqrt (+ (math/expt (- (px p1) (px p2)) 2)
                (math/expt (- (py p1) (py p2)) 2))))


(defn collide-particle-lists
  "Detect collisions between 'collide-parts' and 'concat collide-parts parts'.
  Return a list of pairs of colliding particles."
  [collide-parts parts]
  (let [within-D (fn [p other] (>= D (math/abs (- (px other) (px p)))))
        within-D-right (fn [p other] (>= D (- (px other) (px p))))]
    (loop [cols collide-parts
           other (drop-while #(< D (- (px (first cols)) (px %))) parts)
           pairs (list)]
      (if (empty? cols)
        pairs
        (recur 
          (rest cols)
          (drop-while #(< D (- (px (second cols)) (px %))) parts)
          (concat pairs
                  (filter (fn [[p1 p2]] (> D (dist p1 p2))) 
                          (map list 
                               (concat (take-while #(within-D (first cols) %) other)
                                       (take-while #(within-D-right (first cols) %) (rest cols)))
                               (repeat (first cols))))))))))


(defn collisions
  "Yields a list of pairs of colliding particles"
  [particles]
  (let [buckets (into {} (map (fn [[k v]] [k (sort-by px v)])
                              (bucket-by particles py D)))]
    (loop [bys (keys buckets)
           pairs (list)]
      (if (empty? bys)
        pairs
        (recur (rest bys)
               (concat pairs
                       (collide-particle-lists (buckets (first bys))
                                               (buckets (+ (first bys) D)))))))))


(defn elastic-collision 
  "Collides the two particles and returns a pair particles with updated velocities"
  [part1 part2]
  (let [p1 (:p part1)
        p2 (:p part2)
        v1 (:v part1)
        v2 (:v part2)

        v1' (v- v1  (v* (v- p1 p2)
                      (/ (dot (v- v1 v2) (v- p1 p2))
                         (dot (v- p1 p2) (v- p1 p2)))))

        v2' (v- v2 (v* (v- p2 p1)
                      (/ (dot (v- v2 v1) (v- p2 p1))
                         (dot (v- p2 p1) (v- p2 p1)))))]
    (list (assoc part1 :v v1') (assoc part2 :v v2'))))

(defn apply-motion [p]
  (-> p
      (update :v #(v+ % (:f p)))
      (#(let [p %] (assoc p :p (v+ (:p p) (:v p)))))
      (assoc :f (Vec2. 0 0))))

(defn collate-collisions [collisions]
  (reduce (fn [acc [p1 p2]] (-> acc
                                (update p1 #(if (nil? %) #{p2} (conj % p2)))
                                (update p2 #(if (nil? %) #{p1} (conj % p1)))))
          {} collisions))

;; ---------- Validation -----------
(defn check-all-for-collisions
  "Check every particle against every other to allow validation of bucket approach"
  [ps]
  (loop [p (first ps)
         ps (rest ps)
         pairs (list)]
    (if (empty? ps)
      pairs
      (recur (first ps) (rest ps)
             (concat pairs
                     (map list
                          (filter (fn [p2] (> D (dist p p2))) ps)
                          (repeat p)))))))

(defn check-pairs [n]
  (let [ps (sort-by px (mk-particles n 500 500 D))
        naive (map #(sort-by px %) (sort-by #(px (first %)) (check-all-for-collisions ps)))
        buckt (map #(sort-by px %) (sort-by #(px (first %)) (collisions ps)))
        pair= (fn [[p11 p12] [p21 p22]] (or (and (= p11 p21) (= p12 p22))
                                            (and (= p12 p21) (= p11 p22))
                                            (and (= p11 p22) (= p12 p21))
                                            (and (= p12 p22) (= p11 p21))
                                            ))
        ]
    (and (= (count buckt) (count naive))
         (every? (map (fn [pb] (some (partial pair= pb) naive))) buckt))))

(defn total-velocity [particles]
  (reduce v+ (Vec2. 0 0) (map :v particles)))

(defn total-position [particles]
  (reduce v+ (Vec2. 0 0) (map :p particles)))

;; ^^^^^^^^^^^ Above particles is a list
;; vvvvvvvvvvv Below particles is a hash-map with id as the key

(defn to-id-hash-map [ps] (apply hash-map (mapcat #(list (:id %) %) ps)))


(defn basic-colliding []
  {1 {:id 1 :p (Vec2. 200 215) :v (Vec2. 0 -0.5) :f (Vec2. 0 0)}
   2 {:id 2 :p (Vec2. 205 200) :v (Vec2. -0.1 0) :f (Vec2. 0 0)}
   3 {:id 3 :p (Vec2. 220 201) :v (Vec2. 0 0) :f (Vec2. 0 0)}
   })


(defn exclude "Returns hash-map of particles after moving to non-overlapping positions"
  [collisions]
  (reduce (fn [parts [p1 p2]]
            (let [dv (v- (:p p1) (:p p2))
                  dvnorm (norm dv)
                  m (mag dv)
                  ovl (- D m)
                  p1d (assoc p1 :p (v+ (:p p1) (v* dvnorm (/ ovl 2.0))))
                  p2d (assoc p2 :p (v+ (:p p2) (v* dvnorm (/ ovl -2.0))))
                  ]
              (assoc parts (:id p1) p1d
                           (:id p2) p2d)))
          {} collisions))


(defn eliminate-overlaps [particles]
  (loop [ps particles
         i 0]
    (let [cols (collisions (vals ps))]
      (println (count cols))
      (if (or (>= i 1) (zero? (count cols)))
        ps
        (recur (into ps (exclude cols))
               (inc i))))))

; TODO move these back down to DRIVER section
(def WIDTH 500)
(def HEIGHT 500)
(defn keep-on-screen [particles]
  (apply hash-map
         (mapcat (fn [[id p]]
                   [id 
                    (-> p
                        ((fn [p] (cond (< (px p) 0) (assoc-in p [:v :x] (math/abs (vx p)))
                                       (> (px p) WIDTH) (assoc-in p [:v :x] (- (math/abs (vx p))))
                                       :otherwise p)))
                        ((fn [p] (cond (< (py p) 0) (assoc-in p [:v :y] (math/abs (vy p)))
                                       (> (py p) HEIGHT) (assoc-in p [:v :y] (- (math/abs (vy p))))
                                       :otherwise p))))])
                 particles)))

(defn accelerate-particles [particles]
  (let [p1 (->> particles
                vals
                (filter #(and (< (px %) 50) (< (py %) 50)))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {px 0.125 py 0}) (mag (v2p %))))))
                to-id-hash-map
                (into particles))
        p2 (->> p1
                vals
                (filter #(and (> (px %) (- WIDTH 50)) (< (py %) 50)))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {px 0 py 0.125}) (mag (v2p %))))))
                to-id-hash-map
                (into p1))
        p3 (->> p2
                vals
                (filter #(and (> (px %) (- WIDTH 50)) (> (py %) (- HEIGHT 50))))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {px -0.125 py 0}) (mag (v2p %))))))
                to-id-hash-map
                (into p2))
        p4 (->> p3
                vals
                (filter #(and (< (px %) 50) (> (py %) (- HEIGHT 50))))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {px 0 py -0.125}) (mag (v2p %))))))
                to-id-hash-map
                (into p3))
        ]
    p4))

(defn jitter [particles]
  (->> particles
       vals
       (map #(assoc %
                    px (+ (px %) (* 0.1 (- 1 (rand 0.5))))
                    py (+ (py %) (* 0.1 (- 1 (rand 0.5))))))
       to-id-hash-map))

(defn pressure [collisions particles]
  (let [pressures (apply hash-map (mapcat (fn [[p cs]] (list (:id  p) (count cs)))
                                 (collate-collisions collisions)))]
    (reduce (fn [parts [p1 p2]]
              ;(println p1 p2)
              (let [p1 (parts (:id p1))
                    p2 (parts (:id p2))
                    dv (v- (:p p1) (:p p2))
                    dvnorm (norm dv)
                    _ (println dvnorm)
                    p1d (assoc p1 :f (v+ (:f p1) (v* dvnorm (pressures p2))))
                    p2d (assoc p2 :f (v+ (:f p2) (v* dvnorm (- (pressures p1)))))]
                (assoc parts 
                       (:id p1) p1d
                       (:id p2) p2d)))
            particles
            collisions)))

(defn collide [collisions particles]
  (->> collisions
       (mapcat #(apply elastic-collision %))
       to-id-hash-map
       (into particles)))

(defn iterate-particle-sim [particles]
;  (println "V" (total-velocity (vals particles)))
;  (println "P" (total-position (vals particles)))
  ;(doseq [p particles] (println p))
  (let [cols (shuffle (collisions (vals particles)))
        parts (->> particles
;                   (collide cols)
                   (pressure cols)
;
;                   (#(into % (exclude cols)))
                   
;                   jitter
;                   eliminate-overlaps
;                   accelerate-particles

                   vals
                   (map apply-motion)
                   to-id-hash-map
                   keep-on-screen
                   )]
    {:particles parts :collisions cols})) 

(defn init-particles
  "main?"
  [n w h]
  (let [;ps (basic-colliding)
        ps (to-id-hash-map (mk-particles n w h (/ D 5.)))
        ps2 (eliminate-overlaps ps)
        cols (collisions (vals ps2))]
    {:particles ps2 :collisions cols :stop false}))

;; ---------- Driver ---------------

(defn update-particles [state] 
  (into state (iterate-particle-sim (:particles state))))

(defn run-sim [state] 
  (loop [] 
    (let [st @state]
    (when (not (:stop st))
      ;(print ".")
      (swap! state update-particles)
      (recur)))))

;; ---------- QUIL Viz -------------


(defn draw [state]
  (q/background 0)
  (q/no-fill)
  (q/stroke 0 0 255)
  (doseq [p (vals (:particles state))]
    (q/ellipse (px p) (py p) D D))
  (q/stroke 0 255 0)
  (doseq [[p1 p2] (:collisions state)]
    (q/line (px p1) (py p1) (px p2) (py p2)))

  ;;(q/stroke 0 0 255)
;;  (doseq [[v b] (:buckets state)]
;;    (q/stroke 0 v 255)
;;    (doseq [p b]
;;      (q/ellipse (px p) (py p) D D))))
(q/save-frame "frame#####.png")
)


(defn start-sketch [state]
  (q/sketch
    :host "host"
    :size [WIDTH HEIGHT]
    :draw #(draw @state)
    :on-close (fn [] (swap! state #(assoc % :stop true))
  )))

(defn start [n]
  (let [state (atom (init-particles n WIDTH HEIGHT))]
    (async/thread (run-sim state))
    (start-sketch state)))

; ------------ Syncronous --------------
(defn setup []
  (q/frame-rate 30))

(defn start-sketch-sync [state]
  (q/sketch
    :host "host"
    :size [WIDTH HEIGHT]
    :setup (fn [] (setup) state)
    :draw #(draw %)
    :update #(update-particles %)
;    :on-close (fn [_] (q/save-frame "frame###.png")) 
    :middleware [m/fun-mode]
  ))

(defn start-sync [n]
  (let [state (init-particles n WIDTH HEIGHT)]
    (start-sketch-sync state)))
