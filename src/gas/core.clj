(ns gas.core
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m] ))

(def D 10.)
(def next_id -1)

(defn prnt [prefix x] (println prefix x) x)

(defn mk-particle [x y vx vy] 
  (alter-var-root (var next_id) #(+ 1 %))
  {:id next_id :x x :y y :vx vx :vy vy})

(defn mk-particles [n w h d]
  (repeatedly n #(mk-particle (rand w) (rand h) (- (* 0.5 d) (rand d)) (- (* 0.5 d) (rand d)))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (* d (quot (f item) d)) #(cons item %)))
          {}
          items))

(defn dist [p1 p2]
  (math/sqrt (+ (math/expt (- (:x p1) (:x p2)) 2)
                (math/expt (- (:y p1) (:y p2)) 2))))


(defn collide-particle-lists
  "Detect collisions between 'collide-parts' and 'concat collide-parts parts'.
  Return a list of pairs of colliding particles."
  [collide-parts parts]
  (let [within-D (fn [p other] (>= D (math/abs (- (:x other) (:x p)))))
        within-D-right (fn [p other] (>= D (- (:x other) (:x p))))]
    (loop [cols collide-parts
           other (drop-while #(< D (- (:x (first cols)) (:x %))) parts)
           pairs (list)]
      (if (empty? cols)
        pairs
        (recur 
          (rest cols)
          (drop-while #(< D (- (:x (second cols)) (:x %))) parts)
          (concat pairs
                  (filter (fn [[p1 p2]] (> D (dist p1 p2))) 
                          (map list 
                               (concat (take-while #(within-D (first cols) %) other)
                                       (take-while #(within-D-right (first cols) %) (rest cols)))
                               (repeat (first cols))))))))))


(defn collisions
  "Yields a list of pairs of colliding particles"
  [particles]
  (let [buckets (into {} (map (fn [[k v]] [k (sort-by :x v)])
                              (bucket-by particles :y D)))]
    (loop [bys (keys buckets)
           pairs (list)]
      (if (empty? bys)
        pairs
        (recur (rest bys)
               (concat pairs
                       (collide-particle-lists (buckets (first bys))
                                               (buckets (+ (first bys) D)))))))))

(defn v+ [v1 v2] {:x (+ (:x v1) (:x v2)) :y (+ (:y v1) (:y v2))})
(defn v- [v1 v2] {:x (- (:x v1) (:x v2)) :y (- (:y v1) (:y v2))})
(defn v* [v s] {:x (* (:x v) s) :y (* (:y v) s)})
(defn vdiv [v s] (if (zero? s) {:x 0 :y 0} {:x (/ (:x v) s) :y (/ (:y v) s)}))
(defn dot  [p1 p2] (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))))
(defn mag [v] (math/sqrt (+ (math/expt (:x v) 2) (math/expt (:y v) 2))))
(defn norm [v] (vdiv v (mag v)))
(defn v2p  [p] {:x (:vx p) :y (:vy p)})
(defn p2v  [p] {:vx (:x p) :vy (:y p)})

(defn elastic-collision 
  "Collides the two particles and returns a pair particles with updated velocities"
  [p1 p2]
  (let [v1 (v2p p1)
        v2 (v2p p2)

        v1' (v- v1  (v* (v- p1 p2)
                      (/ (dot (v- v1 v2) (v- p1 p2))
                         (dot (v- p1 p2) (v- p1 p2)))))

        v2' (v- v2 (v* (v- p2 p1)
                      (/ (dot (v- v2 v1) (v- p2 p1))
                         (dot (v- p2 p1) (v- p2 p1)))))

        vset (fn [p v] (assoc p :vx (:x v) :vy (:y v)))]
    (list (vset p1 v1') (vset p2 v2'))))

(defn apply-v [p] (into p {:x (+ (:x p) (:vx p))
                           :y (+ (:y p) (:vy p))}))

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
  (let [ps (sort-by :x (mk-particles n 500 500 D))
        naive (map #(sort-by :x %) (sort-by #(:x (first %)) (check-all-for-collisions ps)))
        buckt (map #(sort-by :x %) (sort-by #(:x (first %)) (collisions ps)))
        pair= (fn [[p11 p12] [p21 p22]] (or (and (= p11 p21) (= p12 p22))
                                            (and (= p12 p21) (= p11 p22))
                                            (and (= p11 p22) (= p12 p21))
                                            (and (= p12 p22) (= p11 p21))
                                            ))
        ]
    (and (= (count buckt) (count naive))
         (every? (map (fn [pb] (some (partial pair= pb) naive))) buckt))))

(defn total-velocity [particles]
  (reduce (fn [v p] (v+ v (v2p p))) {:x 0 :y 0} particles))

(defn total-position [particles]
  (reduce v+ {:x 0 :y 0} particles))

;; ^^^^^^^^^^^ Above particles is a list
;; vvvvvvvvvvv Below particles is a hash-map with id as the key

(defn to-id-hash-map [ps] (apply hash-map (mapcat #(list (:id %) %) ps)))


(defn basic-colliding []
  {1 {:id 1 :x 199.9  :y 199.9 :vx 0.1 :vy 0.} 
   2 {:id 2 :x 200.1  :y 200.1 :vx -5.1 :vy 0}
   3 {:id 3 :x 200.1  :y 199.9 :vx -5.1 :vy 0}
   })

(defn exclude "Returns hash-map of particles after moving to non-overlapping positions"
  [collisions]
  (reduce (fn [parts [p1 p2]]
            (let [dv (v- p1 p2)
                  dvnorm (norm dv)
                  m (mag dv)
                  ovl (- D m)
                  p1d (into p1 (v+ p1 (v* dvnorm (/ ovl 2.0))))
                  p2d (into p2 (v+ p2 (v* dvnorm (/ ovl -2.0))))
                  ]
              (assoc parts (:id p1) p1d
                           (:id p2) p2d)))
          {} (shuffle collisions)))


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
                        ((fn [p] (cond (< (:x p) 0) (assoc p :x 0 :vx (- (:vx p)))
                                       (> (:x p) WIDTH) (assoc p :x WIDTH :vx (- (:vx p)))
                                       :otherwise p)))
                        ((fn [p] (cond (< (:y p) 0) (assoc p :y 0 :vy (- (:vy p)))
                                       (> (:y p) HEIGHT) (assoc p :y HEIGHT :vy (- (:vy p)))
                                       :otherwise p))))])
                 particles)))

(defn accelerate-particles [particles]
  (let [p1 (->> particles
                vals
                (filter #(and (< (:x %) 50) (< (:y %) 50)))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {:x 0.125 :y 0}) (mag (v2p %))))))
                to-id-hash-map
                (into particles))
        p2 (->> p1
                vals
                (filter #(and (> (:x %) (- WIDTH 50)) (< (:y %) 50)))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {:x 0 :y 0.125}) (mag (v2p %))))))
                to-id-hash-map
                (into p1))
        p3 (->> p2
                vals
                (filter #(and (> (:x %) (- WIDTH 50)) (> (:y %) (- HEIGHT 50))))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {:x -0.125 :y 0}) (mag (v2p %))))))
                to-id-hash-map
                (into p2))
        p4 (->> p3
                vals
                (filter #(and (< (:x %) 50) (> (:y %) (- HEIGHT 50))))
                (map #(into % (p2v (v* (v+ (norm (v2p %)) {:x 0 :y -0.125}) (mag (v2p %))))))
                to-id-hash-map
                (into p3))
        ]
    p4))

(defn jitter [particles]
  (->> particles
       vals
       (map #(assoc %
                    :x (+ (:x %) (* 0.1 (- 1 (rand 0.5))))
                    :y (+ (:y %) (* 0.1 (- 1 (rand 0.5))))))
       to-id-hash-map))

(defn collide [collisions particles]
  (mapcat #(apply elastic-collision %))
  to-id-hash-map
  (into particles)
  )

(defn iterate-particle-sim [particles]
;  (println "V" (total-velocity (vals particles)))
  (println "P" (total-position (vals particles)))
  (let [cols (collisions (vals particles))
        parts (->> particles
                   (collide collisions)
                   (#(into % (exclude cols)))
                   
;                   jitter
;                   eliminate-overlaps
;                   accelerate-particles

                   vals
                   (map apply-v)
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
    (q/ellipse (:x p) (:y p) D D))
  (q/stroke 0 255 0)
  (doseq [[p1 p2] (:collisions state)]
    (q/line (:x p1) (:y p1) (:x p2) (:y p2)))

  ;;(q/stroke 0 0 255)
;;  (doseq [[v b] (:buckets state)]
;;    (q/stroke 0 v 255)
;;    (doseq [p b]
;;      (q/ellipse (:x p) (:y p) D D))))
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
