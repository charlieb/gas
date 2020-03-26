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

(defn elastic-collision 
  "Collides the two particles and returns a pair particles with updated velocities"
  [p1 p2]
  (let [v2p (fn [p] {:x (:vx p) :y (:vy p)})
        v1 (v2p p1)
        v2 (v2p p2)
        v- (fn [v1 v2] {:x (- (:x v1) (:x v2)) :y (- (:y v1) (:y v2))})
        v* (fn [v s] {:x (* (:x v) s) :y (* (:y v) s)})
        vdiv (fn [v s] {:x (/ (:x v) s) :y (* (:y v) s)})
        dot (fn [p1 p2] (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))))

        v1' (v- v1  (v* (v- p1 p2)
                      (/ (dot (v- v1 v2) (v- p1 p2))
                         (dot (v- p1 p2) (v- p1 p2)))))

        v2' (v- v2 (v* (v- p2 p1)
                      (/ (dot (v- v2 v1) (v- p2 p1))
                         (dot (v- p2 p1) (v- p2 p1)))))

        vset (fn [p v] (assoc p :vx (:x v) :vy (:y v)))]
    (list (vset p1 v1') (vset p2 v2'))))

;;(defn collate-collistions [pairs]
;;  (reduce (fn [acc [p1 p2]] 
;;            (update acc p1 #(if (nil? %) {p2} (conj % p2))))))

(defn apply-v [p] (into p {:x (+ (:x p) (:vx p))
                           :y (+ (:y p) (:vy p))}))

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

;; ^^^^^^^^^^^ Above particles is a list
;; vvvvvvvvvvv Below particles is a hash-map with id as the key

(defn to-id-hash-map [ps] (apply hash-map (mapcat #(list (:id %) %) ps)))

(defn iterate-particle-sim [particles]
  (let [cols (collisions (vals particles))
        parts (->> cols
                   (mapcat #(apply elastic-collision %))
                   to-id-hash-map
                   (into particles)

                   vals
                   (map apply-v)
                   to-id-hash-map)]
    {:particles parts :collisions cols})) 

(defn basic-colliding []
  {1 {:id 1 :x 200.  :y 200. :vx 0. :vy 0.} 
   2 {:id 2 :x 241  :y 200. :vx -10. :vy 0.}})

(defn test-coll []
  (iterate-particle-sim (basic-colliding)))

(defn init-particles
  "main?"
  [n w h]
  (let [ps (basic-colliding)
        ;;ps (to-id-hash-map (mk-particles n w h D))
        cols (collisions (vals ps))]
    {:particles ps :collisions cols :stop false}))

;; ---------- Driver ---------------
(def WIDTH 500)
(def HEIGHT 500)

(defn update-particles [state] 
  (into state (iterate-particle-sim (:particles state))))

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
)

(defn start-sketch [state]
  (q/sketch
    :host "host"
    :size [WIDTH HEIGHT]
    :draw #(draw @state)
    :on-close (fn [] (q/save-frame "frame###.png") (swap! state #(assoc % :stop true))
  )))

(defn start [n]
  (let [state (atom (init-particles n WIDTH HEIGHT))]
    (async/thread (run-sim state))
    (start-sketch state)))

(defn start-sketch-sync [state]
  (q/sketch
    :host "host"
    :size [WIDTH HEIGHT]
    :setup (fn [] state)
    :draw #(draw %)
    :update #(update-particles %)
    :on-close (fn [_] (q/save-frame "frame###.png")) 
    :middleware [m/fun-mode]
  ))

(defn start-sync [n]
  (let [state (init-particles n WIDTH HEIGHT)]
    (start-sketch-sync state)))
