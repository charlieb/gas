(ns gas.core
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [quil.core :as q :include-macros true] ))

(def D 10.)
(def next_id -1)

(defn mk-particle [x y vx vy] (set! next_id (+ 1 next_id)) {:id next_id :x x :y y :vx vx :vy vy})

(defn mk-particles [n w h d]
  (repeatedly n #(mk-particle (rand w) (rand h) (rand d) (rand d))))

(defn bucket-by
  "Bucket the items of items into d sized buckets of values of f(item)" 
  [items f d]
  (reduce (fn [buckets item] (update buckets (* d (quot (f item) d)) #(cons item %)))
          {}
          items))

(defn dist [p1 p2]
  (math/sqrt (+ (math/expt (- (:x p1) (:x p2)) 2)
                (math/expt (- (:y p1) (:y p2)) 2))))

(defn prnt [x] (println  "++ " x) x)

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
  (let [v- (fn [v1 v2] {:vx (- (:vx v1) (:vx v2)) :vy (- (:vy v1) (:vy v2))})
        v* (fn [v s] {:vx (* (:vx v) s) :vy (* (:vy v) s)})
        vdiv (fn [v s] {:vx (/ (:vx v) s) :vy (* (:vy v) s)})
        p- (fn [v1 v2] {:x (- (:x v1) (:x v2)) :y (- (:y v1) (:y v2))})
        vpdot (fn [v p] (+ (* (:vx v) (:x p)) (* (:vy v) (:y p))))
        pdot (fn [p1 p2] (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))))
        p2v (fn [p] {:vx (:x p) :vy (:y p)})

        v1 (v- p1 (v* (vdiv (vpdot (v- p1 p2) (p- p1 p2))
                          (math/expt (pdot (p- p1 p2))))
                      (p- p1 p2)))

        v2 (v- p2 (v* (vdiv (vpdot (v- p2 p1) (p- p2 p1))
                          (math/expt (pdot (p- p2 p1))))
                      (p- p2 p1)))
        vset (fn [p v] (assoc p :vx (:vx v) :vy (:vy v)))]
    (list (vset p1 v1) (vset p2 v2))))

;;(defn collate-collistions [pairs]
;;  (reduce (fn [acc [p1 p2]] 
;;            (update acc p1 #(if (nil? %) {p2} (conj % p2))))))


(defn iterate-particle-sim [particles])

(defn init-particles
  "main?"
  [n w h]
  (let [ps (mk-particles n w h D)
        cols (collisions ps)]
    {:particles ps :collisions cols :stop false}))

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

;; ---------- Driver ---------------
(def WIDTH 500)
(def HEIGHT 500)

(defn update-particles [state] state)

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
  (doseq [p (:particles state)]
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

