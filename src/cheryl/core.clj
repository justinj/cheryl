(ns cheryl.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def dates
  [{:month :may :day 5}
   {:month :may :day 6}
   {:month :may :day 9}
   {:month :june :day 4}
   {:month :june :day 6}
   {:month :july :day 7}
   {:month :july :day 8}
   {:month :august :day 4}
   {:month :august :day 5}
   {:month :august :day 7}])

(defn initial-knowledge [universe fn]
  (group-by fn universe))

(def initial-state
  {:antoine (initial-knowledge dates :month)
   :bill    (initial-knowledge dates :day)})

(defn deterministic? [[_ outcome]]
  (= 1 (count outcome)))

(defn extract-from-determined [projector possibilities]
  (set (map (fn [[_ [datum]]]
              (projector datum))
            possibilities)))
  
(defn assert-other-doesnt-know
  [state asserter asserter-fn other other-fn]
  (let [possibilities (other state)
        determined-possibilities (filter deterministic? possibilities)
        eliminated-asserter-infos (extract-from-determined
                                   asserter-fn determined-possibilities)
        new-asserter-data (remove (fn [[projection _]] (eliminated-asserter-infos
                                                        projection))
                                  (asserter state))
        new-other-data (map (fn [[projection data]]
                              [projection (remove #(eliminated-asserter-infos (asserter-fn %))
                                    data)])
                               (other state))
        new-other-data (remove (fn [[_ data]] (empty? data)) new-other-data)]
    (assoc state
           asserter new-asserter-data
           other new-other-data)))

(defn assert-i-know
  [state asserter asserter-fn other other-fn]
  (let [possibilities (asserter state)
        determined-possibilities (filter deterministic? possibilities)
        possible-asserter-infos (extract-from-determined
                                 asserter-fn determined-possibilities)
        new-other-data (map (fn [[projection data]]
                              [projection (filter #(possible-asserter-infos (asserter-fn %))
                                                  data)])
                            (other state))
        new-other-data (remove (fn [[_ data]] (empty? data)) new-other-data)]
    (assoc state
           asserter determined-possibilities
           other new-other-data)))

(-> initial-state
    (assert-other-doesnt-know :antoine :month :bill :day)
    (assert-i-know :bill :day :antoine :month)
    (assert-i-know :antoine :month :bill :day))
; => {:antoine ([:june ({:day 6, :month :june})]), :bill ([6 ({:day 6, :month :june})])}

(def ps-possible-pairs
  (mapcat (fn [i] (map (fn [j] [i j]) (range (inc i) (- 101 i)))) (range 2 99)))

(defn p [[a b]] (* a b))
(defn s [[a b]] (+ a b))

(def ps-initial-state
  {:p (initial-knowledge ps-possible-pairs p)
   :s (initial-knowledge ps-possible-pairs s)})


(-> ps-initial-state
    (assert-other-doesnt-know :s s :p p)
    (assert-i-know :p p :s s)
    (assert-i-know :s s :p p))
; => {:p ([52 ([4 13])]), :s ([17 ([4 13])])}
