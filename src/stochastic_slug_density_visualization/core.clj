(ns stochastic-slug-density-visualization.core)

(def generated-slugs
  ; The storage of slugs will be as a map, where the keys are ranges,
  ; e.g., 10, 100, 1000, 10000, etc., and the values are set of the slugs
  ; generated so far that are ≥ than the previous range and < the
  ; current range. Thus 10 will make to the single digit slugs, 100
  ; to the 2 digit slugs, etc.
  (ref {}))

(defn compute-slot-key
  [slug]
  (first (filter #(< slug %) (iterate #(* 10 %) 10))))

(defn add-new-slug-if-possible
  "Attempt to add a new slug to the
   store of generated slugs. If the attempt succeeds,
   return the new slug, otherwise (i.e., the slug collides,
   i.e., it already existed in the store) return nil."
  ([slug]
    (add-new-slug-if-possible slug (compute-slot-key slug)))
  ([slug slot-key]
    (dosync
      (when-not (contains? (@generated-slugs slot-key) slug)
        (alter generated-slugs update-in [slot-key] (comp set conj) slug)
        slug))))

(defn gen-slug
  "Generate a slug that is guaranteed to be unique"
  []
  (let [slot-keys (iterate #(* 10 %) 10)
        slugs (map rand-int slot-keys)]
    (some add-new-slug-if-possible slugs)))

(defn gen-slugs
  "Generate the given number of unique slugs"
  [num-slugs]
  (repeatedly num-slugs gen-slug))

(defn size-counts
  []
  (let [buckets (take-while #(<= % 100000) (iterate #(* 10 %) 10))
        bucket-counts (for [b buckets]
                        (count (get @generated-slugs b {})))]
    (clojure.string/join "," bucket-counts)))

(defn generate-visualization-data
  [num-slugs]
  (dosync
    (ref-set generated-slugs {}))
  (spit "target/data.csv" "Num slugs,<10,<100,<1K,<10K,<100K\n")
  (dotimes [n num-slugs]
    (gen-slug)
    (spit "target/data.csv" (str n "," (size-counts) "\n") :append true)))

(defn compute-slug-sizes
  []
  (dosync
    (ref-set generated-slugs {}))
  (map (comp #(int (java.lang.Math/ceil (java.lang.Math/log10 %))) last sort) 
       (repeatedly 100 #(gen-slugs 10))))