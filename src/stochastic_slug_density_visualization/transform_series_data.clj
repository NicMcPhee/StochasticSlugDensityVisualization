(ns stochastic-slug-density-visualization.transform-series-data
  (:require [clojure-csv.core :as csv]))

(def IN-FILE "data/row_series_data.csv")
(def OUT-FILE "html/slug_size_counts_rickshaw.json")

(defn read-data
  [file-name]
  (csv/parse-csv (slurp file-name)))

; {
;   name: "Midwest",
;   data: [ { x: -1893456000, y: 29888542 }, { x: -1577923200, y: 34019792 }, { x: -1262304000, y: 38594100 }, { x: -946771200, y: 40143332 }, { x: -631152000, y: 44460762 }, { x: -315619200, y: 51619139 }, { x: 0, y: 56571663 }, { x: 315532800, y: 58865670 }, { x: 631152000, y: 59668632 }, { x: 946684800, y: 64392776 }, { x: 1262304000, y: 66927001 } ],
;   color: palette.color()
; },

(defn generate-series-row-items
  [out-file-name data-items]
  (let [generate-item (fn [x y] (str "{ x: " x ", y: " y " }"))]
    (spit out-file-name
          (clojure.string/join ", " (map generate-item (iterate inc 1) data-items))
          :append true)))

(defn generate-series-row
  [out-file-name data-row]
  (spit out-file-name "{\n" :append true)
  (spit out-file-name (str "\t" (first data-row) ",\n") :append true)
  (spit out-file-name "\tdata: [ " :append true)
  (generate-series-row-items out-file-name (rest data-row))
  (spit out-file-name " ],\n" :append true)
  (spit out-file-name "\tcolor: palette.color()\n},\n" :append true)
  )

(defn convert-data
  [in-file-name out-file-name]
  (let [data (read-data in-file-name)]
    (map (partial generate-series-row out-file-name) (rest data))))