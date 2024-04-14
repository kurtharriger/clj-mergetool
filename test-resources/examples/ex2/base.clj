(ns test)
(defn analyze-data [data]
  (let [processed (map process-item data)
        results (reduce combine-results {} processed)]
    results))
