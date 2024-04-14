(ns test)
(defn analyze-data [data]
  (let [processed (map process-data data)
        results (reduce combine-results {} processed)]
    results))
