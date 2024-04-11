(defn analyze-data [data]
  (let [processed (mapv process-item data) ; Changed map to mapv for vector output
        results (reduce combine-results {} processed)]
    results))
