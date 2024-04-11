(defn analyze-data [data]
  (let [processed (mapv process-item data) ; Changed map to mapv for vector output
        filtered (filter relevant? processed)
        results (reduce combine-results {} processed)]
    results))
