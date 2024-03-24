(defn analyze-data [data]
<<<<<<< HEAD
  (let [processed (map process-item data)
        filtered (filter relevant? processed)
        results (reduce combine-results {} filtered)]
||||||| 23fa1a2
  (let [processed (map process-item data)
        results (reduce combine-results {} processed)]
=======
  (let [processed (mapv process-item data) ; Changed map to mapv for vector output
        results (reduce combine-results {} processed)]
>>>>>>> right
    results))
