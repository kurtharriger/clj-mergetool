(ns test)

(defn analyze-data [data]
  (let [processed (map process-data data)
        filtered (filter relevant? processed)
        results (reduce combine-results {} filtered)]
    results))
