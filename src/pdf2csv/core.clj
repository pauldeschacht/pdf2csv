(ns pdf2csv.core)

(defn overlap [span1 span2] 
  {:line-nb (:line-nb span1)
   :last-line-nb (:line-nb span2)
   :x1 (max (:x1 span1) (:x1 span2))
   :x2 (min (:x2 span1) (:x2 span2))}
  )

(defn overlap? [span1 span2]
  (let [x11 (:x1 span1)
        x21 (:x2 span1)
        x12 (:x1 span2)
        x22 (:x2 span2)]
    (cond
     (<= x21 x12) false ;; span1 before span2
     (<= x22 x11) false ;; span2 after span1
     :else true
     )
    ))

;; if no overlap: return input span
;; if overlap: return the common parts, with line-nb 
(defn merge-span-span [span1 span2]
  (if (overlap? span1 span2)
    (overlap span1 span2)
    nil
))

;; returns a lists of spans, both from current line and next line
;; if no overlap: return input span
;; if overlap: return the common parts, with line-nb 
(defn overlap-span-line [span other-spans]
  (->> other-spans
       (map #(merge-span-span span %1))
       (filter #(not (nil? %1)))
       ))

(defn merge-span-line [span other-spans]
  (let [overlap-spans (overlap-span-line span other-spans)]
    (if (empty? overlap-spans)
      (vector span)
      overlap-spans))
  )

(defn same-line [line1 line2] 
  (= (:last-line-nb line1) (:last-line-nb line2))
)

(defn spans-from-line [line-nb spans]
  (filter #(same-line {:last-line-nb line-nb} %1) spans))

(defn spans-not-from-line [line-nb spans]
  (filter #(not (same-line {:last-line-nb line-nb} %1)) spans))

(defn merge-lines [acc-spans line]
  (if (empty? line)
  	acc-spans
  	(let [next-line-nb (:last-line-nb (first line))
              curr-line-nb (dec next-line-nb)
              spans-prev-lines (spans-not-from-line curr-line-nb acc-spans)
              spans-last-line (spans-from-line curr-line-nb acc-spans)]
          (->> spans-last-line
               (map #(merge-span-line %1 line))
               (flatten)
               (concat spans-prev-lines)
               ))))

(defn init-spans [spans]
  (map #(merge {:last-line-nb (:line-nb %1)} %1) spans)
  )

(defn spans-to-lines [spans]
  (->> spans
       (group-by :line-nb)
       (map second)))

(def all-spans [{:line-nb 1 :x1 10 :x2 15} {:line-nb 1 :x1 20 :x2 30}
                {:line-nb 2 :x1 7  :x2 9}  {:line-nb 2 :x1 15 :x2 25}
                {:line-nb 3 :x1 12 :x2 14} {:line-nb 3 :x1 21 :x2 32}])


(defn merge-white-spans-line [lines]
  (let [first-line (first lines)
        rest-lines (rest lines)]
    (reduce merge-lines first-line rest-lines)))

(defn merge-white-spans-lines [all-lines]
  (loop [lines all-lines acc []]
    (if (empty? lines)
      acc
      (recur (rest lines) (concat acc
                                  (vector (merge-white-spans-line lines)))))
    
    ))

(defn my-test []
  (let [spans (init-spans all-spans)
        lines (spans-to-lines spans)
        ]
    (merge-white-spans-lines lines)))
