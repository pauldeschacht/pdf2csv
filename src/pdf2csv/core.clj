(ns pdf2csv.core
  (:require [clojure.data.csv :as csv]
           [clojure.java.io :as io])
  )

(def test-words [{:x1 10 :x2 15 :line-nb 1}
                 {:x1 17 :x2 20 :line-nb 1}
                 {:x1 23 :x2 27 :line-nb 1}
                 {:x1 40 :x2 56 :line-nb 1}])

;;
;; read word positions from csv file
;;
(defn parse-word-position [line]
  {:page-nb (Integer/parseInt (nth line 0))
   :line-nb (Integer/parseInt (nth line 1))
   :font-name (nth line 2)
   :font-size (Float/parseFloat (nth line 3))
   :space-width (Float/parseFloat (nth line 4))
   :x1 (Float/parseFloat (nth line 5))
   :x2 (Float/parseFloat (nth line 6))
   :y1 (Float/parseFloat (nth line 7))
   :y2 (Float/parseFloat (nth line 8))
   :word (nth line 9)})

(defn read-word-positions [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (csv/read-csv in-file :separator \;))))

(defn word-positions-from-file [filename]
  (doall (take 1000 (map parse-word-position (read-word-positions filename))))
  )
;;
;; sort the wordposition according page/line/x-position
;;
(defn group-by-x [words]
  (->> words
       (sort-by :x1))
  )

(defn group-by-line-x [words]
  (->> words
       (group-by :line-nb)
       (into (sorted-map-by <))
       (map second)
       (map group-by-x)
       )) ;;sequence of lines

(defn group-by-page-line-x [words]
  (->> words
       (group-by :page-nb)
       (into (sorted-map-by <))
       (map second) ;; sequence of pages
       (map group-by-line-x)
       ))
;;
;; convert the word positions to white spans
;;
(defn line-to-spans [words]
  (let [width 100000 ;; should be the width of the page
        words1 (concat [{:x1 0
                         :x2 0
                         :line-nb (:line-nb (first words))
                         :page-nb (:page-nb (first words))}] words)
        words2 (concat words [{:x1 width
                               :x2 width
                               :line-nb (:line-nb (first words))
                               :page-nb (:page-nb (first words))}])
        ]
    (map (fn span [w1 w2]
           {:x1 (:x2 w1)
            :x2 (:x1 w2)
            :line-nb (:line-nb w1)
            :last-line-nb (:line-nb w1)
            :page-nb (:page-nb w1)}
           )
         words1
         words2)))

(defn page-to-spans [lines] ;;page == lines
  (map line-to-spans lines))

(defn pages-to-spans [pages]
  (map page-to-spans pages))
;;
;; span-span overlap (first span is on line N, second span is on line N+1)
;;
;; calculate the overlap of 2 white spans. the white spans must be on
;; 2 consecutive lines
;;
(defn overlap [span1 span2]
  "spans must overlap, otherwise the merged span will be garbage. the start line of the overlapping span is not changed, the last line is the line from the second span."
  {:line-nb (:line-nb span1)
   :last-line-nb (:line-nb span2)
   :x1 (max (:x1 span1) (:x1 span2))
   :x2 (min (:x2 span1) (:x2 span2))}
  )

(defn overlap? [span1 span2]
  "check if 2 spans do overlap"
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

(defn merge-span-span [span1 span2]
  "if 2 spans overlap, return the overlapping part, the overlapping part grows vertically (covers at least the 2 lines."
  (if (overlap? span1 span2)
    (overlap span1 span2)
    nil
))
;;
;; span-line overlap (first span is on line N, other-spans are all the
;; spans of line N+1)
;;
;; span-line overlap can result in 0, 1 or more overlapping spans
;;
(defn overlap-span-line [span other-spans]
  (->> other-spans
       (map #(merge-span-span span %1))
       (filter #(not (nil? %1)))
       ))
;;
;;
;;
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
;;
;; helper function to complete the data
(defn init-spans [spans]
  (map #(merge {:last-line-nb (:line-nb %1)} %1) spans)
  )
;;
;; 
;;
(defn spans-to-lines [spans]
  (->> spans
       (group-by :line-nb)
       (map second)))

(def all-spans [{:line-nb 1 :x1 10 :x2 15} {:line-nb 1 :x1 20 :x2 30}
                {:line-nb 2 :x1 7  :x2 13}  {:line-nb 2 :x1 15 :x2 25}
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
;;
;; remove spans that are too small
;;
(defn remove-small-spans-from-line [min-width strips]
  (filter #(> (- (:x2 %1) (:x1 %1)) min-width) strips))

(defn remove-small-spans-from-lines [min-width lines]
  (map #(remove-small-spans-from-line min-width %1) lines))

(defn remove-short-spans-from-line [min-height strips]
  (filter #(> (:height %1) min-height) strips))

(defn remove-short-spans-from-lines [min-height lines]
  (->> lines
       (map #(remove-short-spans-from-line min-height %1))
       (filter #(not (empty? %)))
       ))
;;
;;
;;
(defn height-line [line]
  (map (fn [span]
          (merge {:height (inc (- (:last-line-nb span) (:line-nb span)))} span )) line))

(defn height-lines [lines]
  (map height-line lines))
;;
;; return a list of sequences. each sequence contains the stripes with
;; the same height.
;;
(defn group-line-by-height [line]
  (->> line
       (group-by :height)
       (map second)
       ))

(defn group-lines-by-height [lines]
  (map group-line-by-height lines))
;;
;; stripe to column (strips come from the same line)
;;
(defn stripes-to-columns [line]
  (map (fn [span1 span2]
         {:x1 (:x2 span1)
          :x2 (:x1 span2)
          :y1 (:line-nb span1)
          :y2 (min (:last-line-nb span1) (:last-line-nb span2))
          :page-nb (:page-nb span1)
          :height (min (:height span1) (:height span2))
          })
       line
       (rest line)
       ))

(defn stripes-to-columns-lines [lines]
  (map stripes-to-columns lines))
;;
;; count words per column
;;
(defn word-in-column? [column word]
  ;; (if (and (= (:page-nb column) (:page-nb column))
  ;;          (>= (:y1 column) (:line-nb word))
  ;;          (<= (:y2 column) (:line-nb word))
  ;;          (>= (:x1 column) (:x1 word))
  ;;          (<= (:x2 column) (:x2 word)))
  ;;   true
  ;;   false)
  (do
    (println column)
    (println word)
    true)
  )

(defn word-count-for-column [words column]
  (count
   (filter true?
           (map #(word-in-column?  column %) words))
   )  )

;;
;; column {:page-nb :x1 :x2 :y1 :y2}
;;
;; add word-count to each colum {:page-nb :x1 :x2 :y1 :y2 :word-count}
;;
;; add word-count to each column of the line
;;
(defn word-count-line [words line]
  (let [page-words (flatten (filter #(= (:page-nb %1) (:page-nb line)) (flatten words)))
        _ (println (str "Number of pge words " (count page-words)))
        word-counts (map #(word-count-for-column page-words %) line)
        ]
    (map #(merge %1 {:word-count %2}) line word-counts))

  )

(defn word-count-lines [words lines]
  (map #(word-count-line words %1) lines))

(defn word-fillage [column]
  (let [height (:height column)
        word-count (:word-count column)
        ]
    (if (> word-count 0)
      (/ word-count height)
      0
      )
    )
  
  
  )

(defn word-fillage-line [line]
  (map #(merge %1 {:word-fillage (word-fillage %1)}) line))

(defn word-fillage-lines [lines]
  (map #(word-fillage-line %) lines))

(defn process-words [words]
  (->> words
       (pages-to-spans)
       (map merge-white-spans-lines)
       (map #(remove-small-spans-from-lines 1.5 %1))
       (map height-lines)
       (map #(remove-short-spans-from-lines 4 %1))
       (map stripes-to-columns-lines)
       (map #(word-count-lines words %1))
       (map #(word-fillage-lines %1))
;       (map group-lines-by-height)
       ))

(defn process-file [filename]
  (->> (word-positions-from-file filename)
       (group-by-page-line-x)
       (process-words)
       ))

