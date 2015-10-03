(ns pdf2csv.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string]
            ))
;;
;; read word positions from csv file
;;
(defn parse-word-position [line]
  {:page-nb (Integer/parseInt (nth line 0))
   :line-nb (Integer/parseInt (nth line 1))
   :font-name (nth line 2)
   :font-size (Float/parseFloat (nth line 3))
   :space-width (Float/parseFloat (nth line 4))
   :x1 (+ 1.0 (Float/parseFloat (nth line 5)))
   :x2 (+ 1.0 (Float/parseFloat (nth line 6)))
   :y1 (Float/parseFloat (nth line 7)) 
   :y2 (Float/parseFloat (nth line 8))
   :word (nth line 9)})

(defn read-word-positions [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (csv/read-csv in-file :separator \;))))

(defn word-positions-from-file [filename]
;;  (doall (take 1000 (map parse-word-position (read-word-positions filename))))
  (doall (mapv parse-word-position (read-word-positions filename)))
  )
;;
;; sort the wordposition according page/line/x-position
;;
(defn group-by-x [words]
  (->> words
       (sort-by :x1)))

(defn group-by-line-x [words]
  (->> words
       (group-by :line-nb)
       (into (sorted-map-by <))
       (mapv second)
       (mapv group-by-x)
       )) ;;sequence of lines

(defn group-by-page-line-x [words]
  (->> words
       (group-by :page-nb)
       (into (sorted-map-by <))
       (mapv second) ;; sequence of pages
       (mapv group-by-line-x)
       ))
;;
;; convert the word positions to white spans (space between the words)
;;
(defn line-to-spans [words]
  (let [width 100000 ;; TODO should be the width of the page
        words1 (concat [{:x1 0
                         :x2 0
                         :y1 (:y1 (first words))
                         :y2 (:y2 (first words))
                         :line-nb (:line-nb (first words))
                         :page-nb (:page-nb (first words))
                         }] words)
        words2 (concat words [{:x1 width
                               :x2 width
                               :y1 (:y1 (first words))
                               :y2 (:y2 (first words))
                               :line-nb (:line-nb (first words))
                               :page-nb (:page-nb (first words))
                               }])
        ]
    (mapv (fn span [w1 w2]
           {:x1 (:x2 w1)
            :x2 (:x1 w2)
            :y1 (:y1 w1)
            :y2 (:y2 w1)
            :height (- (:y2 w1) (:y1 w1))
            :line-nb (:line-nb w1)
            :last-line-nb (:line-nb w1)
            :page-nb (:page-nb w1)}
           )
         words1
         words2)))

(defn page-to-spans [lines] 
  (mapv line-to-spans lines))

(defn pages-to-spans [pages]
  (mapv page-to-spans pages))
;;
;; span-span overlap (first span is on line N, second span is on line N+1)
;;
;; calculate the overlap of 2 white spans. the white spans must be on
;; 2 consecutive lines
;;
(defn overlap [span1 span2]
  "spans must overlap, otherwise the merged span will be garbage. the start line of the overlapping span is not changed, the last line is the line from the second span."
  (let [y1 (min (:y1 span1) (:y1 span2))
        y2 (max (:y2 span1) (:y2 span2))
        ]
    {:line-nb (:line-nb span1)
     :last-line-nb (:line-nb span2)
     :x1 (max (:x1 span1) (:x1 span2))
     :x2 (min (:x2 span1) (:x2 span2))
     :y1 y1 
     :y2 y2
     :height (- y2 y1)
     :page-nb (:page-nb span1)
     }))

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
     )))

(defn merge-span-span [span1 span2]
  "if 2 spans overlap, return the overlapping part, the overlapping part grows vertically (covers at least the 2 lines."
  (if (overlap? span1 span2)
    (overlap span1 span2)
    nil))
;;
;; span-line overlap (first span is on line N, other-spans are all the
;; spans of line N+1)
;;
;; span-line overlap can result in 0, 1 or more overlapping spans
;;
(defn overlap-span-line [span other-spans]
  (->> other-spans
       (mapv #(merge-span-span span %1))
       (filterv #(not (nil? %1)))
       ))
;;
;;
;;
(defn merge-span-line [span other-spans]
  (let [overlap-spans (overlap-span-line span other-spans)]
    (if (empty? overlap-spans)
      (vector span)
      overlap-spans)))

(defn same-line [line1 line2] 
  (= (:last-line-nb line1) (:last-line-nb line2)))

(defn spans-from-line [line-nb spans]
  (filterv #(same-line {:last-line-nb line-nb} %1) spans))

(defn spans-not-from-line [line-nb spans]
  (filterv #(not (same-line {:last-line-nb line-nb} %1)) spans))

(defn merge-lines [acc-spans line]
  (if (empty? line)
    acc-spans
    (let [next-line-nb (:last-line-nb (first line))
          curr-line-nb (dec next-line-nb)
          spans-prev-lines (spans-not-from-line curr-line-nb acc-spans)
          spans-last-line (spans-from-line curr-line-nb acc-spans)]
      (->> spans-last-line
           (mapv #(merge-span-line %1 line))
           (flatten)
           (concat spans-prev-lines)
           (into [])
           ))))
;;
;; 
;;
(defn spans-to-lines [spans]
  (->> spans
       (group-by :line-nb)
       (mapv second)))

(defn merge-white-spans-line [lines]
  (let [first-line (first lines)
        rest-lines (rest lines)]
    (reduce merge-lines first-line rest-lines)))

(defn merge-white-spans-lines [all-lines]
  (loop [lines all-lines acc []]
    (if (empty? lines)
      acc
      (recur (rest lines) (into [] (concat acc
                                          (vector (merge-white-spans-line lines))))))
    
    ))
;;
;; remove small spans
;;
(defn is-large-span? [min-height min-width span]
  (let [height (- (:last-line-nb span) (:line-nb span))
        width (- (:x2 span) (:x1 span))]
    (and
     (>= height min-height)
     (>= width min-width))))

(defn remove-small-spans-from-line [min-height min-width line]
  (filterv #(is-large-span? min-height min-width %) line))

(defn remove-small-spans-lines [min-height min-width lines]
  (mapv #(remove-small-spans-from-line min-height min-width %) lines)
  )
;;
;; group the span/stripe/column per height
;;
(defn group-line-by-height [line]
  (->> line
       (group-by :height)
       (map second)
       ))

(defn group-lines-by-height [lines]
  (map group-line-by-height lines))
;;
;; stripes to columns
;;
(defn stripes-to-columns [line]
  (let [sorted-line (sort-by :x1 line)]
    (map (fn [span1 span2]
           (let [y1 (min (:y1 span1) (:y1 span2))
                 y2 (min (:y2 span1) (:y2 span2))
                 ]
             {:x1 (:x2 span1)
              :x2 (:x1 span2)
              :y1 y1
              :y2 y2
              :page-nb (:page-nb span1)
              :height (- y2 y1)
              :line-nb (:line-nb span1)
              :last-line-nb (min (:last-line-nb span1)
                                 (:last-line-nb span2))
              }))
         sorted-line
         (rest sorted-line)
         )))

(defn stripes-to-columns-lines [lines]
  (map stripes-to-columns lines))
;;
;; count words per column
;;
(defn word-in-column? [column word]
  (if (and (= (:page-nb column) (:page-nb word))
           (<= (:y1 column) (:y1 word))
           (>= (:y2 column) (:y2 word))
           (<= (:x1 column) (:x1 word))
           (>= (:x2 column) (:x2 word)))
    true
    false)
  )

;;
;; TODO: only count 1 if there are n words of the same line
;;
(defn word-count-for-column [words column]
  (filter #(word-in-column? column %) words))
;;
;; column {:page-nb :x1 :x2 :y1 :y2}
;;
;; add words and word-count to each colum {:page-nb :x1 :x2 :y1 :y2 :word-count :words [...]}
;;
(defn word-count-line [words line]
  (let [page-words (flatten (filter #(= (:page-nb %1) (:page-nb (first  line))) (flatten words)))
        words-per-column (map #(word-count-for-column page-words %) line)
        ]
    (map #(merge %1 {:word-count (count %2) :words %2})
         line words-per-column)))

(defn word-count-lines [words lines]
  (map #(word-count-line words %1) lines))
;;
;; basic statistics based on number of words in the column
;;
(defn word-fillage [column]
  (let [lines (inc (- (:last-line-nb column) (:line-nb column)))
        word-count (:word-count column)]
    (if (> word-count 0)
      (/ word-count lines)
      0
      )))

(defn word-fillage-line [line]
  (map #(merge %1 {:word-fillage (word-fillage %1)}) line))

(defn word-fillage-lines [lines]
  (map #(word-fillage-line %) lines))
;;
;; remove columns that are too small
;;
(defn remove-min-height-line [min-height-line columns]
  (filter #(>= (inc (- (:last-line-nb %1) (:line-nb %1))) min-height-line) columns))

(defn remove-min-word-fillage [min-word-fillage columns]
  (filter #(>= (:word-fillage %1) min-word-fillage) columns))

(defn remove-small-cols-from-lines [min-height min-width min-word-fillage lines]
  (->> lines
       (map #(remove-min-height-line min-height %1))
       (map #(remove-min-word-fillage min-word-fillage %1))
       (filter #(>= (count %1) min-width))
       ))
;;
;; simple score: use size of the grid (lines * columns)
;;
(defn simple-score-line [line]
  (let [width (count line)
        min-height (if (> width 0)
                     (apply min (map #(- (:last-line-nb %) (:line-nb %)) line))
                     0
                     )
        ]
    {:simple-score (* min-height width)
     :columns line}
    ))
;;
;; choose the biggest grid per page 
;;
(defn simple-score-lines [lines]
  (->> lines
       (map simple-score-line)
       (sort-by :simple-score)
       (last)
       (:columns)))
;;
;; sort the word in each column according line number
;;
(defn sort-column-words-on-line [columns]
  (->> columns
       (map :words)
       (map #(group-by :line-nb %))
       ))
;;
;; build a grid of words (line / columns)
;;
;; For each column, get the words that belong to a given line
;;
;; Each column contains the list of word positions. Extract the words
;; that belong to a given line and concatenate the words
;;
(defn column-line-to-csv [column line-nb] ;;string
  (let [words (get column line-nb)]
    (->> words
         (map :word)
         (clojure.string/join " "))))

(defn columns-line-to-csv [columns line-nb] ;; list of string
  (map #(column-line-to-csv % line-nb) columns))

(defn columns-to-csv [columns]
  (let [line-min (apply min (keys (first  columns)))
        line-max (apply max (keys (first  columns)))]
    (for [line-nb (range line-min (inc line-max))]
      (columns-line-to-csv columns line-nb))))
;;
;; write pages to file
;;
(defn write-csv [out csv]
  (with-open [out-file (io/writer out)]
    (doall (map
            #(csv/write-csv out-file % :separator \tab)
            csv))))
;;
;; testing
;;
(defn group-spans-by-x1 [lines]
  (mapv #(sort-by :x1 %) lines)
  )
;;(def in "test/pdf2csv/simple-wordpositions.csv")
;;(def in "test/pdf2csv/jan.pdf-wordLinePositions.csv")
;;(def in "test/pdf2csv/CAAC2012.pdf-wordLinePositions.csv")
;;(def in "/home/pdeschacht/dev/pdf2txtpos/pdf2txtpos/target/ACI_2013_08_worldwide.info")
;;(def in "/home/pdeschacht/pdf/Seaport/01Jul14_daily_segment_report.info")
;;(def in "/Users/pauldeschacht/dev/csv-conf/1113MYTDYE.info")
(def in "resources/0514MYTDYEiPaxFrt.info")
(def in "resources/0514MYTDYEiPaxFrt.info")
;;(def in "resources/page2.info")
(def in "resources/goair15.info")
(def pos1 (word-positions-from-file in))
(def pos2 (group-by-page-line-x pos1))
(def spans (pages-to-spans pos2))
(def min-span-width 2.5) ;;TODO make this a bit SMALLER than the space width
(def spans* (mapv #(remove-small-spans-lines 0 min-span-width %) spans)) ;; remove narrow spans
(def stripes (mapv merge-white-spans-lines spans*))
(def stripes* (mapv group-spans-by-x1 stripes))
;;(def stripes* (mapv #(remove-small-spans-lines 0 min-span-width %) stripes**))
(def cols (mapv stripes-to-columns-lines stripes*))
(def cols-with-word-count (mapv #(word-count-lines pos2 %1) cols))
;;(def cols-with-words (word-count-lines pos2  cols))
(def cols-wc (mapv #(word-fillage-lines %1) cols-with-word-count))

(def min-height 3) ;; at least 3 lines
(def min-width 3)  ;; at least 3 columns per line
(def min-word-fillage (/ 1 2)) ;; at least 50% filled
;;(def min-word-fillage 0)
(def result (mapv #(remove-small-cols-from-lines min-height min-width min-word-fillage %1) cols-wc))
(def scored-lines (map simple-score-lines result))
(def grid (mapv sort-column-words-on-line scored-lines))
(def csv (mapv columns-to-csv grid))
(write-csv (str in ".csv") csv)
