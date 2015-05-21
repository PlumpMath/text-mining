(ns text-mining.core
  ;; import namespaces under aliases
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [opennlp.nlp :as onlp]
            ;; refer the stem var (function) only
            [stemmers.porter :refer [stem]]))

;; define a set of stopwords
(def stopwords #{"a", "about", "above", "across", "after", "afterwards", "again", "against", "ago", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves"})

;; construct a tokenize function from the parser model and bind as var "tokenize" with def
(def tokenize (onlp/make-tokenizer "parser-model/en-token.bin"))


;; define a function to open a file and create a word count
(defn file->word-count [file-name]
  (with-open [reader (io/reader file-name)]
    ;; ->> is a threading macro, threading each result through the following expression as
    ;; the last argument, creating a pipeline of data flow
    (->> reader
         (line-seq)
         (drop-while #(re-matches #"\S+: .+" %))
         (mapcat tokenize)
         (filter #(re-matches #"\w\w+" %))
         (map #(.toLowerCase %))
         (filter (comp not stopwords))
         (map stem)
         frequencies)))

;; test it
(file->word-count "20news-bydate-train/talk.religion.misc/84203")

;; similar to unix ls
(defn ls [dir-name]
  ;; -> is a threading macro passing each expression as the first argument
  (-> (io/file dir-name)
      .list
      seq))

(def word-counts
  ;; for list comprehension
  ;; for each folder (group) in the newsgroup data
  (for [group (ls "./20news-bydate-train/")
        ;; for each text file (post)
        post (ls (str "./20news-bydate-train/" group))]
    ;; create a vector with [group post] key (1st) and the word count as value (2nd)
    [[group post] (file->word-count (str "./20news-bydate-train/" group "/" post))]))

;; only to inspect the results:
;; extract the word counts and count the size of each frequency map (distinct words)
;; comp is functional composition, effectively (fn [a] (count (second a)))
(def Ns (map (comp count second) word-counts))
(nth word-counts 9)


;; create a joint frequency table
;; merge-with merges all maps and adds values if the key occurs multiple times
(def dictionary (apply merge-with + (map second word-counts)))

(count dictionary)

;; extract the 2000 most frequent words
(def word-vector (take 2000 (sort-by second > dictionary)))

;; check whether flavour is in the first 2000 words by collecting them in a set
((into #{} (map first word-vector)) "flavour")
(stem "flavour")
(stem "flavor")

;; all clojure data is literally printable and readable extendable data notation (edn)
;; which is more expressive than json (but backwards compatible)
(spit "word-vector.edn" (vec word-vector))


;; create index over 2000 entries and a hash-map to map words to index
(def word-index (reduce #(assoc %1 (first %2) (second %2))
                        {}
                        ;; use list destructuring on arguments
                        (map (fn [[w c] i] [w i]) word-vector (range 2000))))

;; now create all word vectors
(def word-vectors
  (->> word-counts
       ;; only the frequency hash-maps
       (map second)
       ;; now associate the positions with the correct word-index w
       ;; for every word w with its count c
       (map (fn [wc] (reduce (fn [v [w c]]
                              (if-let [wi (word-index w)]
                                (assoc v wi c)
                                v))
                            ;; initial vector of zeros
                            (vec (repeat 2000 0))
                            ;; the frequency hash-map of [word count] pairs
                            wc)))))

;; some playground
(count word-vectors)
(map (fn [[w c]] [w (word-index w) c]) (second (first word-counts)))

((second (first word-counts)) "reported")

;; clojure vectors are functions, get element 338
((first word-vectors) 338)

;; helper function to write json in one step (streams allow to write
;; incrementally if data is larger than memory, but we remove that here)
(defn write-json [base-dir name coll]
  (with-open [w (io/writer (str base-dir name))]
    (json/generate-stream coll w)))

(write-json "./" "dictionary.json" dictionary)
(write-json "./" "word_vector.json" word-vector)
(write-json "./" "word_vectors.json" word-vectors)
(write-json "./" "word_counts.json" word-counts)
