(ns text-mining.core
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [opennlp.nlp :as onlp]
            [stemmers.porter :refer [stem]]))

(def stopwords #{"a", "about", "above", "across", "after", "afterwards", "again", "against", "ago", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves"})

(def tokenize (onlp/make-tokenizer "parser-model/en-token.bin"))

(defn file->word-count [file-name]
  (with-open [reader (io/reader file-name)]
    (->> reader
         (line-seq)
         (drop-while #(re-matches #"\S+: .+" %))
         (mapcat tokenize)
         (filter #(re-matches #"\w\w+" %))
         (map #(.toLowerCase %))
         (filter (comp not stopwords))
         (map stem)
         frequencies)))

(file->word-count "20news-bydate-train/talk.religion.misc/84203")

(defn ls [dir-name]
  (-> (io/file dir-name)
      .list
      seq))

(def word-counts
  (for [group (ls "./20news-bydate-train/")
        post (ls (str "./20news-bydate-train/" group))]
    [[group post] (file->word-count (str "./20news-bydate-train/" group "/" post))]))

(def Ns (map (comp count second) word-counts))

(nth word-counts 9)

(def dictionary (apply merge-with + (map second word-counts)))

(count dictionary)

(def word-vector (take 2000 (sort-by second > dictionary)))

((into #{} (map first word-vector)) "flavour")
(stem "flavour")
(stem "flavor")

(spit "word-vector.edn" (vec word-vector))


(def word-index (reduce #(assoc %1 (first %2) (second %2))
                        {}
                        (map (fn [[w c] i] [w i]) word-vector (range 2000))))

(def word-vectors
  (->> word-counts
       (map second)
       (map (fn [wc] (reduce (fn [v [w c]]
                              (if-let [wi (word-index w)]
                                (assoc v wi c)
                                v))
                            (vec (repeat 2000 0)) wc)))))

(count word-vectors)
(map (fn [[w c]] [w (word-index w) c]) (second (first word-counts)))

((second (first word-counts)) "reported")

((first word-vectors) 338)

(defn write-json [base-dir name coll]
  (with-open [w (io/writer (str base-dir name))]
    (json/generate-stream coll w)))

(write-json "./" "dictionary.json" dictionary)
(write-json "./" "word_vector.json" word-vector)
(write-json "./" "word_vectors.json" word-vectors)
(write-json "./" "word_counts.json" word-counts)
