(ns petra.core
  (:gen-class))

(defn merg
  [a b]
  (list a b))

(def V :V)
(def N :N)
(def A :A)
(def D :D)
(def P :P)

(defn psel-single [x]
  (let [bmap 
        (if (map? x) 
          x
          (if (vector? x)
            {:cat (first x) :role (second x)}
            {:cat x}))]
    (merge {:cat nil :role nil} bmap))
)

(defn tag-opt [sels]
  (let [make-map (fn [x]
                   (let [sing (psel-single (if (seq? x) (first x) x))] 
                     (assoc sing :opt (or (seq? x) false))))]
    (apply vector (map make-map sels)))
)

(defn psel [sels cg]
  (let [parsed (tag-opt sels)]
    (mapv #(let [head? (= (:cat %) :_)] (merge % (if head? {:cat cg} {}) {:head head?}))
          parsed))
)

(defn get-head [so]
  (first (filter :head so))
)

(defn label
  [so]
  (:cat (get-head so))
)

(defn mrg
  [& sos]
  (seq sos)
)

(defn sel-sat?
  [so]
  (let [sel (:sel (get-head so))]
    (mapv #(= (:cat %1) (:cat %2)) sel so))
)

(defn entry
  [lx cg sel]
  {:lex lx :cat cg :sel (psel sel cg)}
)

(defn lex 
  [& entries] 
  (map #(apply entry %) entries) 
)

(defn lexer
  [strin lx]
  (map 
   #(first (filter (fn [ent] (= % (:lex ent))) lx))
   strin
   )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (psel [:_ :A '([:B :DO])] :L))
  (entry "eat" V [:_ N])
  (mrg 1 2)
)
