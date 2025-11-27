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

(defn psel
  "parse selectional frame"
  [sel cg]
  (map 
   #(identity {:cat (:cat %) :head (get % :head false) :role (get % :role nil)}) 
   (map 
    #(let [head? (= % :_)] 
       (if (seqable? %) % {:cat (if head? cg %) :head head?})) 
    (map 
     #(if (sequential? %) {:cat (first %) :role (second %)} %) 
     sel)))
)

(defn label
  [so]
  (first (filter :head so))
)

(defn entry
  [lex cg sel]
  {:lex lex :cat cg :sel (psel sel cg)}
)

(defn mrg
  [& sos]
  (seq sos)
)

(defn lex
  [& entries]
  (map #(apply entry %) entries)
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (entry "eat" V [:_ N])
  (mrg 1 2)
)
