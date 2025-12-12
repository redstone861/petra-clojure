(ns petra.syntactic
  (:gen-class))

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
                   (let [sing (psel-single (if (seq? x) (eval (first x)) x))] 
                     (assoc sing :opt (or (seq? x) false))))]
    (apply vector (map make-map sels)))
)

(defn psel [sels cg]
  (let [parsed (tag-opt sels)]
    (mapv #(let [head? (= (:cat %) :_)] (merge % (if head? {:cat cg} {}) {:head head?}))
          parsed))
)

(defn get-head [cso]
  "return the (c?)so that heads this cso.
   the argument must be the RESULT OF MERGE (mrg)."
  (first (filter :head (:so cso)))
)

(defn transfer-meta
  "modify (and return) the so such that all necessary information 
  is transferred from the selectional stack, e.g., role."
  [so sel]
  (assoc so :role (get sel :role nil) :head (get sel :head false))
)

(defn label-from-head
  "given the head so (a map), return a map containing all of the data
  that will be reflected in the merged cso (the mother)."
  [head]
  {:cat (:cat head)
   :role (:role head)}
)

(defn mrg
  [sos head l r]
  {:pre [(seqable? sos)
         (seqable? l)
         (seqable? r)]}
  (let [sel (concat l [{:head true}] r)
        merged-raw sos
        ;_1 (println "merging with sel " sel ", merged-raw " merged-raw)
        merged (mapv transfer-meta merged-raw sel)]
    (assoc (label-from-head head) :so merged))
)

; set as head: (assoc-in (vec lexed) [# :head] true)

(defn eval-sel
  "evaluate whether the syntactic object's head has its sel met. the head must be marked with :head.
  I THINK I REWROTE THIS ELSEWHERE? DONT USE"
  [so]
  (let [sel (:sel (get-head so))]
    (print sel)
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


; TODO: some lexical items should be permitted to be multi-word, the parser shouldn't have to care about this. e.g., "put on", "look at". lex these (greedily?) as single items.
; also works for: "go north" (should equal same lexical item as "n") "go through" (="enter"?)...
(defn pre-lexer
  [str-in lexicon]
  (clojure.string/split str-in #"\s+")
)

(defn lexer
  [str-in lx]
  (map 
   #(first (filter (fn [ent] (= % (:lex ent))) lx))
   (pre-lexer str-in lx)
   )
)

(defn atomic? 
  "returns true if the so is atomic (a lexical item)."
  [so]
  (and (map? so) (contains? so :sel))
)

(defn unary-pass
  "make all systematically unary atomic nodes CSOs."
  [sos]
  {:pre [(seqable? sos)]}
  (map
   (fn [so]
     (let [sel (:sel so)]
       (if (= 1 (count sel))
         (mrg [so] so [] [])
         so)))
   sos)
)

(defn build-candidate-windows
  "Given a selectional frame `sel` (vector of maps),
   returns a list of candidate [l r] windows (l left deps, r right deps),
   sorted by descending total width."
  [sel]
  (if-not sel
    [] ; this element did not have sel (i.e., it is a cso)
    (let [vsel (vec sel)
          head-idx (.indexOf vsel (first (filter :head vsel)))
          left  (subvec vsel 0 head-idx)
          right (subvec vsel (inc head-idx))
          ;; Given a side, produce all inclusion patterns respecting order
          ;; but only optional items may be dropped.
          choose-side
          (fn [side]
            (letfn [(step [items]
                      (if (empty? items)
                        [[]]
                        (let [{:keys [opt] :as itm} (first items)
                              more (rest items)
                              keep (map #(cons itm %) (step more))
                              drop (if opt (step more) [])]
                          (concat keep drop))))]
              (step side)))
          lefts  (choose-side left)
          rights (choose-side right)
          ;; Combine left and right patterns
          windows (for [l lefts
                        r rights]
                    [(vec l) (vec r)])]
      ;; Larger windows first
      (sort-by (fn [[l r]] (- (+ (count l) (count r)))) windows))))

(defn satisfies-selection?
  [sel-entry so]
  (= (:cat sel-entry) (:cat so))
)

(defn vec-equal? [f v1 v2]
  (and (= (count v1) (count v2))             ;; same length
       (every? true?                          ;; all elements pass f
               (map f v1 v2))))

(defn pair-equal? [f l1 l2]
  (and (= (count l1) (count l2))
       (every? true?
               (mapv (partial vec-equal? f) l1 l2))))

(defn matches-window?
  "Tests whether subsequence `window` matches the pattern window.
   `l` = sel left of head, `r` = right sel"
  [window l r]
  (let [;; Reduce patterns to the specific chosen l/r size
        match-left (take (count l) window)
        match-right (take-last (count r) window)]
    (pair-equal? satisfies-selection? [match-left match-right] [l r]))
)

(defn greedy-sel-merge
  "Given a seq `xs` of lexical items, find the indices whose selectional
   frame matches a window in the surrounding context, using the largest
   possible window first, falling back to smaller ones.
   Replaces the entire matched window with (mrg window), and returns a list
   of all possible next workspace states (single merge) from this process.
   (i.e., returns a list of lists)"
  [xs]
  (let [v (vec xs)
        n (count v)]
    (loop [i 0
           newvecs (list)]
      (if (>= i n)
        ;; no match found -> return original sequence (as same type as input)
        newvecs
        (let [sel  (:sel (v i))
              wins (build-candidate-windows sel)
              ;; find first candidate window [l r] that matches
              found (some (fn [[l r]]
                            (let [L (- i (count l))
                                  R (+ i (count r))]
                              (when (and (>= L 0)
                                         (< R n)
                                         (= (inc (- R L)) (count sel)) ; window size equals sel size
                                         (matches-window? (subvec v L (inc R)) l r))
                                ;; return useful info for splicing
                                [L R l r])))
                          wins)]
          (if found
            (let [[L R l r] found
                  window (subvec v L (inc R))
                  newv (concat (subvec v 0 L)
                               [(mrg window (v i) l r)]
                               (subvec v (inc R) n))]
              ;(println "found" found ", for elem " (v i) ": " newv)
              (recur (inc i) (conj newvecs newv)))
            ;; no window matched at this index -> advance to next index
            (recur (inc i) newvecs)))))))

(defn contains-submap? 
  [m sub]
  (every? (fn [[k v]] (= (m k ::not-found) v)) sub)
)

(defn highest-matching
  "find the highest descendant (or self) of the cso `root` that matches
  the specification `spec`, a map, containing key,value pairs to be matched."
  [root spec]
  (if (atomic? root) 
    nil
    (if (contains-submap? root spec) 
      root 
      (first (filter #(contains-submap? % spec) (:so root))))) ; todo
)

(def lex-a (lex 
              ["eat" V [:_ '([N :DO])]] 
              ["apple" N ['(D) '(A) :_]]
              ["the" D [:_]]
              ["red" A [:_]]
              ["take" V [:_ [N :DO]]]))

(def lexed (lexer "eat the red apple" lex-a))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;(println "Hello, World!")
  ;(println (psel [:_ :A '([:B :DO])] :L))
  ;(greedy-sel-merge (unary-pass lexed))
  ;(entry "eat" V [:_ N])
  ;(mrg 1 2)
)
