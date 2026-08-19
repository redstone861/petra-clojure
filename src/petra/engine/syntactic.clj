(ns petra.engine.syntactic
  (:require [clojure.string])
  (:gen-class))

(def V :V)
(def N :N)
(def A :A)
(def D :D)
(def P :P)

(defn psel-single
  "one slot of a selectional frame. Written as

     N                     a category
     [N :DO]               category + theta-role
     [P \"in\"]             category + the LEXEME that must head the filler
     [N :DO #{:held}]      category + role + PRAGMATIC ASSERTIONS
     [P :GOAL \"in\"]       any combination
     {...}                 the parsed form, for anything exotic

  Roles are keywords, head lexemes are strings and assertion sets are sets, so
  none of the three needs distinguishing by position.

  Assertions are preferences, not constraints: they break a tie between candidate
  referents and never reject a lone one. See petra.engine.parser.

  Head-to-head selection -- the string form -- is what lets two homophonous heads
  be different verbs: a `put` selecting a P headed by \"in\" and a `put` selecting
  one headed by \"on\" are distinct lexical items that merely sound alike. Roles
  are keywords and lexemes are strings, so the two never need distinguishing by
  position."
  [x]
  (let [bmap (cond
               (map? x) x
               (vector? x) (let [[cat & more] x]
                             (merge {:cat cat}
                                    (when-let [r (first (filter keyword? more))] {:role r})
                                    (when-let [l (first (filter string? more))] {:head-lex l})
                                    (when-let [a (first (filter set? more))] {:asserts a})))
               :else {:cat x})]
    (merge {:cat nil :role nil :head-lex nil :asserts nil} bmap))
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
  is transferred from the selectional stack, e.g., role.

  Pragmatic assertions travel by exactly the same route as roles: the slot that
  licensed a filler stamps them onto it, so whoever later resolves that phrase can
  read off what the verb expects of it."
  [so sel]
  (assoc so
         :role (get sel :role nil)
         :head (get sel :head false)
         :asserts (get sel :asserts nil))
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

(def DIR :DIR)                                              ; a direction, per design/lexer-parser.txt

;; A PREDICATIVE preposition -- "the cup [on the table]", a reduced relative
;; saying where the cup is. Distinct in category from the argument P a verb
;; selects ("put it [on the shelf]"), because in this formalism category is what
;; selection sees: two things that must be selected differently have to be
;; different categories. Both are spelled "on"; they are separate lexical items.
(def PRED :PRED)

(defn entry
  "a lexical item. `extra` is merged in, and is how a word carries what the engine
  needs from it: which verb keyword it names, which direction it is, which objects
  it could denote."
  [lx cg sel & [extra]]
  (merge {:lex lx :cat cg :sel (psel sel cg)} extra)
)

(defn lex 
  [& entries] 
  (map #(apply entry %) entries) 
)


(defn- lexeme-index
  "lexeme string -> vector of entries, and the longest lexeme in words."
  [lx]
  [(group-by :lex lx)
   (reduce max 1 (map #(count (clojure.string/split (:lex %) #"\s+")) lx))])

(defn pre-lexer
  "split input into lexemes, matching the LONGEST multi-word entry at each point.
  So \"pick up\" and \"look at\" are single items and the parser never sees the
  seam -- which is also how design/lexer-parser.txt resolves separable verbs
  without modelling movement."
  [str-in lexicon]
  (let [[index longest] (lexeme-index lexicon)
        words (clojure.string/split (clojure.string/lower-case (clojure.string/trim str-in)) #"\s+")]
    (loop [ws words, out []]
      (if (empty? ws)
        out
        (let [take-n (fn [n] (clojure.string/join " " (take n ws)))
              n (or (first (filter #(contains? index (take-n %))
                                   (range (min longest (count ws)) 0 -1)))
                    1)]
          (recur (drop n ws) (conj out (take-n n))))))))

(defn lexer
  "input -> a vector of CANDIDATE vectors, one per lexeme. A lexeme with no entry
  yields an empty vector, which is how the caller reports an unknown word."
  [str-in lx]
  (let [[index _] (lexeme-index lx)]
    (mapv (fn [w] (vec (get index w []))) (pre-lexer str-in lx))))

(defn atomic? 
  "returns true if the so is atomic (a lexical item)."
  [so]
  (and (map? so) (contains? so :sel))
)

(defn all-sos
  "every syntactic object in the tree, root first."
  [root]
  (tree-seq (complement atomic?) :so root))

(defn head-leaf
  "the lexical item that ultimately heads this SO -- follow :head all the way down.
  What head-to-head selection is checked against."
  [so]
  (if (atomic? so)
    so
    (some-> (get-head so) head-leaf)))

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
          ;; TODO: a frame with no :head gives head-idx nil, and the subvec below
          ;; then throws something obscure. should be a named error naming the
          ;; offending lexical entry.
          head-idx (first (keep-indexed (fn [i x] (when (:head x) i)) vsel))
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
  "does `so` fill this slot? Category always; and when the slot names a lexeme,
  the thing heading `so` must be that word."
  [sel-entry so]
  (and (= (:cat sel-entry) (:cat so))
       (or (nil? (:head-lex sel-entry))
           (= (:head-lex sel-entry) (:lex (head-leaf so)))))
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
        match-right (take-last (count r) window)
        ;; NB the order: vec-equal? calls (f actual-so sel-entry). That was
        ;; invisible while selection only compared :cat, which is symmetric --
        ;; head-to-head selection is not, so be explicit.
        fills? (fn [so sel-entry] (satisfies-selection? sel-entry so))]
    (pair-equal? fills? [match-left match-right] [l r]))
)

(defn greedy-sel-merge
  "Given a seq `xs` of lexical items, find the indices whose selectional
   frame matches a window in the surrounding context.
   Replaces the entire matched window with (mrg window), and returns a list
   of all possible next workspace states (single merge) from this process.
   (i.e., returns a list of lists)

   NOT greedy any more, despite the name. Taking only the largest matching window
   per index silently lost every reading where an optional slot goes unfilled --
   with nouns able to take a PP modifier, \"put the cup on the shelf\" had ZERO
   parses, because the noun always swallowed the PP and left the verb's own P slot
   starved. Windows are still tried largest-first, so big constituents are found
   early, but every match now yields a successor and the search decides."
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
              ;; EVERY candidate window that matches, not just the widest
              found (keep (fn [[l r]]
                            (let [L (- i (count l))
                                  R (+ i (count r))]
                              ;; NB: no "window size = (count sel)" check. sel counts
                              ;; the optionals too, so requiring it made every
                              ;; optional-dropping window unreachable -- "eat the
                              ;; apple" had zero successors. The window is exactly
                              ;; (count l) + 1 + (count r) by construction.
                              (when (and (>= L 0)
                                         (< R n)
                                         (pos? (+ (count l) (count r))) ; a bare head is unary-pass's job
                                         (matches-window? (subvec v L (inc R)) l r))
                                ;; return useful info for splicing
                                [L R l r])))
                          wins)]
          (recur (inc i)
                 (into newvecs
                       (map (fn [[L R l r]]
                              (concat (subvec v 0 L)
                                      [(mrg (subvec v L (inc R)) (v i) l r)]
                                      (subvec v (inc R) n)))
                            found))))))))

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


;; ---------------------------------------------------------------------------
;; the search
;; ---------------------------------------------------------------------------
;; greedy-sel-merge gives every ONE-merge successor of a workspace. Deriving a
;; sentence is a search over those: keep merging until a single SO remains.
;; Breadth-first with a seen-set, because the same workspace is reachable by
;; different merge orders and there is no point exploring it twice.

(def ^:const default-search-limit 20000)

(defn derive-all
  "every complete derivation of `workspace` -- the states that collapsed to one
  syntactic object. Returns a vector, possibly empty (no parse) or with more than
  one entry (structurally ambiguous)."
  [workspace & {:keys [limit] :or {limit default-search-limit}}]
  (loop [frontier [(vec (unary-pass workspace))]
         seen     #{}
         done     []
         budget   limit]
    (cond
      (or (empty? frontier) (not (pos? budget))) (vec (distinct done))
      :else
      (let [[ws & more] frontier]
        (cond
          (seen ws)          (recur (vec more) seen done budget)
          (= 1 (count ws))   (recur (vec more) (conj seen ws) (conj done (first ws)) (dec budget))
          :else (recur (into (vec more) (map vec (greedy-sel-merge ws)))
                       (conj seen ws)
                       done
                       (dec budget)))))))

;; ---------------------------------------------------------------------------
;; reading a derivation
;; ---------------------------------------------------------------------------

(defn find-role
  "the highest SO bearing `role`, or nil. This is what replaces PRSO/PRSI being
  parser outputs: they are queries over the derivation."
  [root role]
  (first (filter #(= role (:role %)) (all-sos root))))

(defn words-of
  "every lexeme under an SO, in order."
  [root]
  (keep :lex (all-sos root)))

(defn leaves
  "the atomic (lexical) SOs under root, in order."
  [root]
  (filter atomic? (all-sos root)))

(defn leaves-above
  "the lexical items under root, WITHOUT descending into subtrees of these
  categories. Lets a noun phrase be read without its modifiers bleeding in: the
  words of \"the cup on the table\" that describe the cup are just \"the cup\"."
  [root stop-cats]
  (letfn [(walk [so]
            (cond
              (atomic? so)                   [so]
              (contains? stop-cats (:cat so)) []
              :else (mapcat walk (:so so))))]
    (walk root)))

(defn child-of-cat
  "the direct child of `so` with this category, or nil."
  [so cat]
  (first (filter #(= cat (:cat %)) (:so so))))
