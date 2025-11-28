(ns petra.core
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

(defn get-head [so]
  (first (filter :head so))
)

(defn label
  [so]
  (:cat (get-head so))
)

(defn transfer-meta
  "modify (and return) the so such that all necessary information 
  is transferred from the selectional stack, e.g., role."
  [so sel]
  (assoc so :role (:role sel))
)

(defn mrg
  [sos head l r]
  (let [sel (concat l [:_] r)
        merged-raw (apply list sos)
        merged (mapv transfer-meta merged-raw sel)]
    (print sel)
    {:head head :cat (:cat head) :so merged})
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

(defn lexer
  [strin lx]
  (map 
   #(first (filter (fn [ent] (= % (:lex ent))) lx))
   strin
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
  (map
   (fn [so]
     (let [sel (:sel so)]
       (if (= 1 (count sel))
         (mrg so so [] [])
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

(defn replace-first-by-sel-frame
  "Given a seq of lexical items, find the first index whose selectional
   frame matches a window in the surrounding context, using the largest
   possible window first, falling back to smaller ones, then go on to next index.
   Replace the entire matched window with (mrg window)."
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

(defn greedy-mrg
  "greedily-first merge the first atomic element of the list."
  [sos]
  (let [[before nb] (split-with (complement atomic?) sos)
         head  (first nb)
         after (rest nb)]
    [before head after]
    (filter identity
            (map (fn []))))
)

(def lexicon (lex 
              ["eat" V [:_ '([N :DO])]] 
              ["apple" N ['(D) '(A) :_]]
              ["the" D [:_]]
              ["red" A [:_]]
              ["take" V [:_ [N :DO]]]))

(def lexed (lexer ["eat" "apple"] lexicon))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (psel [:_ :A '([:B :DO])] :L))
  (entry "eat" V [:_ N])
  ;(mrg 1 2)
)
