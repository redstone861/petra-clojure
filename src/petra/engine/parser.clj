(ns petra.engine.parser
  "Input -> something turn! can run.

  The pipeline: lex (longest-match, all candidates per word) -> search the merge
  space for complete derivations -> read the verb off the root and the arguments
  off their theta-roles -> resolve each noun phrase against what's in scope.

  PRSO and PRSI are not outputs of this process. They are queries over a
  derivation, which is the whole reason the parser is built out of selectional
  frames instead of ZIL's flat syntax table."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [petra.engine.core :as e]
            [petra.engine.text :as t]
            [petra.engine.lexicon :as lx]
            [petra.engine.syntactic :as syn]))

(def ^:const max-readings 64)                               ; cap on lexical ambiguity

(defn- fail [frame-id & {:as args}]
  {::error (e/say frame-id (or args {}))})

;; ---------------------------------------------------------------------------
;; pragmatic assertions
;; ---------------------------------------------------------------------------
;; A frame slot may assert what the verb expects of its filler -- [N :DO #{held}]
;; on DROP, #{takeable not-held} on TAKE. These are PREFERENCES, used only to break
;; a tie between candidate referents:
;;
;;   exactly one candidate satisfies them  -> take it, and ask nothing
;;   several do                            -> ask, but only among those
;;   none do                               -> pick one and let the verb complain
;;
;; That last rule is the important one. The parser must not turn an assertion into
;; an error, because the VERB owns that error and words it properly ("You aren't
;; carrying that."). An assertion's whole job is to avoid a needless question.
;;
;; The vocabulary itself lives in engine.core next to feature-symbols, because most
;; assertions ARE features and the lexicon has to validate a slot when it is
;; declared. This is ZIL's FIND/GWIM (9.5) and the HAVE/HELD syntax tokens (9.6).

(defn- satisfies-asserts?
  [k asserts k-actor objects]
  (every? (fn [a] (if-let [pred (e/assertion-pred a)]
                    (pred k k-actor objects)
                    true))                                  ; unknown: make-words rejects these
          asserts))

;; ---------------------------------------------------------------------------
;; noun phrases -> objects
;; ---------------------------------------------------------------------------

(defn- np-objects
  "the objects a noun phrase could denote.

  Its own words intersect: every word carries the set it could name, so \"tin cup\"
  is {cups} n {tin things}. A predicative modifier is NOT one of those words -- it
  is a separate claim about where the thing is -- so the walk stops at PRED and the
  modifier narrows afterwards, recursively, since \"the cup on the table on the
  shelf\" nests.

  Without that boundary, \"the cup on the table\" would intersect {cups} with
  {tables} and denote nothing at all."
  [np objects]
  (let [own  (keep ::lx/objects (syn/leaves-above np #{syn/PRED}))
        base (if (empty? own) #{} (reduce set/intersection own))]
    (if-let [pred (syn/child-of-cat np syn/PRED)]
      (let [holders (np-objects (syn/find-role pred :LOC) objects)]
        (set (filter (fn [o] (some #(e/ultimately-in? o % objects) holders)) base)))
      base)))

(defn- or-join [phrases]
  (case (count phrases)
    1 (first phrases)
    2 (str (first phrases) " or " (second phrases))
    (str (string/join ", " (butlast phrases)) ", or " (last phrases))))

(defn- ask-which [os objects]
  {::error (e/say ::t/which-one
                  {:things (or-join (map #(e/stringify-tell-token :the % objects)
                                         (sort-by #(e/o:label % objects) os)))})})

(defn- resolve-np
  "one object, or an error explaining which way it went wrong. When more than one
  candidate fits the words, the slot's pragmatic assertions decide."
  [np k-actor objects]
  (let [phrase (string/join " " (syn/words-of np))
        os (np-objects np objects)]
    (cond
      (empty? os)      (fail ::t/cant-see :thing phrase)
      (= 1 (count os)) {::object (first os)}                 ; determined, not chosen
      :else
      ;; a tie. does what the verb expects of this argument break it?
      (let [fits (filter #(satisfies-asserts? % (:asserts np) k-actor objects) os)]
        (case (count fits)
          ;; ::chose means "the parser made an assumption the player did not state",
          ;; which is the only thing worth reporting.
          1 (let [k (first fits)] {::object k ::chose k})
          ;; nothing fits, so this is a doomed guess rather than a helpful
          ;; assumption -- pick one silently and let the verb explain.
          0 {::object (first (sort os))}
          (ask-which fits objects))))))

(defn- note-for
  "the aside telling the player what the parser settled on, or nil if nothing was
  in doubt. Rendered here, like ::error is, so a caller only has to print it."
  [ks objects]
  (when-let [ks (seq (distinct (remove nil? ks)))]
    (e/say ::t/chose
           {:things (string/join ", "
                                 (map #(e/stringify-tell-token :the % objects) ks))})))

;; ---------------------------------------------------------------------------
;; reading a derivation
;; ---------------------------------------------------------------------------

(defn- verb-of [root] (::lx/verb (syn/head-leaf root)))

(defn- direction-of
  "a direction either rides on the verb's own word (\"north\") or is a DIR argument
  of it (\"go north\")."
  [root]
  (or (::lx/direction (syn/head-leaf root))
      (some-> (syn/find-role root :DIR) syn/head-leaf ::lx/direction)))

(defn- read-derivation
  "a complete derivation -> a dispatchable map, or an error."
  [root k-actor objects]
  (if-not (= syn/V (:cat root))
    (fail ::t/not-a-sentence)
    (let [verb (verb-of root)
          dobj (syn/find-role root :DO)
          iobj (syn/find-role root :IO)
          rd   (when dobj (resolve-np dobj k-actor objects))
          ri   (when iobj (resolve-np iobj k-actor objects))]
      (cond
        (nil? verb)   (fail ::t/not-a-sentence)
        (::error rd)  rd
        (::error ri)  ri
        :else {:verb verb
               :dobj (::object rd)
               :iobj (::object ri)
               :direction (direction-of root)
               ::chosen (vec (keep ::chose [rd ri]))}))))

;; ---------------------------------------------------------------------------
;; the parse
;; ---------------------------------------------------------------------------

(defn- readings
  "every combination of lexical choices, capped. Most words are unambiguous, so
  this is usually a single reading."
  [candidates]
  (take max-readings
        (reduce (fn [acc alts] (for [a acc, c alts] (conj a c)))
                [[]]
                candidates)))

(defn parse
  "input -> {:verb :dobj :iobj :direction} for turn!, or {::error message}.
  Every failure is a sentence the player can read."
  ([input] (parse input @e/ACTOR @e/OBJECTS))
  ([input k-actor objects]
   (let [lexicon (lx/lexicon k-actor objects)
         cands (syn/lexer input lexicon)]
     (cond
       (empty? cands)
       (fail ::t/say-something)

       ;; an unknown word is the one failure that can be named precisely
       (some empty? cands)
       (let [words (syn/pre-lexer input lexicon)
             unknown (first (keep-indexed (fn [i c] (when (empty? c) (nth words i))) cands))]
         (fail ::t/unknown-word :word unknown))

       :else
       (let [roots (mapcat syn/derive-all (readings cands))
             ;; a reading that yields a usable command beats one that merely parses
             results (map #(read-derivation % k-actor objects) roots)
             usable (remove ::error results)]
         (if-let [p (first usable)]
           ;; more than one whole reading resolved: the structure was in doubt, so
           ;; one was picked -- name what it acts on, same as any other assumption.
           (let [ks (if (next usable)
                      (conj (::chosen p) (or (:dobj p) (:iobj p)))
                      (::chosen p))]
             (cond-> (dissoc p ::chosen)
               (note-for ks objects) (assoc ::note (note-for ks objects))))
           (or (first results) (fail ::t/not-a-sentence))))))))
