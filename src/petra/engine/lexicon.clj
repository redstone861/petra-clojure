(ns petra.engine.lexicon
  "Words -> meanings. The registry in engine.core maps a verb KEYWORD to
  behaviour; this maps INPUT WORDS to those keywords. Synonymy lives here and
  nowhere else, which is the split ZIL's VERB-SYNONYM only looks like it gets
  wrong until you notice it lives in the syntax file rather than the verbs file.

  Two halves:

    static   declared with def-word: verbs, prepositions, determiners, directions.
             A game writes these once; they are its syntax file.
    dynamic  built per parse from whatever is in scope, so \"lantern\" is a word
             only while a lantern is there to be referred to. Each such entry
             carries the set of objects it could denote, which is what turns a
             noun phrase back into an object."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [petra.engine.core :as e]
            [petra.engine.syntactic :as syn]))

;; ---------------------------------------------------------------------------
;; static words
;; ---------------------------------------------------------------------------

(def WORDS (atom []))

(def word-symbols
  {'verb      ::verb                                        ; the verb keyword this word names
   'direction ::direction})                                 ; the direction it names

(def ^:dynamic *known-assertions*
  "set by petra.engine.parser, which owns the registry. A fn rather than a require,
  to keep the lexicon from depending on the resolver."
  (constantly true))

(defn make-words [words cat frame extra]
  (let [sel (syn/psel frame cat)
        _ (doseq [slot sel, a (:asserts slot)]
            (when-not (*known-assertions* a)
              (throw (ex-info "unknown pragmatic assertion"
                              {:assertion a :words words}))))
        entries (mapv (fn [w] (merge {:lex (string/lower-case w) :cat cat :sel sel} extra))
                      words)]
    (swap! WORDS into entries)
    entries))

(defmacro def-word
  "declare one meaning, under any number of synonymous spellings.

    (def-word [\"take\" \"get\" \"pick up\"] V [:_ [N :DO]]  verb ::take)
    (def-word [\"in\" \"into\"]              P [:_ [N :IO]])
    (def-word [\"north\" \"n\"]              V [:_]          verb ::walk direction ::north)

  Multi-word spellings are fine: the lexer matches the longest, so \"pick up\" is
  one item and the parser never sees the seam."
  [words cat frame & properties]
  (when-not (even? (count properties))
    (throw (ex-info "def-word needs property/value pairs" {:words words})))
  (let [extra (into {}
                    (for [[k v] (partition 2 properties)]
                      (if-let [wk (get word-symbols k)]
                        [wk v]
                        (throw (ex-info "Unknown word property"
                                        {:property k :words words
                                         :known (vec (sort (keys word-symbols)))})))))]
    `(make-words ~words ~cat ~frame ~extra)))

(defn clear-words! [] (reset! WORDS []))

;; ---------------------------------------------------------------------------
;; dynamic words, from whatever is in scope
;; ---------------------------------------------------------------------------
;; Frames are built once here rather than per word: psel calls eval on optional
;; slots, and doing that per word per turn would be silly.

;; a noun phrase: optional determiner, optional adjectives, and optionally one
;; predicative PP adjoined on the right -- "the small cup on the table".
(def noun-frame (syn/psel ['(:D) '(:A) :_ '(:PRED)] syn/N))
(def adj-frame  (syn/psel [:_ '(:A)] syn/A))                ; A selecting A: adjectives stack

(defn- invert
  "{word #{objects}} from a fn giving each object's words."
  [ks words-fn]
  (reduce (fn [m k] (reduce #(update %1 %2 (fnil conj #{}) k) m (words-fn k)))
          {} ks))

(defn scope-words
  "lexical entries for every object in the game, each carrying only the objects it
  could denote RIGHT NOW.

  The vocabulary is the whole game's, deliberately: a word for something that
  exists but is elsewhere is a word the game knows, so referring to it earns \"you
  can't see any rusty pail here\" and not \"I don't know the word rusty\". Scope
  decides what is referable, not what is pronounceable. Out-of-scope words carry
  an empty candidate set, so resolution fails with the right complaint.

  A word naming several objects carries all of them; narrowing is what adjectives
  are for."
  [k-actor objects]
  (let [everything (keys objects)
        here (e/in-scope k-actor objects)
        visible (fn [os] (set/intersection os here))]
    (concat
     (for [[w os] (invert everything #(e/nouns-of % objects))]
       {:lex w :cat syn/N :sel noun-frame ::objects (visible os)})
     (for [[w os] (invert everything #(e/adjectives-of % objects))]
       {:lex w :cat syn/A :sel adj-frame ::objects (visible os)}))))

(defn lexicon
  "the whole lexicon for one parse: the game's static words plus the words that
  exist only because something is in scope."
  ([] (lexicon @e/ACTOR @e/OBJECTS))
  ([k-actor objects] (concat @WORDS (scope-words k-actor objects))))
