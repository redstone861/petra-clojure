(ns petra.engine.parser-test
  "Input becomes a command. PRSO/PRSI are queries over a derivation."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as string]
            [petra.engine.core :as e :refer [object room def-verb]]
            [petra.engine.lexicon :as lx :refer [def-word]]
            [petra.engine.parser :as ps]
            [petra.engine.syntactic :as syn :refer [V N P D A DIR PRED]]
            [petra.support :as t]))

(defn- build! []
;; --- a small world ---------------------------------------------------------
  (object ::you       label "you" features [no-article])
  (object ::tin-cup   label "tin cup"   features [takeable])
  (object ::clay-cup  label "clay cup"  features [takeable])
  (object ::rag       label "wet rag"   features [takeable])
  (object ::pail      label "rusty pail" features [container takeable])
  (object ::shelf     label "stone shelf" features [surface])
  (object ::table     label "trestle table" features [surface])
  (object ::door      label "oak door")
  (object ::elsewhere label "brass gong" features [takeable])   ; exists, never placed
  (room ::hall label "Hall" features [lit]
        share [::door]
        contains [::tin-cup ::clay-cup ::rag ::pail ::shelf ::table]
        to [[north ::yard]])
  (room ::yard label "Yard" features [lit] to [[south ::hall]])
;; --- verbs and a syntax file ----------------------------------------------
  (def-verb ::look    handle (fn [_] ::e/handled))
  (def-verb ::take    handle (fn [_] ::e/handled))
  (def-verb ::drop    handle (fn [_] ::e/handled))
  (def-verb ::open    handle (fn [_] ::e/handled))
  (def-verb ::examine handle (fn [_] ::e/handled))
  (def-verb ::put-in  handle (fn [_] ::e/handled))
  (def-verb ::put-on  handle (fn [_] ::e/handled))
  (def-verb ::walk    handle (fn [_] ::e/handled))
  (def-word ["look" "l"]              V [:_]                                verb ::look)
  (def-word ["look at" "examine" "x"] V [:_ [N :DO]]                        verb ::examine)
  (def-word ["take" "get" "pick up"]  V [:_ [N :DO #{:takeable :not-held}]] verb ::take)
  (def-word ["drop"]                  V [:_ [N :DO #{:held}]]               verb ::drop)
  (def-word ["open"]                  V [:_ [N :DO #{:shut}]]               verb ::open)
  (def-word ["put" "insert"]          V [:_ [N :DO #{:held}] [P "in"]]      verb ::put-in)
  (def-word ["put" "set" "lay"]       V [:_ [N :DO #{:held}] [P "on"]]      verb ::put-on)
  (def-word ["go" "walk"]             V [:_ [DIR :DIR]]                     verb ::walk)
  (def-word ["in"]                    P [:_ [N :IO #{:container}]])
  (def-word ["on"]                    P [:_ [N :IO #{:surface}]])
  (def-word ["in" "inside"]           PRED [:_ [N :LOC]])
  (def-word ["on"]                    PRED [:_ [N :LOC]])
  (def-word ["the" "a" "an" "my"]     D [:_])
  (doseq [[ws d] [[["north" "n"] ::e/north] [["south" "s"] ::e/south]]]
    (lx/make-words ws DIR [:_] {::lx/direction d})
    (lx/make-words ws V   [:_] {::lx/verb ::walk ::lx/direction d})))

(use-fixtures :each (t/with-world build!))

(defn- setup! [] (e/set-actor! ::you) (e/place! ::you ::hall))

;; --- the basics ------------------------------------------------------------

(deftest a-sentence-becomes-a-command
  (setup!)
  (is (= {:verb ::look :dobj nil :iobj nil :direction nil} (t/parse "look")))
  (is (= ::look (t/verb-of "l")) "a synonym reaches the same verb")
  (is (= [::take ::rag] [(t/verb-of "take the wet rag") (t/dobj-of "take the wet rag")]))
  (is (= ::rag (t/dobj-of "take rag")) "the determiner is optional")
  (is (= ::tin-cup (t/dobj-of "take tin cup")) "an adjective narrows"))

(deftest multi-word-verbs-are-one-lexeme
  (setup!)
  (is (= ::examine (t/verb-of "look at rag")))
  (is (= ::take (t/verb-of "pick up rag")) "so separable verbs need no movement"))

(deftest roles-are-read-off-the-derivation
  (setup!)
  (e/open! ::pail)
  (let [p (t/parse "put the tin cup in the rusty pail")]
    (is (= ::put-in (:verb p)))
    (is (= ::tin-cup (:dobj p)))
    (is (= ::pail (:iobj p))))
  (is (= [::tin-cup ::pail]
         ((juxt :dobj :iobj) (t/parse "put tin cup in pail")))
      "with every determiner dropped"))

(deftest directions
  (setup!)
  (is (= [::walk ::e/north] ((juxt :verb :direction) (t/parse "north")))
      "a bare direction is a sentence")
  (is (= ::e/north (t/dir-of "n")))
  (is (= ::e/north (t/dir-of "go north")) "or an argument of go"))

(deftest optional-slots-really-are-optional
  (setup!)
  (is (some? (t/dobj-of "take the wet rag")) "all slots filled")
  (is (some? (t/dobj-of "take rag")) "determiner dropped")
  (is (some? (t/dobj-of "take the tin cup")) "adjective present")
  (is (some? (t/dobj-of "take tin cup")) "both"))

;; --- failures --------------------------------------------------------------

(deftest failures-are-sentences-a-player-can-read
  (setup!)
  (is (= "[I don't know the word \"frobnitz\".]" (t/err "take frobnitz")))
  (is (= "[I don't understand that sentence.]" (t/err "the the the")))
  (is (some? (t/err "")))
  (testing "in the game but not in scope"
    (is (= "You can't see any gong here." (t/err "take gong"))
        "the word is known; only scope decides referability")
    (is (not (re-find #"don't know" (t/err "take gong"))))))

(deftest scope-includes-the-rooms-share-list
  (setup!)
  (is (contains? (e/in-scope) ::door))
  (is (= ::door (t/dobj-of "open oak door")) "which is what makes a door openable")
  (e/place! ::you ::yard)
  (is (re-find #"can't see" (t/err "open oak door")) "not from the next room"))

;; --- disambiguation --------------------------------------------------------

(deftest an-unbroken-tie-asks
  (setup!)
  (is (= "Which do you mean, the clay cup or the tin cup?" (t/err "take cup")))
  (is (= ::clay-cup (t/dobj-of "take clay cup")) "an adjective settles it"))

(deftest three-way-disambiguation-reads-properly
  (setup!)
  (e/make-object ::stone-cup {::e/label "stone cup" ::e/features #{::e/f-takeable}})
  (e/place! ::stone-cup ::hall)
  (is (= "Which do you mean, the clay cup, the stone cup, or the tin cup?"
         (t/err "take cup"))))

(deftest assertions-break-a-tie-without-asking
  (setup!)
  (e/place! ::tin-cup ::you)                                 ; holding one of two cups
  (is (= ::tin-cup  (t/dobj-of "drop the cup")) "DROP wants a held thing")
  (is (= ::clay-cup (t/dobj-of "take the cup")) "TAKE wants a not-held thing")
  (testing "the same phrase, two verbs, two referents"
    (is (not= (t/dobj-of "drop cup") (t/dobj-of "take cup")))))

(deftest assertions-never-become-errors
  (setup!)                                                   ; holding neither cup
  (is (nil? (t/err "drop the cup")) "the verb owns that complaint, not the parser")
  (is (some? (t/dobj-of "drop the cup")) "so something is still chosen")
  (is (= (t/dobj-of "drop the cup") (t/dobj-of "drop the cup")) "deterministically"))

(deftest a-lone-candidate-is-never-rejected
  (setup!)
  (e/remove! ::clay-cup)
  (is (= ::tin-cup (t/dobj-of "drop the cup"))
      "not held, but it is the only cup there is"))

(deftest the-note-reports-an-assumption-and-only-that
  (setup!)
  (is (nil? (t/note "take the wet rag")) "nothing was in doubt")
  (is (nil? (t/note "take tin cup")) "nor here")
  (e/place! ::tin-cup ::you)
  (is (= "(the clay cup)" (t/note "take the cup")) "an assumption, reported")
  (is (= "(the tin cup)" (t/note "drop the cup")))
  (e/place! ::tin-cup ::hall)
  (is (nil? (t/note "drop the cup"))
      "a doomed guess is no help to anyone, so it is silent"))

;; --- structure -------------------------------------------------------------

(deftest head-to-head-selection-splits-two-homophonous-puts
  (setup!)
  (e/open! ::pail)
  (is (= ::put-in (t/verb-of "put the tin cup in the pail")))
  (is (= ::put-on (t/verb-of "put the tin cup on the shelf")))
  (is (= ::put-on (t/verb-of "lay tin cup on shelf")) "a synonym of only one")
  (is (some? (t/err "lay tin cup in pail")) "and it refuses the other preposition"))

(deftest a-pp-attaches-where-the-verb-allows
  (setup!)
  (e/place! ::tin-cup ::table)
  (e/place! ::clay-cup ::shelf)
  (testing "TAKE has no P slot, so the PP describes the noun"
    (is (= ::tin-cup (t/dobj-of "take the cup on the table")))
    (is (= ::clay-cup (t/dobj-of "take the cup on the shelf"))))
  (testing "PUT-ON needs one, so it is the verb's argument"
    (e/place! ::tin-cup ::you)
    (is (= [::tin-cup ::shelf]
           ((juxt :dobj :iobj) (t/parse "put the tin cup on the shelf"))))))

(deftest both-attachments-in-one-sentence
  (setup!)
  (e/place! ::tin-cup ::table)
  (e/place! ::clay-cup ::shelf)
  (let [p (t/parse "put the cup on the table on the shelf")]
    (is (= ::tin-cup (:dobj p)) "the first PP narrowed the object")
    (is (= ::shelf (:iobj p)) "the second is the goal")))

(deftest which-attachment-wins-is-decided-by-the-world
  (setup!)
  (e/place! ::table ::shelf)                                 ; now a table IS on the shelf
  (e/place! ::tin-cup ::hall)                                ; and nothing is on the table
  (is (= ::table (:iobj (t/parse "put the cup on the table on the shelf")))
      "same sentence, the other attachment"))

(deftest a-modifier-is-not-one-of-the-nouns-own-words
  (setup!)
  (e/place! ::tin-cup ::table)
  (is (= ::tin-cup (t/dobj-of "take the cup on the table"))
      "or {cups} n {tables} would be empty")
  (is (re-find #"can't see" (t/err "take the cup on the pail"))
      "a location nothing is in is a can't-see, not a misparse"))

(deftest predicative-and-argument-prepositions-are-different-categories
  (is (not= syn/P syn/PRED)))

;; --- the machinery ---------------------------------------------------------

(deftest optionality-and-exhaustive-search
  (let [L [(syn/entry "eat" V [:_ [N :DO]])
           (syn/entry "apple" N ['(:D) '(:A) :_])
           (syn/entry "the" D [:_])
           (syn/entry "red" A [:_ '(:A)])]
        parse-count (fn [s] (count (syn/derive-all (mapv first (syn/lexer s L)))))]
    (is (= 1 (parse-count "eat the red apple")))
    (is (= 1 (parse-count "eat the apple")) "adjective dropped")
    (is (= 1 (parse-count "eat apple")) "both dropped")))

(deftest selection-checks-the-fillers-head-lexeme
  (let [in-slot  (first (syn/psel [[P "in"]] V))
        on-thing {:cat P :lex "on" :sel nil}
        in-thing {:cat P :lex "in" :sel nil}]
    (is (syn/satisfies-selection? in-slot in-thing))
    (is (not (syn/satisfies-selection? in-slot on-thing))
        "same category, wrong head")))

(deftest unknown-assertions-are-refused-at-declaration
  (is (t/throws-info? #"unknown pragmatic assertion"
                      #(lx/make-words ["frob"] V [:_ [N :DO #{:wibble}]] {})))
  (is (contains? (ps/assertion-names) :held)))
