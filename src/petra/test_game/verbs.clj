(ns petra.test-game.verbs
  "The test game's verbs, and its syntax file. Deliberately the game's own and not
  the engine's: what words mean and how generous a TAKE is are the game's business.
  If this ever gets extracted as a shared library, it will be by copying this file,
  not by the engine growing verbs."
  (:require [petra.engine.core :as e :refer [def-verb]]
            [petra.engine.lexicon :as lx :refer [def-word]]
            [petra.engine.parser]                           ; registers the assertions
            [petra.engine.syntactic :refer [V N P D A DIR PRED]]
            [petra.engine.text :as t]))

;; ---------------------------------------------------------------------------
;; behaviour
;; ---------------------------------------------------------------------------

(def-verb ::look
  handle (fn [_] (e/look!)))

(def-verb ::examine
  handle (fn [{:keys [k-dobj]}]
           (if-let [d (e/description k-dobj)]
             (e/tell! d :>>)
             (e/tell! "You see nothing special about " :the k-dobj "." :>>))))

(def-verb ::inventory
  handle (fn [{:keys [k-actor objects]}]
           (let [held (e/contents k-actor objects)]
             (if (empty? held)
               (e/tell! "You are empty-handed." :>>)
               (e/tell! (str "You are carrying:\n"
                             (clojure.string/join
                              "\n" (map #(str "  " (e/stringify-tell-token :a % objects))
                                        (sort-by #(e/o:label % objects) held))))
                        :>>)))))

(def-verb ::take
  handle (fn [{:keys [k-dobj k-actor objects]}]
           (cond
             (e/ultimately-in? k-dobj k-actor objects) (e/tell! "You already have that." :>>)
             (not (e/feature-set? k-dobj ::e/f-takeable objects))
               (e/tell! "That isn't something you can carry." :>>)
             :else (do (e/move! k-dobj k-actor) (e/tell! "Taken." :>>)))))

(def-verb ::drop
  handle (fn [{:keys [k-dobj k-actor k-here objects]}]
           (if (e/ultimately-in? k-dobj k-actor objects)
             (do (e/move! k-dobj k-here) (e/tell! "Dropped." :>>))
             (e/tell! "You aren't carrying that." :>>))))

(def-verb ::open
  handle (fn [{:keys [k-dobj]}]
           (if (e/open? k-dobj)
             (e/tell! :The k-dobj " is already open." :>>)
             (do (e/open! k-dobj) (e/tell! "You open " :the k-dobj "." :>>)))))

(def-verb ::close
  handle (fn [{:keys [k-dobj]}]
           (if (e/open? k-dobj)
             (do (e/shut! k-dobj) (e/tell! "You close " :the k-dobj "." :>>))
             (e/tell! :The k-dobj " is already shut." :>>))))

(def-verb ::put-in
  ;; ZIL's implicit take (9.6): if you aren't holding it and could be, take it
  ;; first, then decline so the default does the putting.
  pre    (fn [{:keys [k-dobj k-actor objects]}]
           (when (and k-dobj
                      (not (e/ultimately-in? k-dobj k-actor objects))
                      (e/feature-set? k-dobj ::e/f-takeable objects))
             (e/tell! "(first taking " :the k-dobj ")" :>>)
             (e/move! k-dobj k-actor)
             nil))
  handle (fn [{:keys [k-dobj k-iobj objects]}]
           (cond
             (= k-dobj k-iobj) (e/tell! "That would be quite a trick." :>>)
             (e/feature-set? k-iobj ::e/f-surface objects)
               (e/tell! "You can't put anything inside " :the k-iobj "." :>>)
             (not (e/feature-set? k-iobj ::e/f-container objects))
               (e/tell! "You can't put anything in " :the k-iobj "." :>>)
             (not (e/open? k-iobj)) (e/tell! :The k-iobj " is shut." :>>)
             :else (do (e/move! k-dobj k-iobj)
                       (e/tell! "You put " :the k-dobj " in " :the k-iobj "." :>>)))))

;; A DIFFERENT verb, reached by a homophonous `put` that selects a P headed by
;; "on" instead of one headed by "in". Same word, different lexical item.
(def-verb ::put-on
  pre    (fn [{:keys [k-dobj k-actor objects]}]
           (when (and k-dobj
                      (not (e/ultimately-in? k-dobj k-actor objects))
                      (e/feature-set? k-dobj ::e/f-takeable objects))
             (e/tell! "(first taking " :the k-dobj ")" :>>)
             (e/move! k-dobj k-actor)
             nil))
  handle (fn [{:keys [k-dobj k-iobj objects]}]
           (cond
             (= k-dobj k-iobj) (e/tell! "That would be quite a trick." :>>)
             (not (e/feature-set? k-iobj ::e/f-surface objects))
               (e/tell! "There's no good surface on " :the k-iobj "." :>>)
             :else (do (e/move! k-dobj k-iobj)
                       (e/tell! "You put " :the k-dobj " on " :the k-iobj "." :>>)))))

;; movement: resolve, then act. The exit-* accessors are the whole interface.
(def-verb ::walk
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (cond
             (nil? direction) (e/tell! "Which way?" :>>)
             :else
             (let [r (e/resolve-exit k-here direction objects)]
               (cond
                 (e/exit-to r)      (e/goto! (e/exit-to r))
                 (e/exit-handler r) ((e/exit-handler r) ctx)
                 :else              (e/tell! (e/exit-message r) :>>))))
           ::e/handled))

(def-verb ::verbose
  turn? false
  handle (fn [_] (e/set-verbosity! e/v-verbose) (e/tell! "Verbose mode." :>>)))

(def-verb ::brief
  turn? false
  handle (fn [_] (e/set-verbosity! e/v-brief) (e/tell! "Brief mode." :>>)))

(def ^:const quit ::quit)
(def-verb ::quit
  turn? false
  handle (fn [_] (e/end-game! "Goodbye.")))

;; ---------------------------------------------------------------------------
;; the syntax file: which words mean which of the above
;; ---------------------------------------------------------------------------

(def-word ["look" "l"]                    V [:_]              verb ::look)
(def-word ["look at" "examine" "x"]       V [:_ [N :DO]]      verb ::examine)
(def-word ["inventory" "i"]               V [:_]              verb ::inventory)
;; Pragmatic assertions: what each verb expects of its object. They break a
;; tie between referents and nothing more -- see petra.engine.parser.
(def-word ["take" "get" "pick up"]        V [:_ [N :DO #{:takeable :not-held}]] verb ::take)
(def-word ["drop" "put down"]             V [:_ [N :DO #{:held}]]  verb ::drop)
(def-word ["open"]                        V [:_ [N :DO #{:not-open}]]  verb ::open)
(def-word ["close" "shut"]                V [:_ [N :DO #{:open}]]  verb ::close)
;; Head-to-head selection: two homophonous V heads, told apart by which
;; preposition heads the phrase they select. "put ... in" and "put ... on" are
;; genuinely different verbs that happen to be spelled the same.
(def-word ["put" "place" "insert"]        V [:_ [N :DO #{:held}] [P "in"]] verb ::put-in)
(def-word ["put" "place" "set" "lay"]     V [:_ [N :DO #{:held}] [P "on"]] verb ::put-on)
(def-word ["verbose"]                     V [:_]              verb ::verbose)
(def-word ["brief"]                       V [:_]              verb ::brief)
(def-word ["quit" "q"]                    V [:_]              verb ::quit)

;; Argument prepositions: what a verb selects. "put it IN the pail."
(def-word ["in"]                          P [:_ [N :IO #{:container}]])
(def-word ["on"]                          P [:_ [N :IO #{:surface}]])

;; Predicative prepositions: a reduced relative narrowing a noun phrase --
;; "the cup ON THE TABLE" means the cup that is on the table. Same spellings,
;; different category, so a verb can never mistake one for its own argument.
(def-word ["in" "inside"]                 PRED [:_ [N :LOC]])
(def-word ["on"]                          PRED [:_ [N :LOC]])

(def-word ["the" "a" "an" "some" "my"]    D [:_])

;; A direction is both a DIR (for "go north") and a sentence in its own right
;; ("north"), which is what lexical ambiguity is for -- no special case needed.
(def-word ["go" "walk" "run"]             V [:_ [DIR :DIR]]   verb ::walk)
(doseq [[words dir] [[["north" "n"] e/kw-north] [["south" "s"] e/kw-south]
                     [["east" "e"] e/kw-east]   [["west" "w"] e/kw-west]
                     [["up" "u"] e/kw-up]       [["down" "d"] e/kw-down]
                     [["in"] e/kw-in]           [["out"] e/kw-out]]]
  (lx/make-words words DIR [:_] {::lx/direction dir})
  (lx/make-words words V   [:_] {::lx/verb ::walk ::lx/direction dir}))
