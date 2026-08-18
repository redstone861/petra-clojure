(ns petra.engine.core
  (:require [clojure.string :as string]
            [petra.engine.text :as text]))

;; ---------------------------------------------------------------------------
;; property keys
;; ---------------------------------------------------------------------------

(def ^:const kw-label ::label)
(def ^:const kw-room-exits ::exits)
(def ^:const kw-handler ::handler)
(def ^:const kw-pre-handler ::pre-handler)
(def ^:const kw-turn? ::turn?)
(def ^:const kw-features ::features)

;; features. authors write these as bare symbols -- (features [lit open]) -- see
;; `feature-symbols` for the mapping; a game may also use its own keywords.
;; ::f-vowel-article : force /an/ for the :a tell!-token (an hour). only needed
;;                     where the label's first letter misleads -- see
;;                     `indefinite-article`, which otherwise infers it.
;; ::f-consonant-article : force /a/ (a unicorn, a one-way door)
;; ::f-no-article : print name with no article, like /God/ instead of /the God/
;; ::f-open : the object (a door, a container) is currently open
;; ::f-container : things can be put inside it, and it can be shut
;; ::f-transparent : you can see into it even when it is shut
;; ::f-lit : gives off light (of a room: is lit regardless of what is in it)
;; ::f-no-describe : the describers skip it, because something else mentions it
;; ::f-touched : has been moved at least once. set by move!, not by the author;
;;               it is what retires an object's `fdesc`.
;; ::f-visited : the actor has been in this room before. set by look!, not by the
;;               author; it is what makes a long description appear once.

;; containment is stored ONE way: a parent's ::contains-local is the set of keys
;; it holds, and that set is the single source of truth. an object does not
;; record its own location; location-of derives it. see the containment section.
(def ^:const kw-contains-local ::contains-local)
;; ::contains-shared is NOT containment -- it is referenceability. it lists the
;; shared objects (doors, water, stairs) a room lets the player refer to without
;; holding them, the way ZIL's GLOBAL property lists local-globals.
(def ^:const kw-contains-shared ::contains-shared)
(def ^:const kw-description-first ::description-first)
;; ::description-detailed holds EITHER a string or a fn of ctx returning a string
;; (or nil, meaning "nothing special to say"). see `description`. one property
;; where ZIL needed LDESC plus the M-LOOK rarg plus the M-OBJDESC?/M-OBJDESC
;; query-then-command pair.
(def ^:const kw-description-detailed ::description-detailed)
;; ::on maps an event keyword to a notification fn. see `notify!`. this is what
;; replaces the rest of ZIL's rarg tags: dispatch is a lookup in this map rather
;; than a COND inside one multiplexed routine. the map is open, so a game may
;; register and raise its own namespaced events.
(def ^:const kw-on ::on)
(def ^:const kw-label-heads ::label-heads)
(def ^:const kw-label-modifiers ::label-modifiers)

(def ^:const kw-north ::north)
(def ^:const kw-east ::east)
(def ^:const kw-south ::south)
(def ^:const kw-west ::west)
(def ^:const kw-up ::up)
(def ^:const kw-down ::down)
(def ^:const kw-in ::in)
(def ^:const kw-out ::out)

(def OBJECTS (atom {}))
(def ACTOR (atom nil))                                      ; actor key

;; the root containers. every room is contained by ROOMS; that is what makes a
;; room a room, and what lets room-of stop climbing.
(def ^:const ROOMS ::ROOMS)
(def ^:const SHARED ::SHARED)
(def ^:const GLOBALS ::GLOBALS)
(def ^:const INTANGIBLES ::INTANGIBLES)

;; ---------------------------------------------------------------------------
;; access
;; ---------------------------------------------------------------------------
;; CONVENTION: everything public in this namespace takes and returns object
;; KEYS, never object maps. `obj` is the one place a map surfaces. each reader
;; has an arity taking an explicit `objects` snapshot, so that everything read
;; while handling one input sees one consistent world.

(defn obj
  ([k] (obj k @OBJECTS))
  ([k objects] (get objects k)))

(defn prop
  ([k attr] (prop k attr @OBJECTS))
  ([k attr objects] (get (obj k objects) attr)))

(defn o:label
  ([k] (o:label k @OBJECTS))
  ([k objects] (prop k kw-label objects)))

(defn handler-of
  ([k] (handler-of k @OBJECTS))
  ([k objects] (prop k kw-handler objects)))

(defn feature-set?
  ([k feature] (feature-set? k feature @OBJECTS))
  ([k feature objects] (contains? (prop k kw-features objects) feature)))

(defn open? [k] (feature-set? k ::f-open))

;; ---------------------------------------------------------------------------
;; containment
;; ---------------------------------------------------------------------------
;; ::contains-local is authoritative; an object's location is derived from it.
;; the invariant is that a key appears in at most one parent's ::contains-local,
;; and move!/remove! are the only things that write it, so they are the only
;; things that have to preserve it.

(defn contents
  "the set of keys k directly holds. authoritative."
  ([k] (contents k @OBJECTS))
  ([k objects] (or (prop k kw-contains-local objects) #{})))

(defn location-of
  "the key of the object holding k, or nil if k is nowhere (ZIL's LOC of false).
  derived by scan, since ::contains-local is the only stored direction. if the
  one-parent invariant is ever violated, this returns an arbitrary parent."
  ([k] (location-of k @OBJECTS))
  ([k objects]
   (some (fn [[parent-k m]]
           (when (contains? (get m kw-contains-local) k) parent-k))
         objects)))

(defn parent-index
  "child key -> the set of every parent whose ::contains-local holds it. one pass
  over the whole containment tree; order doesn't matter."
  ([] (parent-index @OBJECTS))
  ([objects]
   (reduce (fn [acc [parent-k m]]
             (reduce (fn [a child] (update a child (fnil conj #{}) parent-k))
                     acc
                     (get m kw-contains-local)))
           {}
           objects)))

(defn containment-problems
  "every key held by more than one parent, as {key #{parents}}. empty when the
  world is well formed.

  move!/remove! preserve the one-parent invariant, but `contains` in a definition
  cannot -- nothing stops two rooms both listing the same object, and the symptom
  would be an object that teleports depending on which scan won.

  returns data rather than throwing, so it's usable from the repl; `boot!` is what
  turns a non-empty result into an error."
  ([] (containment-problems (parent-index)))
  ([index] (into {} (filter (fn [[_ parents]] (> (count parents) 1)) index))))

(defn in?
  "true if k is DIRECTLY held by k-in (ZIL's IN?)."
  ([k k-in] (in? k k-in @OBJECTS))
  ([k k-in objects] (contains? (contents k-in objects) k)))

(defn ultimately-in?
  "true if k is held by k-in at any depth (ZIL's HELD? / ULTIMATELY-IN?)."
  ([k k-in] (ultimately-in? k k-in @OBJECTS))
  ([k k-in objects]
   (loop [cur (location-of k objects)
          seen #{}]
     (cond
       (nil? cur) false
       (= cur k-in) true
       (seen cur) false                                     ; malformed world; don't spin
       :else (recur (location-of cur objects) (conj seen cur))))))

(defn room-of
  "the room k is in: the nearest ancestor (or k itself) held by ROOMS.
  nil if k is not in any room -- removed, or inside something removed.
  this is ZIL's META-LOC, and with k = the actor it is ZIL's HERE."
  ([k] (room-of k @OBJECTS))
  ([k objects]
   (loop [cur k
          seen #{}]
     (let [loc (and cur (location-of cur objects))]
       (cond
         (nil? loc) nil
         (= ROOMS loc) cur
         (seen cur) nil                                     ; malformed world; don't spin
         :else (recur loc (conj seen cur)))))))

(defn see-inside?
  "true if k's contents are apparent: anything that is not a shut container."
  ([k] (see-inside? k @OBJECTS))
  ([k objects]
   (or (not (feature-set? k ::f-container objects))
       (feature-set? k ::f-open objects)
       (feature-set? k ::f-transparent objects))))

(defn visible-descendants
  "every key inside k that can actually be seen, not descending into shut
  containers."
  ([k] (visible-descendants k @OBJECTS))
  ([k objects]
   (loop [frontier (vec (contents k objects))
          seen #{}]
     (if-let [x (first frontier)]
       (if (seen x)
         (recur (subvec frontier 1) seen)
         (recur (cond-> (subvec frontier 1)
                  (see-inside? x objects) (into (contents x objects)))
                (conj seen x)))
       seen))))

(defn lit?
  "true if k is lit: k gives off light itself, or something visible inside it
  does. (a lamp shut inside an opaque box lights nothing.)"
  ([k] (lit? k @OBJECTS))
  ([k objects]
   (or (feature-set? k ::f-lit objects)
       (boolean (some #(feature-set? % ::f-lit objects)
                      (visible-descendants k objects))))))

(defn- disj-child [objects parent-k k]
  (update-in objects [parent-k kw-contains-local] disj k))

(defn- conj-child [objects parent-k k]
  (update-in objects [parent-k kw-contains-local] (fnil conj #{}) k))

(defn- relocate
  "the containment half of place!/move!: k out of wherever it was, into k-to."
  [objects k k-to]
  (let [from (location-of k objects)]
    (cond-> objects
      from (disj-child from k)
      true (conj-child k-to k))))

(defn place!
  "put k into k-to and record nothing else about it. one swap!, so the one-parent
  invariant is never observably broken.

  this is the engine's own placement primitive, for establishing a world rather
  than acting in one -- `boot!` uses it to seat the actor. game code almost always
  wants `move!` instead, which additionally notes that k has been disturbed."
  [k k-to]
  (swap! OBJECTS relocate k k-to)
  k)

(defn move!
  "put k into k-to, taking it out of wherever it was (ZIL's MOVE). one swap!,
  so the one-parent invariant is never observably broken.

  `place!` plus bookkeeping: also marks k ::f-touched, which is what retires its
  `fdesc`. the author never sets that by hand -- an object's first description
  should stop being used once the object has been disturbed, and that is not a
  thing worth remembering."
  [k k-to]
  (swap! OBJECTS
         (fn [objects]
           (cond-> (relocate objects k k-to)
             (contains? objects k) (update-in [k kw-features]
                                              (fnil conj #{}) ::f-touched))))
  k)

(defn remove!
  "take k out of the containment tree; its location becomes nil (ZIL's REMOVE)."
  [k]
  (swap! OBJECTS
         (fn [objects]
           (if-let [from (location-of k objects)]
             (disj-child objects from k)
             objects)))
  k)

;; ---------------------------------------------------------------------------
;; properties and features
;; ---------------------------------------------------------------------------

(defn swap-object-attr
  "basically, swaps the value of the attribute of the given key for the object with (apply fun current-value-of-attribute args)"
  [object-key attr-key fun & args]
  (swap! OBJECTS update-in [object-key attr-key] #(apply fun % args)))

(defn set-feature [object-key feature]
  (swap-object-attr object-key kw-features (fnil conj #{}) feature))

(defn clear-feature [object-key feature]
  (swap-object-attr object-key kw-features (fnil disj #{}) feature))

(defn open! "open a door or container." [k] (set-feature k ::f-open) k)
(defn shut! "shut a door or container." [k] (clear-feature k ::f-open) k)

(defn set-actor! [actor-key]
  (reset! ACTOR actor-key))

;; ---------------------------------------------------------------------------
;; tell!
;; ---------------------------------------------------------------------------

(defn cr? [x] (= :>> x))

(defn- capitalize-first
  "upcase the first character only. clojure.string/capitalize would downcase
  the rest, which ruins a label like \"Green Door\"."
  [s]
  (if (string/blank? s)
    s
    (str (string/upper-case (subs s 0 1)) (subs s 1))))

(defn- vowel-initial? [s]
  (boolean (re-find #"(?i)^[aeiou]" (str s))))

(defn indefinite-article
  "\"a\" or \"an\", inferred from the label. the features only exist to override
  the inference for the words English disagrees with -- `vowel` for an hour, an
  MP; `consonant` for a unicorn, a one-way door.

  ZIL needed VOWELBIT on every single vowel-initial object because it could not
  afford to look at the string. we can look, so the author only marks exceptions
  -- and, more to the point, forgetting to mark one no longer prints \"a apple\"."
  ([k label] (indefinite-article k label @OBJECTS))
  ([k label objects]
   (cond
     (feature-set? k ::f-vowel-article objects) "an"
     (feature-set? k ::f-consonant-article objects) "a"
     (vowel-initial? label) "an"
     :else "a")))

(defn stringify-tell-token
  "render tell!-token `token` against the object KEY `k`, or nil if `token` is
  not a tell! token at all."
  ([token k] (stringify-tell-token token k @OBJECTS))
  ([token k objects]
   (let [label (o:label k objects)]
     (case token
       :a (if (feature-set? k ::f-no-article objects)
            label
            (str (indefinite-article k label objects) " " label))
       :the (if (feature-set? k ::f-no-article objects)
              label
              (str "the " label))
       (:A :The) (capitalize-first
                   (stringify-tell-token (keyword (string/lower-case (name token)))
                                         k
                                         objects))
       nil))))

(def tell-macro-forms ;todo add this to tell. this will take some work.
  {'a []
   'the []
   'A []
   'The []
   '> []
   }
)

(defn tell!
  "print a message, item by item. a tell!-token (:a :the :A :The) consumes the
  item after it, which must be an object KEY, and prints that object's label
  with the appropriate article. :>> is a carriage return; two in a row give a
  blank line. anything else prints as-is.
  usage: (tell! \"I don't think \" :the ::green-wall \" would agree with you.\" :>>)
         (tell! \"Staring at \" :a ::enemy \"? Dangerous!\" :>>)
  returns ::handled, so a handler whose last act is a tell! reports that it
  handled the input."
  [& msg]
  (let [objects @OBJECTS]
    (loop [items msg]
      (when-let [[x & more] (seq items)]
        (if (cr? x)
          (do (newline)
              (recur more))
          (if-let [rendered (and (seq more) (stringify-tell-token x (first more) objects))]
            (do (print rendered)
                (recur (rest more)))
            (do (print x)
                (recur more)))))))
  ::handled)

;; ---------------------------------------------------------------------------
;; the turn in flight
;; ---------------------------------------------------------------------------
;; Some things a handler learns are facts about the TURN, not answers to the
;; question it was asked. "the clock should not advance" is not a response to
;; "did you handle this input", and sending both down one return channel is what
;; ZIL tried first and abandoned (Learning ZIL 8.4): every intermediate routine
;; had to forward the marker faithfully, which was "lots of extra code and lots
;; of chances to screw up." They replaced it with state settable from any depth.
;;
;; THE LIST OF WHAT LIVES IN HERE IS CLOSED. Keep it that way.
;;
;;   :time-passed?   does this input advance the clock, so that ev-each-turn (and
;;                   later, daemons) run. ZIL's GAME-VERB?.
;;
;; That is the entire list. To qualify, a fact must be MEANINGLESS OUTSIDE ONE
;; TURN. Things that look tempting and do not qualify:
;;
;;   score           a quantity belonging to the GAME, not to a turn -- it
;;                   outlives every turn. a global atom, like OBJECTS and ACTOR.
;;                   (ZIL's INCREMENT-SCORE is a SETG on a global and prints its
;;                   notification inline, with no end-of-turn batching.)
;;   move count      likewise game-scoped.
;;   what "it" means  persists into the NEXT turn -- "DROP IT" -- so by
;;                   definition not turn-scoped. ZIL's THIS-IS-IT.
;;   daemon queue    game-scoped.
;;   :handled?
;;   :over?          outcomes `turn!` computes, not signals anybody records. they
;;                   live in turn!'s return value instead, which keeps the
;;                   invariant that everything in here was put here on purpose by
;;                   something during the turn.
;;
;; The one plausible future addition is :output? -- did anything print -- which a
;; WAIT verb needs to stop a multi-turn wait early, and which `tell!` would have
;; to set from a place that has no context. NOT added, because nothing consumes
;; it. Anything beyond that deserves an argument first.
;;
;; `record-turn!` is private on purpose. The public surface is one named fn per
;; fact, so this list cannot quietly grow.

(def ^:const default-turn-state
  {:time-passed? true})

(def ^:dynamic *turn*
  "the turn in flight, as an atom, bound by `turn!`. dynamic so that code at any
  depth can record a fact about the turn without every frame in between having to
  cooperate. nil outside a turn. see the note above for what may go in it, and
  why that list is closed."
  nil)

(defn turn-state
  "what has been recorded about the turn in flight. nil outside a turn."
  []
  (when *turn* @*turn*))

(defn- record-turn!
  [k v]
  (when *turn* (swap! *turn* assoc k v))
  nil)

(defn no-time-passes!
  "this input doesn't advance the clock, so ev-each-turn won't fire. ZIL's
  GAME-VERB?: meta-verbs like VERBOSE, SAVE, SCORE.

  a no-op outside a turn, so a handler called straight from the repl or a scratch
  script doesn't blow up."
  []
  (record-turn! :time-passed? false))

;; death is NOT one of these flags, and that is the one place ZIL's \"fatal\"
;; lumped together two unrelated things. a flag is a note the turn carries to its
;; end; death is an abort -- once the actor is dead, the rest of the handler and
;; the rest of the chain must not run, or you get "You are crushed by the
;; boulder. Taken." so it throws. ZIL's JIGS-UP didn't return either.

(defn die!
  "end the game, printing `msg` (already-rendered text -- use `say` if the line
  belongs in petra.engine.text). aborts the turn; `turn!` catches it."
  [& msg]
  (throw (ex-info "the game is over"
                  {::game-over true
                   ::message (apply str msg)})))

(defn game-over?
  "true if `e` is what die! throws, rather than a real fault."
  [e]
  (boolean (::game-over (ex-data e))))

;; ---------------------------------------------------------------------------
;; the turn context
;; ---------------------------------------------------------------------------
;; every fn the engine calls out to -- responder, describer, notification,
;; pre-action, verb default -- takes exactly this one map. what a return value
;; MEANS depends on where the fn was installed, not on anything in the context:
;;
;;   ::handler              responder    truthy = I consumed the input, stop the chain
;;   ::description-detailed describer    a string, or nil to decline
;;   ::on {event fn}        notification return value is discarded
;;
;; nothing is told why it is being called, because nothing is installed for more
;; than one reason. that is the whole of what ZIL needed RARG for.

(defn context
  "build a turn context. `k-self` is whoever is about to be called.
  rebuild rather than reuse: :objects is a snapshot, and anything called earlier
  in a turn may have moved things since."
  ([] (context {} nil))
  ([base k-self]
   (let [objects @OBJECTS
         k-actor @ACTOR]
     (assoc base
            :self k-self
            :k-actor k-actor
            :k-here (room-of k-actor objects)
            :objects objects))))
;; note: the turn in flight is deliberately NOT in here. one door, `*turn*`, via
;; the named fns above -- two paths to one atom invited raw swaps that bypassed
;; the closed list, and gave only the handlers that skip the `handler` macro any
;; visibility in exchange.

;; ---------------------------------------------------------------------------
;; describers
;; ---------------------------------------------------------------------------

(defn description
  "k's detailed description: a string, or nil if k has nothing special to say
  and the caller should fall back to a default.

  the property may hold a plain string or a fn of ctx, so a description that
  changes with the world costs nothing extra to write. ZIL needed the M-OBJDESC?
  query pass only because its describers printed as they went and so could not
  ask what an object *would* say without saying it -- returning the string
  instead of printing it collapses that pair into one call."
  ([k] (description k (context)))
  ([k ctx]
   (let [d (prop k kw-description-detailed (:objects ctx))]
     (if (fn? d)
       (d (assoc ctx :self k))
       d))))

;; ---------------------------------------------------------------------------
;; text frames
;; ---------------------------------------------------------------------------
;; the engine contains no player-facing prose. every line it can print lives in
;; petra.engine.text keyed by id, and gets filled in here. see that namespace for the
;; slot syntax.

(def FRAMES (atom text/FRAMES))

(defn set-frames! [frames] (reset! FRAMES frames))

(defn merge-frames!
  "override some frames and leave the rest, so a game can retune a few lines
  without restating the whole set."
  [frames]
  (swap! FRAMES merge frames))

(def ^:private slot-re #"\{\{\s*(.+?)\s*\}\}")

(def ^:private slot-tokens
  {"a" :a "an" :a "A" :A "An" :A "the" :the "The" :The})

(defn- render-slot [spec args objects]
  (let [parts (string/split spec #"\s+")
        [tok nm] (if (= 1 (count parts)) [nil (first parts)] parts)
        arg-key (keyword nm)]
    (if-not (contains? args arg-key)
      (str "[?" nm "]")                                     ; visible in playtest, not silent
      (let [v (get args arg-key)]
        (cond
          (nil? tok) (str v)
          (= "label" tok) (str (o:label v objects))
          (slot-tokens tok) (str (stringify-tell-token (slot-tokens tok) v objects))
          :else (str v))))))

(defn fill
  "fill a frame string's {{slots}} from `args`. `args` may carry an :objects
  snapshot; otherwise the current world is read."
  [s args]
  (let [objects (get args :objects @OBJECTS)]
    (string/replace s slot-re (fn [[_ spec]] (render-slot spec args objects)))))

(defn say
  "the finished English for frame `id`. returns a string -- printing is tell!'s
  job -- so every line the engine emits can be asserted on in a test."
  ([id] (say id {}))
  ([id args]
   (let [f (get @FRAMES id ::no-such-frame)]
     (cond
       (= f ::no-such-frame) (str "[missing text frame " id "]")
       (fn? f) (str (f args))
       :else (fill f args)))))

(def ^:const v-brief ::brief)                               ; long description on a first visit only
(def ^:const v-verbose ::verbose)                           ; long description every time
(def ^:const v-superbrief ::superbrief)                     ; room name only, and no contents

(def VERBOSITY (atom v-brief))

(defn set-verbosity! [mode] (reset! VERBOSITY mode))

(defn describe-object
  "the line describing k as it lies where it lies, or nil if k has nothing
  specific to say (in which case a caller should fold it into a stock listing),
  or nil if k should not be listed at all.

  `fdesc` is used until the object has been moved, then `desc` takes over --
  which is the whole of what ZIL's TOUCHBIT/FDESC dance did, minus the dance."
  ([k] (describe-object k (context)))
  ([k ctx]
   (let [objects (:objects ctx)]
     (when-not (feature-set? k ::f-no-describe objects)
       (or (when-not (feature-set? k ::f-touched objects)
             (prop k kw-description-first objects))
           (description k ctx))))))

(defn- oxford-join
  "punctuate a list of already-rendered phrases. the punctuation itself is
  authorable -- see the ::list-* frames in petra.engine.text."
  [items]
  (case (count items)
    0 nil
    1 (first items)
    2 (str (first items) (say ::text/list-two) (second items))
    (str (string/join (say ::text/list-separator) (butlast items))
         (say ::text/list-last)
         (last items))))

(defn contents-in-order
  "k's contents in a stable order. ::contains-local is a set, so the describers
  sort -- by label, so that the order of the English is at least explicable from
  the English, rather than from whatever the internal keywords happen to be. the
  key breaks ties, so the result is fully deterministic either way.

  an author who wants a specific order should write the prose into the room's own
  `desc` and mark those objects `no-desc`."
  ([k] (contents-in-order k @OBJECTS))
  ([k objects]
   (sort-by (fn [c] [(or (o:label c objects) "") (str c)])
            (contents k objects))))

(defn- contents-clause
  "the ::container-holds line for k, or nil if k holds nothing you can see."
  [k ctx]
  (let [objects (:objects ctx)]
    (when (see-inside? k objects)
      (when-let [inner (seq (keep #(when-not (feature-set? % ::f-no-describe objects)
                                     (stringify-tell-token :a % objects))
                                  (contents-in-order k objects)))]
        (say ::text/container-holds
             {:container k :items (oxford-join inner) :objects objects})))))

(defn describe-contents
  "prose for everything visible inside k: first every object with something
  specific to say, in order, then one stock sentence gathering the rest so you
  never get five \"There is a X here.\" lines in a row. nil if there is nothing
  to say at all.

  ZIL made three passes here, and needed the M-OBJDESC? query pass to learn in
  advance which objects intended to describe themselves. because describe-object
  hands back its line instead of printing it, one pass does: ask each object
  once, then partition on the answers."
  ([k] (describe-contents k (context)))
  ([k ctx]
   (let [objects (:objects ctx)
         ;; the actor is in the room but is not scenery in it. skipping it here
         ;; saves every game from having to mark its own player object no-desc.
         children (remove #(or (= % (:k-actor ctx))
                               (feature-set? % ::f-no-describe objects))
                          (contents-in-order k objects))
         answered (map (fn [c] [c (describe-object c ctx)]) children)
         spoken (for [[c line] answered :when line]
                  (string/join " " (remove nil? [line (contents-clause c ctx)])))
         mute (for [[c line] answered :when (nil? line)] c)
         gathered (when-let [items (seq (map #(stringify-tell-token :a % objects) mute))]
                    (string/join " "
                                 (cons (say ::text/contents-listing
                                            {:items (oxford-join items) :objects objects})
                                       (keep #(contents-clause % ctx) mute))))
         all (remove nil? (concat spoken [gathered]))]
     (when (seq all)
       (string/join " " all)))))

(defn describe-room
  "the full description of room k, as a string: its name, its long description if
  that is warranted, then its contents. `:full?` forces the long description --
  that is the player having typed LOOK. otherwise it appears on a first visit, or
  every time in verbose mode."
  ([k] (describe-room k (context) {}))
  ([k ctx] (describe-room k ctx {}))
  ([k ctx {:keys [full?]}]
   (let [objects (:objects ctx)]
     (if-not (lit? k objects)
       (say ::text/too-dark)
       (let [verbosity @VERBOSITY
             brief-only? (= verbosity v-superbrief)
             long? (and (not brief-only?)
                        (or full?
                            (= verbosity v-verbose)
                            (not (feature-set? k ::f-visited objects))))]
         (->> [(o:label k objects)
               (when long? (description k ctx))
               (when-not brief-only? (describe-contents k ctx))]
              (remove nil?)
              (string/join "\n")))))))

(defn look!
  "print where the actor is, and mark the room visited. the one place a room
  description reaches the screen -- everything above it returns strings, so it
  can all be tested without capturing output."
  ([] (look! true))
  ([full?]
   (let [ctx (context)
         k (:k-here ctx)]
     (when k
       (tell! (describe-room k ctx {:full? full?}) :>>)
       (set-feature k ::f-visited))
     ::handled)))

;; ---------------------------------------------------------------------------
;; events
;; ---------------------------------------------------------------------------
;; the events the engine itself raises. an ::on map is open: a game may register
;; its own namespaced events and raise them with notify!.

(def ^:const ev-enter ::enter)                              ; the actor just entered you
(def ^:const ev-leave ::leave)                              ; the actor is about to leave you
(def ^:const ev-each-turn ::each-turn)                      ; a turn ended while the actor was in you

(defn listener
  "the fn k has registered for `event`, or nil."
  ([k event] (listener k event @OBJECTS))
  ([k event objects] (get (prop k kw-on objects) event)))

(defn notify!
  "tell k that `event` happened. always returns nil: a notification's return
  value carries no meaning, which is what keeps it from being confused with a
  responder's. ZIL had no such separation, so an interrupt had to remember to
  RTRUE purely to tell V-WAIT that it had printed something."
  ([k event] (notify! k event (context)))
  ([k event ctx]
   (when-let [f (listener k event (:objects ctx))]
     (f (assoc ctx :self k)))
   nil))

;; ---------------------------------------------------------------------------
;; movement
;; ---------------------------------------------------------------------------

(defn goto!
  "move the actor into room `k-room` and describe it. this is fiat, not an
  attempt: nothing here can refuse. ZIL drew the same line -- \"DO-WALK is just
  an attempt... GOTO overrides all that, however, and positively sends the player
  to the given room.\"

  the order is load-bearing, which is the reason this lives in the engine rather
  than being any old handler that calls move!:

    ev-leave   before the move, so a leave listener still sees the room it is
               losing, and sees it with the actor still in it
    move!      before ev-enter, so an enter listener's k-here is already here
    look!      last, because an enter listener may change what there is to see --
               ZIL's crypt moves a poltergeist in on M-ENTER and expects the
               description that follows to mention it

  a listener cannot veto: notifications discard their return value, and refusing
  a move is the job of a conditional exit or of the room's own responder, which
  runs in the chain before movement ever happens. (ZIL's TORTURE-CHAMBER-F is
  exactly that: an M-BEG clause that eats WALK while the player is strapped down.)

  actor-only, since `look!` describes wherever the actor is. moving anything else
  is `move!` plus whatever notifying you want by hand.

  going to the room the actor is already in is not special-cased: leave and enter
  both fire. returns the room key."
  [k-room]
  (let [k-actor @ACTOR]
    ;; author errors are loud. an exit pointing at a typo would otherwise land the
    ;; actor somewhere room-of can't resolve, leaving k-here nil and the game
    ;; quietly describing nothing at all.
    (when-not k-actor
      (throw (ex-info "goto! with no actor set" {:hint "call set-actor! first"})))
    (when-not (in? k-room ROOMS)
      (throw (ex-info "goto! target is not a room"
                      {:target k-room
                       :hint "rooms are made with def-room, which is what puts them in ROOMS"
                       :known-rooms (vec (sort (contents ROOMS)))})))
    (when-let [from (room-of k-actor)]
      (notify! from ev-leave))
    (move! k-actor k-room)
    (notify! k-room ev-enter)
    ;; arriving is not a LOOK: full? stays false so brief mode still shows the
    ;; long description only on a first visit, which is the whole job of
    ;; ::f-visited. (look!) with no args means the player typed LOOK and wants
    ;; everything -- Learning ZIL 11.3.
    (look! false)
    k-room))

;; ---------------------------------------------------------------------------
;; verbs
;; ---------------------------------------------------------------------------
;; A verb is a namespaced keyword; the registry maps it to behaviour. That
;; indirection is the whole point: `(= verb ::take)` becomes possible, and many
;; input words can name one internal verb -- ZIL's SLICE -> V-CUT.
;;
;; The registry maps KEYWORD -> BEHAVIOUR. A lexicon will map WORDS -> KEYWORD.
;; Synonymy is therefore entirely lexical and has no business here, which is what
;; ZIL's VERB-SYNONYM looks like it gets wrong until you notice it lives in the
;; syntax file rather than the verbs file.

(def VERBS (atom {}))

(def verb-symbols
  {'handle kw-handler                                       ; same key as objects: same contract
   'pre    kw-pre-handler                                    ; ZIL's PRE-, and it belongs to the VERB
   'turn?  kw-turn?})                                        ; default true; say false for meta-verbs

(defn verb-def
  ([v] (verb-def v @VERBS))
  ([v verbs] (get verbs v)))

(defn verb-prop [v attr] (get (verb-def v) attr))

(defn verb-handler
  "the verb's default -- the last resort in the chain. required, so always there."
  [v]
  (verb-prop v kw-handler))

(defn pre-action
  "the verb's pre-action, or nil. In ZIL this was declared per-syntax, and 9.4 is
  explicit that a PRSA must carry the SAME pre-action across all of its syntaxes --
  so it is a property of the verb, not of a call site."
  [v]
  (verb-prop v kw-pre-handler))

(defn consumes-turn?
  "does this verb advance the clock? true unless the verb says otherwise. ZIL's
  GAME-VERB?, except stated once on the verb instead of kept as a list."
  [v]
  (boolean (get (verb-def v) kw-turn? true)))

(defn make-verb [v properties]
  (swap! VERBS assoc v properties)
  v)

(defmacro def-verb
  "define a verb.

    (def-verb ::take   handle v-take)
    (def-verb ::shoot  pre pre-shoot  handle v-shoot)
    (def-verb ::save   turn? false    handle v-save)

  `handle` is required: a verb with no last resort leaves inputs unanswered, and
  a non-response is always a no-no (Learning ZIL 1.2). A one-line stub is fine."
  [verb-key & properties]
  (when-not (even? (count properties))
    (throw (ex-info "def-verb needs property/value pairs" {:verb verb-key})))
  (let [pairs (partition 2 properties)]
    (when-not (some #{'handle} (map first pairs))
      (throw (ex-info "a verb needs a `handle`"
                      {:verb verb-key
                       :hint "without a default, inputs using this verb go unanswered"})))
    (let [m (into {}
                  (for [[k v] pairs]
                    (if-let [verb-key' (get verb-symbols k)]
                      [verb-key' v]
                      (throw (ex-info "Unknown verb property"
                                      {:property k :verb verb-key
                                       :known (vec (sort (keys verb-symbols)))})))))]
      `(make-verb ~verb-key ~m))))

;; ---------------------------------------------------------------------------
;; PERFORM
;; ---------------------------------------------------------------------------

(defn try-handle
  "give k's responder a crack at the input. nil if k has no responder at all --
  which perform! wants to be indistinguishable from a responder declining."
  [k ctx]
  (when-let [h (and k (handler-of k (:objects ctx)))]
    (h ctx)))

(defn perform-internal!
  [verb k-dobj k-iobj direction]
  (when-not (verb-def verb)
    (throw (ex-info "unknown verb"
                    {:verb verb
                     :hint "define it with def-verb"
                     :known (vec (sort (keys @VERBS)))})))
  (let [pre-verb (pre-action verb)
        base {:verb verb :pre-verb pre-verb
              :k-dobj k-dobj :k-iobj k-iobj :direction direction}
        respond (fn [k-self]
                  (when k-self
                    (try-handle k-self (context base k-self))))
        ret (or
              ;; the actor gets the first crack. the ACTOR atom is ZIL's WINNER:
              ;; usually the player, but the addressee while you are talking to
              ;; someone. a responder that cares can ask (= self k-actor) --
              ;; ZIL's M-WINNER carried no more information than that.
              (respond @ACTOR)
              ;; then the room the actor is in. this is ZIL's M-BEG, which was a
              ;; distinct tag only because the room's one routine also served
              ;; M-LOOK/M-ENTER/M-END and had to tell them apart.
              (respond (room-of @ACTOR))
              ;; then the verb's pre-action, which exists to get in ahead of the
              ;; objects rather than behind them like the verb default
              (and pre-verb (pre-verb (context base nil)))
              ;; then the indirect object, then the direct object. a responder
              ;; that needs to know which it is asks (= self k-iobj) / (= self k-dobj).
              (respond k-iobj)
              (respond k-dobj)
              ;; and last, the verb default
              ((verb-handler verb) (context base nil)))]
    ;; one meaning only: did anything consume the input. anything a handler wants
    ;; to say about the turn itself went to *turn*, not down this channel.
    (boolean ret)))

(defn perform!
  "run the responder chain for one input and return whether anything consumed it.
  this is ZIL's PERFORM, including the habit of calling it yourself from inside a
  handler to re-dispatch an input as some other verb. it does no turn
  bookkeeping -- that is `turn!`."
  [verb & {:keys [dobj iobj direction]}]
  (perform-internal! verb dobj iobj direction))

;; ---------------------------------------------------------------------------
;; the turn
;; ---------------------------------------------------------------------------

(defn turn!
  "run one whole turn: hand the input to the responder chain, then, if the clock
  advanced, let the room know the turn ended.

  takes what the parser (plus whatever engine-side post-parsing) worked out: a
  verb, and optionally a direct object, indirect object and pre-action. a turn is
  a turn; this doesn't care where they came from.

  returns {:time-passed? :handled? :over?} -- the signals recorded during the
  turn, plus the two outcomes this fn computes -- so whatever drives turns can
  decide what to do next."
  [verb & {:keys [dobj iobj direction]}]
  (binding [*turn* (atom (assoc default-turn-state
                                :time-passed? (consumes-turn? verb)))]
    (try
      (let [handled? (perform-internal! verb dobj iobj direction)]
        (when (:time-passed? (turn-state))
          (when-let [k-here (room-of @ACTOR)]
            (notify! k-here ev-each-turn)))
        (assoc (turn-state) :handled? handled? :over? false))
      (catch clojure.lang.ExceptionInfo e
        (if-not (game-over? e)
          (throw e)
          (do (tell! (::message (ex-data e)) :>>)
              (tell! (say ::text/died) :>>)
              ;; dying is emphatically handling the input
              (assoc (turn-state) :handled? true :over? true)))))))

;; ---------------------------------------------------------------------------
;; exits
;; ---------------------------------------------------------------------------
;; An exit compiles to DATA, not a closure: one spec map per direction, holding
;; what the author declared. `resolve-exit` is the only thing that interprets it,
;; and it computes rather than acts -- it returns a result and prints nothing.
;;
;; That purity is load-bearing. It is what lets a pre-action resolve an exit,
;; intervene (open a door it has the key to), decline, and let the verb default
;; resolve again -- ZIL's implicit-take pattern, 9.6. Under the old
;; thunk-that-prints design, resolving twice printed the refusal twice.
;;
;; The result's keys are engine business. Game code asks named questions instead:
;; exit-to, exit-message, exit-exists?, exit-door, exit-permanent?, exit-notes.
;;
;;   {::to    <room-key>   ; RESOLVED destination. the actor may go.
;;    ::say   <string>     ; refusal text, rendered. the actor may not.
;;    ::run   <fn>         ; UNDECIDED: a `with` exit. the verb must run it.
;;    ::spec  <map>        ; the exit AS DECLARED. absent iff no exit that way.
;;    ::notes <map>}       ; the game's own annotations. omitted when empty.
;;
;; Exactly one of ::to / ::say / ::run is present.
;;
;; ::run is why this stays a query even though a `with` fn may do anything at all:
;; resolution never runs it, it only hands it over. So an automap or a curious
;; handler can resolve every exit in a room for fun, and nothing happens.
;;
;; Note ::spec may carry its own ::to -- the destination as *declared* -- which is
;; a different fact from the resolved one. `[east ::hall if FLAG]` with FLAG false
;; declares ::hall but resolves to a refusal, so reading the declared destination
;; as though it were the resolved one would walk the actor through a closed gate.
;; That is why the spec is nested rather than merged, and why exit-to (resolved)
;; and exit-destination (declared) are different accessors.

(defn exit-to
  "the room the actor may go to, or nil if the attempt was refused."
  [r] (::to r))

(defn exit-message
  "what to say when the actor may not go, or nil when they may."
  [r] (::say r))

(defn exit-exists?
  "was any exit declared in that direction at all? distinguishes a refusal from
  walking into a blank wall."
  [r] (some? (::spec r)))

(defn exit-door
  "the door gating this exit, or nil. about the DECLARATION, not the outcome: it
  answers \"is a door involved\", so it returns the door whether or not the door
  is what refused."
  [r] (::via (::spec r)))

(defn exit-permanent?
  "true for a `never` exit -- a refusal no change in the world will ever lift."
  [r] (contains? (::spec r) ::never))

(defn exit-handler
  "for a `with` exit, the fn to run; nil otherwise. Its contract is a responder's,
  exactly like an object's `handle` or a verb's: truthy means it dealt with the
  attempt. It may move the actor, print, mutate the world, stop the clock, or kill
  them -- anything a handler may do.

  It is already wrapped so that a decline still says something; see wrap-exit-fn."
  [r] (::run r))

(defn exit-destination
  "where the exit nominally leads, AS DECLARED -- which is not where the actor may
  go. Use exit-to for that. This is for cataloguing: it is the only thing a map can
  say about a `with` exit."
  [r] (::to (::spec r)))

(defn exit-has-destination?
  "did the author declare a destination at all? `never` exits never do, and `with`
  exits need not."
  [r] (contains? (::spec r) ::to))

(defn exit-notes
  "the game's own annotations on this exit, keyed by the game's own keywords."
  [r] (or (::notes r) {}))

(defn exit-directions
  "every direction declared from k-room. for prose, or for drawing a map."
  ([k-room] (exit-directions k-room @OBJECTS))
  ([k-room objects] (set (keys (prop k-room kw-room-exits objects)))))

(defn direction-to
  "the direction from k-room whose exit is DECLARED to lead to k-dest, or nil.
  For turning \"go to the chapel\" into a direction.

  Matches on the declared destination, not the resolved one, so it finds an exit
  whose gate happens to be shut and finds `with` exits too."
  ([k-room k-dest] (direction-to k-room k-dest @OBJECTS))
  ([k-room k-dest objects]
   (some (fn [[dir spec]] (when (= k-dest (::to spec)) dir))
         (prop k-room kw-room-exits objects))))

(defn- refusal
  "a refusal may be written as a literal string or as a text-frame id."
  [x]
  (when x (if (keyword? x) (say x) x)))

(defn wrap-exit-fn
  "wrap an author's `with` fn so that declining still says something.

  The fn's contract is a responder's, so nil means \"I did not deal with this\" --
  and then somebody has to speak or the turn goes silent, which 1.2 forbids. Doing
  that here rather than in the verb keeps the ::cant-go frame inside the engine,
  where all engine prose lives. A good exit fn handles its own refusals, so in
  practice this fallback almost never fires.

  Called from the code def-object emits. Not something a game writes."
  [f]
  (fn [ctx] (or (f ctx) (tell! (say ::text/cant-go) :>>))))

(defn resolve-exit
  "what happens if the actor tries to go `dir` from `k-room`. Computes; does not
  move anything, does not print, and does not run a `with` fn -- so it is safe to
  call speculatively, from anywhere, with no turn in flight. See the schema above,
  and prefer the exit-* accessors to reading the result directly."
  ([k-room dir] (resolve-exit k-room dir @OBJECTS))
  ([k-room dir objects]
   (let [spec (get (prop k-room kw-room-exits objects) dir)
         notes (::notes spec)
         out (fn [m] (cond-> (assoc m ::spec (dissoc spec ::notes))
                       (seq notes) (assoc ::notes notes)))]
     (cond
       (nil? spec)               {::say (say ::text/cant-go)}
       (contains? spec ::never)  (out {::say (refusal (::never spec))})
       (contains? spec ::with)   (out {::run (::with spec)})
       (contains? spec ::via)    (if (feature-set? (::via spec) ::f-open objects)
                                   (out {::to (::to spec)})
                                   (out {::say (or (refusal (::else spec))
                                                   (say ::text/door-shut
                                                        {:door (::via spec) :objects objects}))}))
       (contains? spec ::if)     (if @(::if spec)
                                   (out {::to (::to spec)})
                                   (out {::say (or (refusal (::else spec)) (say ::text/cant-go))}))
       :else                     (out {::to (::to spec)})))))

;; ---------------------------------------------------------------------------
;; the object-definition DSL
;; ---------------------------------------------------------------------------

(defn unwrap-symbol [x] ; safe unquoting. handles 'x and (quote x).
  (if (and (seq? x)
           (= 'quote (first x)))
    (second x)
    x))

(def direction-symbols ; dsl names for the directions the engine knows
  {'north kw-north
   'east  kw-east
   'south kw-south
   'west  kw-west
   'up    kw-up
   'down  kw-down
   'in    kw-in
   'out   kw-out})

(def exit-option-symbols ; dsl names for the gates and their alternatives
  {'if    ::if                                              ; gate: an atom that must be truthy
   'via   ::via                                             ; gate: a door that must be open
   'never ::never                                           ; gate: never, with a reason
   'with  ::with                                            ; gate: a fn that decides
   'or    ::else})                                          ; the message when a gate refuses

(def ^:private exit-gates [::if ::via ::never ::with])

(defn- compile-exit
  "one `[dir dest? & option/note pairs]` form -> [dir-key spec-map].

  Runs at macroexpansion, so every mistake below is a compile error rather than a
  surprise mid-game. The destination is positional and OPTIONAL: room keys are
  keywords and option names are bare symbols, so whether one was given is decided
  by looking at the second element. `never` has no destination -- often there is no
  room on the other side at all -- and `with` needs none, since the fn decides.

  A destination given alongside `with` is a HINT ONLY. Nothing reads it at
  resolution; the fn must goto! for itself. It exists so that a `with` exit can
  still be catalogued -- see exit-destination, which is the only thing a map or a
  parser can learn about one."
  [exit]
  (let [[dir & more] exit
        dir-sym (unwrap-symbol dir)
        dir-key (or (get direction-symbols dir-sym)
                    (when (keyword? dir-sym) dir-sym)       ; a game's own direction
                    (throw (ex-info "unknown direction"
                                    {:direction dir
                                     :known (vec (sort (keys direction-symbols)))})))
        declared? (and (seq more) (not (symbol? (unwrap-symbol (first more)))))
        dest (when declared? (first more))
        opts (if declared? (rest more) more)
        _ (when (odd? (count opts))
            (throw (ex-info "exit options must be name/value pairs"
                            {:direction dir :options (vec opts)})))
        spec (reduce (fn [m [k v]]
                       (let [sym (unwrap-symbol k)]
                         (cond
                           (= sym 'with) (assoc m ::with (list `wrap-exit-fn v))
                           (get exit-option-symbols sym) (assoc m (get exit-option-symbols sym) v)
                           (keyword? sym)                (assoc-in m [::notes sym] v)
                           :else (throw (ex-info "unknown exit option"
                                                 {:option k :direction dir
                                                  :known (vec (sort (keys exit-option-symbols)))
                                                  :hint "a game's own annotations must be keywords"})))))
                     {}
                     (partition 2 opts))
        gates (filterv #(contains? spec %) exit-gates)]
    (when (> (count gates) 1)
      (throw (ex-info "an exit may have at most one gate"
                      {:direction dir :gates gates
                       :hint "use `with` for a compound condition"})))
    (when (and (contains? spec ::never) dest)
      (throw (ex-info "a `never` exit takes no destination"
                      {:direction dir :destination dest
                       :hint "there is often no room on the other side to name"})))
    (when (and (contains? spec ::else) (not (some #{::if ::via} gates)))
      (throw (ex-info "`or` needs an `if` or a `via` to be the alternative to"
                      {:direction dir})))
    (when (and (nil? dest) (not (some #{::never ::with} gates)))
      (throw (ex-info "this exit needs a destination"
                      {:direction dir
                       :hint "only `never` and `with` may omit one"})))
    [dir-key (cond-> spec dest (assoc ::to dest))]))

(defn to-preproc
  "the `to` property: a vector of exit forms -> {direction spec}. Pure data, so a
  room's exits stay inspectable -- see exit-directions."
  [exits]
  (into {} (map compile-exit exits)))

(def feature-symbols ; dsl names for the features the engine understands
  {'vowel       ::f-vowel-article
   'consonant   ::f-consonant-article
   'no-article  ::f-no-article
   'open        ::f-open
   'container   ::f-container
   'transparent ::f-transparent
   'lit         ::f-lit
   'no-desc     ::f-no-describe
   'touched     ::f-touched
   'visited     ::f-visited})

(defn features-preproc
  "DSL-translate a features vector. a bare symbol naming an engine feature becomes
  that feature's keyword; anything else passes through, so a game can carry its
  own features in the same set."
  [fs]
  (into #{}
        (map (fn [f] (let [s (unwrap-symbol f)] (get feature-symbols s s))) fs)))

(def event-symbols ; dsl names for the events the engine raises
  {'enter ev-enter
   'leave ev-leave
   'each-turn ev-each-turn})

(defn on-preproc
  "DSL-translate the keys of an `on` map. a bare symbol naming an engine event
  becomes that event's keyword; anything else passes through untouched, so a game
  can key the map on its own namespaced events."
  [event-map]
  (into {}
        (map (fn [[k v]]
               (let [s (unwrap-symbol k)]
                 [(get event-symbols s s) v]))
             event-map)))

;each key is a DSL symbol for a property, and the value is a function that returns a map of implementation-level properties that should be generated through the use of the DSL prop. For a compile-time prop of e.g. "foo bar", the function at 'foo is called as with arguments [bar]. a single property may generate a map of any size; e.g., a single DSL property may correspond to multiple implementation properties (just return e.g. {:one 1 :two 2}).
(def prop-symbols-pre {
                   'label (fn [x] {kw-label x})
                   ;; bare symbols: (features [lit open]), or your own keywords
                   'features (fn [fs] {kw-features (features-preproc fs)})
                   ;; the responder: truthy return means "I consumed the input"
                   'handle (fn [f] {kw-handler f})
                   'noun (fn [heads] {kw-label-heads (apply hash-set heads)})
                   'adj (fn [mods] {kw-label-modifiers (apply hash-set mods)})
                   'fdesc (fn [x] {kw-description-first x})
                   ;; a string, or a fn of ctx returning a string or nil
                   'desc (fn [x] {kw-description-detailed x})
                   ;; notifications, keyed by event: {enter <fn> each-turn <fn>}
                   'on (fn [event-map] {kw-on (on-preproc event-map)})
                   'share (fn [keys] {kw-contains-shared (apply hash-set keys)})
                   ;; `contains` is the authoritative containment relation, so a
                   ;; parent may list children that are not defined yet.
                   'contains (fn [keys] {kw-contains-local (apply hash-set keys)})
                   ;; compiles to {direction spec}; see compile-exit
                   'to (fn [exits] {kw-room-exits (to-preproc exits)})})

(defn make-object [k properties]
  (swap! OBJECTS assoc k properties)
  k)

(defmacro def-object
  "define an object. `object-key` is evaluated, so both a keyword literal and a
  symbol naming one (ROOMS, SHARED, ...) work and keep their namespace.
  note that this REPLACES any existing definition, ::contains-local included --
  define a container before the things move! puts into it."
  [object-key & properties]
  (when-not (even? (count properties))
    (throw (ex-info "def-object needs property/value pairs"
                    {:object object-key :properties properties})))
  (let [compiled-props
        (into {}
              (for [[prop raw] (partition 2 properties)]
                (if-let [compiler-f (get prop-symbols-pre prop)]
                  (compiler-f raw)
                  (throw (ex-info "Unknown property"
                                  {:property prop :object object-key})))))]
    `(make-object ~object-key ~compiled-props)))

(defmacro def-room
  "define a room: a def-object that is also registered as held by ROOMS. that
  membership is what room-of climbs toward, so a room defined with def-object
  alone will not be found as anybody's location."
  [room-key & properties]
  `(let [k# (def-object ~room-key ~@properties)]
     (swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) k#)
     k#))

;; terser aliases, for world files
(defmacro object [& forms] `(def-object ~@forms))
(defmacro room [& forms] `(def-room ~@forms))

;; the root containers must exist before anything is placed in them.
(def-object ROOMS)
(def-object SHARED)
(def-object GLOBALS)
(def-object INTANGIBLES)

;; ---------------------------------------------------------------------------
;; the game
;; ---------------------------------------------------------------------------
;; A game declares what it IS and says nothing about being run. `def-game`
;; compiles to a plain map under a var called CONFIG, which is all a runner needs
;; to find; the game folder therefore holds no entry point, no boot call, and no
;; reference to a runner. Dependencies point one way only: game -> engine.
;;
;; The keys here are universal behaviours the engine interprets, so they are
;; engine keywords -- but authored as bare symbols, like every other property in
;; this DSL. Add to `config-symbols` as more of them turn out to be universal.

(def config-symbols
  {'title  ::title                                          ; what the game is called
   'author ::author                                          ; who to credit
   'actor  ::actor                                           ; the object the player IS
   'start  ::start})                                         ; the room they begin in

(defmacro def-game
  "declare a game. compiles to `(def CONFIG {...})` in the current namespace --
  pure data, which is what a runner reads.

    (def-game
      title  \"The Gatehouse\"
      author \"you\"
      actor  ::you
      start  ::gatehouse)"
  [& properties]
  (when-not (even? (count properties))
    (throw (ex-info "def-game needs property/value pairs" {:properties properties})))
  (let [m (into {}
                (for [[k v] (partition 2 properties)]
                  (if-let [config-key (get config-symbols k)]
                    [config-key v]
                    (throw (ex-info "Unknown game property"
                                    {:property k
                                     :known (vec (sort (keys config-symbols)))})))))]
    `(def ~'CONFIG ~m)))

(defn boot!
  "start a game from its CONFIG: announce it, become the actor, arrive in the
  starting room. This is ZIL's GO routine, which likewise printed the opening
  text and then did a LOOK before handing off to the main loop.

  `actor` and `start` are structurally required -- without them there is nobody to
  be and nowhere to be it -- so their absence throws. A missing `title` or
  `author` merely renders as [?title] via the banner frame, loud but survivable.

  The actor's starting position is stated in ONE place, `start`, and the author
  never puts the actor into the world by hand. Placing it is the engine's job, and
  deliberately so: whether the actor ends up held by the room, or by a vehicle in
  it, or by local-globals, is an implementation matter the author shouldn't have to
  have an opinion about -- and would have to revise if the engine changed its mind.
  So `boot!` requires the actor to be held by nothing at all, and puts it where the
  config says.

  Note there is no goto! here, and so no ev-leave/ev-enter: beginning somewhere is
  not the same as arriving there, and firing either on a room you never left or
  entered would be a lie. ZIL's GO likewise did a LOOK rather than a GOTO. Placing
  the actor is a plain write to the world; only look! runs.

  Boots once, on a world nobody has placed the actor in. Calling it twice throws,
  which is the right answer -- restarting means rebuilding the world, not booting
  a used one again.

  Returns the config, so a runner can keep reading it."
  [config]
  (let [k-actor (::actor config)
        k-start (::start config)]
    (when-not k-actor
      (throw (ex-info "game config has no `actor`"
                      {:config config :hint "(def-game actor ::you ...)"})))
    (when-not k-start
      (throw (ex-info "game config has no `start`"
                      {:config config :hint "(def-game start ::some-room ...)"})))
    ;; goto! used to catch this on our behalf; without it the check has to be here,
    ;; or a `start` naming an object surfaces as a vaguer complaint about the actor
    (when-not (in? k-start ROOMS)
      (throw (ex-info "game `start` is not a room"
                      {:start k-start
                       :hint "rooms are made with def-room, which is what puts them in ROOMS"
                       :known-rooms (vec (sort (contents ROOMS)))})))
    ;; one pass over the whole containment tree, answering two questions at once
    (let [index (parent-index)]
      ;; nothing anywhere may sit in two places
      (when-let [bad (seq (containment-problems index))]
        (throw (ex-info "objects held by more than one parent"
                        {:problems (into {} (map (fn [[k ps]] [k (vec (sort ps))]) bad))
                         :hint "each object belongs in exactly one `contains`"})))
      ;; and the actor may not be placed in the world at all -- that's boot!'s job
      (when-let [holders (get index k-actor)]
        (throw (ex-info "the actor is already placed in the world"
                        {:actor k-actor
                         :held-by (vec (sort holders))
                         :declared-start k-start
                         :hint (str "remove the actor from `contains`; its starting "
                                    "position is the config's `start`")}))))
    (tell! (say ::text/banner {:title (::title config) :author (::author config)}) :>>)
    (set-actor! k-actor)
    (place! k-actor k-start)                                ; a plain write: not a goto!,
                                                            ; and nothing was disturbed
    (look!)
    config))
