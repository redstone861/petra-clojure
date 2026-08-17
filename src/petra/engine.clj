(ns petra.engine
  (:require [clojure.string :as string]
            [petra.text :as text]))

;; ---------------------------------------------------------------------------
;; property keys
;; ---------------------------------------------------------------------------

(def ^:const kw-label ::label)
(def ^:const kw-room-exits ::exits)
(def ^:const kw-handler ::handler)
(def ^:const kw-pre-handler ::pre-handler)
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

(defn move!
  "put k into k-to, taking it out of wherever it was (ZIL's MOVE). one swap!,
  so the one-parent invariant is never observably broken.

  also marks k ::f-touched, which is what retires its `fdesc`. the author never
  sets that by hand -- an object's first description should stop being used once
  the object has been disturbed, and that is not a thing worth remembering."
  [k k-to]
  (swap! OBJECTS
         (fn [objects]
           (let [from (location-of k objects)]
             (cond-> objects
               from (disj-child from k)
               true (conj-child k-to k)
               (contains? objects k) (update-in [k kw-features]
                                                (fnil conj #{}) ::f-touched)))))
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
;; petra.text keyed by id, and gets filled in here. see that namespace for the
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
  authorable -- see the ::list-* frames in petra.text."
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
;; PERFORM
;; ---------------------------------------------------------------------------

;; values a responder can return that perform! must pass up to the main loop
(def ^:const pf-fatal ::pf-fatal)                            ; flush the rest of the input line (ZIL's M-FATAL / P-CONT -1)
(def ^:const pf-dead ::pf-dead)                              ; the actor died

(defn perform-pass-up?
  "returns true if the value is one that perform! should pass up to its caller
  (i.e. the perform had a result that should affect the main loop in some way)."
  [ret]
  (contains? #{pf-fatal pf-dead} ret))

(defn try-handle
  "give k's responder a crack at the input. nil if k has no responder at all --
  which perform! wants to be indistinguishable from a responder declining."
  [k ctx]
  (when-let [h (and k (handler-of k (:objects ctx)))]
    (h ctx)))

(defn perform-internal!
  [verb pre-verb k-dir k-ind]
  (let [base {:verb verb :pre-verb pre-verb :k-dir k-dir :k-ind k-ind}
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
              ;; that needs to know which it is asks (= self k-ind) / (= self k-dir).
              (respond k-ind)
              (respond k-dir)
              ;; and last, the verb default
              (verb (context base nil)))]
    (if (perform-pass-up? ret)
      ret
      false)))                                              ; TODO ev-each-turn raised at end of mainloop

(defn perform!
  [verb & {:keys [dir ind pre]}]
  (perform-internal! verb pre dir ind))

;; ---------------------------------------------------------------------------
;; exits
;; ---------------------------------------------------------------------------

(defn tell-door-cant-go [door-key]
  (tell! (say ::text/door-shut {:door door-key}) :>>))

(defn- no-exit
  "an exit that refuses. tell! returns a truthy ::handled, so a rejecting exit
  has to swallow that and report false, or the caller would read it as a room."
  [tell-thunk]
  (tell-thunk)
  false)

(defn exit-in-direction
  "Takes a direction keyword and some arguments (see design doc),
  returning [dir-key exitfn] where (exitfn) returns the new room or false if no movement should occur."
  ;if [atom]: conditional with global flag atom
    ;else [msg]: reject with message (otherwise, default) - never use else w/o if or if-open
  ;with [fun]: function-exit
  ;if-open [door]: conditional with door
  ;never [msg]: reject with message
  [dir-key room-key & {if-atom :if
                       else-msg :else
                       with-fun :with
                       if-open-object-key :if-open
                       never-msg :never}]
  (cond
    if-atom
    [dir-key #(if @if-atom
                room-key
                (no-exit (fn [] (tell! (or else-msg (say ::text/cant-go)) :>>))))]

    with-fun
    [dir-key #(with-fun room-key)]

    if-open-object-key
    [dir-key #(if (open? if-open-object-key)
                room-key
                (no-exit (fn [] (if else-msg
                                  (tell! else-msg :>>)
                                  (tell-door-cant-go if-open-object-key)))))]

    never-msg
    [dir-key #(no-exit (fn [] (tell! never-msg :>>)))]

    ;; unconditional. still a thunk, so that every exit has one shape.
    :else
    [dir-key (constantly room-key)]))

(defn with-to [exits]
   (into {} (map #(apply exit-in-direction %) exits))
  )

;; ---------------------------------------------------------------------------
;; the object-definition DSL
;; ---------------------------------------------------------------------------

(defn unwrap-symbol [x] ; safe unquoting. handles 'x and (quote x).
  (if (and (seq? x)
           (= 'quote (first x)))
    (second x)
    x))

(def to-preproc-map ; dsl keys.
  {'if    :if
   'or  :else
   'with  :with
   'via   :if-open
   'never :never
   'north kw-north
   'east kw-east
   'south kw-south
   'west kw-west
   'up kw-up
   'down kw-down
   'in kw-in
   'out kw-out})

(defn to-preproc-single ; given single to-spec vector, DSL-translate
  [exit]
  (mapv
    (fn [x]
      (let [s (unwrap-symbol x)]
        (get to-preproc-map s x)))
    exit)
)

(defn to-preproc ; given vec of to-spec vectors, DSL-translate the directions and keywords
  [exits]
  (let [result (mapv to-preproc-single exits)]
    result)
)

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
                   'pre (fn [f] {kw-pre-handler f})
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
                   'to (fn [exits] {kw-room-exits (to-preproc exits)})}) ; needs postproc

(def prop-keys-post {
                     kw-room-exits (fn [processed] (with-to processed))
                     })

(defn postprocess-props [properties]
  (reduce-kv (fn [new-m k v]
               (assoc new-m k ((get prop-keys-post k identity) v))) ; postprocess (or, if no key exists, ->)
             {} ;; Initial empty map
             properties))

(defn make-object [k properties]
  (swap! OBJECTS assoc k (postprocess-props properties))
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
