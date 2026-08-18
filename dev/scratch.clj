;; Scratch harness. Not a test suite -- no lein test hookup, nothing here is
;; load-bearing, and it is meant to be rewritten or thrown away freely as the
;; engine's shape changes. It exists only so that exercising the engine doesn't
;; start from nothing every session.
;;
;;   lein run -m clojure.main dev/scratch.clj

(require 'petra.test-game.dungeon 'petra.engine.core 'petra.engine.text)
(in-ns 'petra.engine.core)
(alias 'd 'petra.test-game.dungeon)

(def fails (atom 0))
(defn chk [label expected actual]
  (when-not (= expected actual) (swap! fails inc))
  (println (if (= expected actual) "  ok " "FAIL ") label "=>" (pr-str actual)
           (if (= expected actual) "" (str "\n        EXPECTED " (pr-str expected)))))

(set-actor! ::d/you)
(move! ::d/you ::d/god-kingdom)

;; verbs are keywords now, so a throwaway test verb needs a name
(def verb-counter (atom 0))
(defn as-verb!
  ([f] (as-verb! f {}))
  ([f props]
   (let [k (keyword "petra.engine.core" (str "tv-" (swap! verb-counter inc)))]
     (make-verb k (assoc props kw-handler f))
     k)))

(println "\n=== perform! means one thing again: did anything consume the input ===")
(make-object ::mute {kw-label "mute thing"})
(make-object ::speaker {kw-label "speaker" kw-handler (fn [_] ::handled)})
(chk "nobody handled it"        false (perform! (as-verb! (fn [_] nil)) :dobj ::mute))
(chk "an object handled it"     true  (perform! (as-verb! (fn [_] nil)) :dobj ::speaker))
(chk "the verb default handled it" true (perform! (as-verb! (fn [_] ::handled)) :dobj ::mute))

(println "\n=== turn! reports the turn, and raises ev-each-turn ===")
(def ticks (atom 0))
(swap! OBJECTS assoc-in [::d/god-kingdom kw-on ev-each-turn] (fn [_] (swap! ticks inc)))
(reset! ticks 0)
(chk "a normal turn" {:time-passed? true :handled? true :over? false}
     (turn! (as-verb! (fn [_] ::handled))))
(chk "the room heard the turn end" 1 @ticks)
(chk "an unhandled turn still passes time" {:time-passed? true :handled? false :over? false}
     (turn! (as-verb! (fn [_] nil))))
(chk "so the room heard that one too" 2 @ticks)

(println "\n=== no-time-passes! -- and from arbitrary depth, the whole point ===")
(reset! ticks 0)
(chk "a handler suppresses the clock" {:time-passed? false :handled? true :over? false}
     (turn! (as-verb! (fn [_] (no-time-passes!) ::handled))))
(chk "so no ev-each-turn" 0 @ticks)

;; a plain helper: no ctx, no idea it's inside a turn, three frames deep
(defn- meta-verb? [] (no-time-passes!) true)
(defn- deep-helper [] (meta-verb?))
(defn- deeper-still [] (deep-helper))
(reset! ticks 0)
(chk "recorded from three frames down, with no ctx threaded"
     {:time-passed? false :handled? true :over? false}
     (turn! (as-verb! (fn [_] (deeper-still) ::handled))))
(chk "clock still suppressed" 0 @ticks)

(println "\n=== turn state is per-turn and doesn't leak ===")
(reset! ticks 0)
(turn! (as-verb! (fn [_] (no-time-passes!) ::handled)))
(chk "the next turn starts clean" {:time-passed? true :handled? true :over? false}
     (turn! (as-verb! (fn [_] ::handled))))
(chk "and that one ticked" 1 @ticks)
(chk "outside a turn, turn-state is nil" nil (turn-state))
(chk "and recording is a harmless no-op" nil (no-time-passes!))

(println "\n=== one door only ===")
(chk "the turn is NOT in the context" false
     (contains? (context {} nil) :turn))
(chk "record-turn! is private, so the closed list can't quietly grow" true
     (:private (meta #'record-turn!)))
(chk "the turn carries exactly one recorded signal" [:time-passed?]
     (keys default-turn-state))

(println "\n=== die! is an abort, not a flag ===")
(def reached (atom []))
(make-object ::doomed
             {kw-label "doomed thing"
              kw-handler (fn [_] (swap! reached conj :dir) ::handled)})
(reset! ticks 0)
(reset! reached [])
(swap! OBJECTS assoc-in [::d/you kw-handler]
       (fn [_] (die! "The boulder finds you at last.")))
(let [out (atom nil)
      st (with-out-str (reset! out (turn! (as-verb! (fn [_] (swap! reached conj :verb-default) ::handled))
                                        :dobj ::doomed)))]
  (chk "turn comes back over?" true (:over? @out))
  (chk "nothing further in the chain ran" [] @reached)
  (chk "no ev-each-turn after death" 0 @ticks)
  (println "  --- screen ---")
  (print (clojure.string/replace st #"(?m)^" "  | "))
  (chk "printed the circumstance then the frame"
       "The boulder finds you at last.\n*** You have died. ***\n" st))

(println "=== die! from depth, and a real fault still propagates ===")
(defn- swallowed-by-a-grue [] (die! "You are eaten by a grue."))
(swap! OBJECTS assoc-in [::d/you kw-handler] (fn [_] (swallowed-by-a-grue)))
(let [st (atom nil)]
  (with-out-str (reset! st (turn! (as-verb! (fn [_] ::handled)))))
  (chk "die! works from a helper too" true (:over? @st)))
(swap! OBJECTS assoc-in [::d/you kw-handler] (fn [_] (throw (ex-info "boom" {:real-bug true}))))
(chk "a genuine ExceptionInfo is not swallowed as death" "boom"
     (try (turn! (as-verb! (fn [_] ::handled))) :no-throw
          (catch clojure.lang.ExceptionInfo e (ex-message e))))
(swap! OBJECTS update ::d/you dissoc kw-handler)

(println "\n=== the death line is authorable like any other ===")
(merge-frames! {::text/died "*** Your adventure ends here. ***"})
(swap! OBJECTS assoc-in [::d/you kw-handler] (fn [_] (die! "The floor gives way.")))
(println (clojure.string/replace (with-out-str (turn! (as-verb! (fn [_] ::handled))))
                                 #"(?m)^" "  | "))
(set-frames! petra.engine.text/FRAMES)
(swap! OBJECTS update ::d/you dissoc kw-handler)

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

;; ---------------------------------------------------------------------------
;; movement
;; ---------------------------------------------------------------------------

(println "\n=== goto! ordering: leave -> move -> enter -> look ===")
(def log (atom []))
(set-actor! ::d/you)
(move! ::d/you ::d/god-kingdom)
(set-frames! petra.engine.text/FRAMES)

;; watch the sequence, and prove each step sees what it should
(swap! OBJECTS assoc-in [::d/god-kingdom kw-on ev-leave]
       (fn [ctx] (swap! log conj [:leave (:self ctx) :actor-still-here?
                                  (in? ::d/you (:self ctx))])))
(swap! OBJECTS assoc-in [::d/green-room kw-on ev-enter]
       (fn [ctx] (swap! log conj [:enter (:self ctx) :k-here (:k-here ctx)])
         ;; an enter listener changes what there is to see; the look after it must notice
         (move! ::d/hourglass ::d/green-room)))
(reset! log [])
(let [ret (atom nil)
      out (with-out-str (reset! ret (goto! ::d/green-room)))]
  (chk "returns the room key" ::d/green-room @ret)
  (chk "leave fired before the move, with the actor still in the old room"
       [:leave ::d/god-kingdom :actor-still-here? true] (first @log))
  (chk "enter fired after the move, k-here already the new room"
       [:enter ::d/green-room :k-here ::d/green-room] (second @log))
  (chk "exactly two notifications" 2 (count @log))
  (chk "the actor moved" ::d/green-room (room-of ::d/you))
  (println "  --- screen ---")
  (print (clojure.string/replace out #"(?m)^" "  | "))
  (chk "look! ran, and saw what the enter listener added" true
       (boolean (re-find #"an hourglass" out)))
  (chk "and the long description showed on a first visit" true
       (boolean (re-find #"Moss carpets" out))))

(println "=== f-visited still belongs to look!, not goto! ===")
(let [out (with-out-str (goto! ::d/god-kingdom))
      back (with-out-str (goto! ::d/green-room))]
  (chk "revisiting drops the long description" false
       (boolean (re-find #"Moss carpets" back)))
  (chk "but still names the room" true (boolean (re-find #"Green Room" back)))
  (chk "an explicit look! still forces the long description back" true
       (boolean (re-find #"Moss carpets" (with-out-str (look!))))))

(println "=== a leave listener cannot veto ===")
(swap! OBJECTS assoc-in [::d/green-room kw-on ev-leave] (fn [_] false))
(with-out-str (goto! ::d/god-kingdom))
(chk "returning false changed nothing" ::d/god-kingdom (room-of ::d/you))
(swap! OBJECTS update ::d/green-room update kw-on dissoc ev-leave)
(swap! OBJECTS update ::d/god-kingdom update kw-on dissoc ev-leave)
(swap! OBJECTS update ::d/green-room update kw-on dissoc ev-enter)

(println "=== author errors are loud ===")
(chk "a typo'd destination throws rather than teleporting into limbo" true
     (try (goto! ::d/grene-room) false
          (catch clojure.lang.ExceptionInfo e (= ::d/grene-room (:target (ex-data e))))))
(chk "so does pointing an exit at an object instead of a room" true
     (try (goto! ::d/rusty-pail) false
          (catch clojure.lang.ExceptionInfo e (some? (:known-rooms (ex-data e))))))
(chk "and going nowhere with no actor set" true
     (let [saved @ACTOR]
       (set-actor! nil)
       (try (goto! ::d/green-room) false
            (catch clojure.lang.ExceptionInfo e (some? (:hint (ex-data e))))
            (finally (set-actor! saved)))))
(chk "the actor never moved during any of that" ::d/god-kingdom (room-of ::d/you))

(println "=== goto! composes with the exit thunks it doesn't know about ===")
(with-out-str (goto! ::d/aqua-room))
(let [exits (prop ::d/aqua-room kw-room-exits)]
  (clear-feature ::d/green-door ::f-open)
  (let [r (atom nil) out (with-out-str (reset! r ((get exits ::down))))]
    (chk "shut door refuses, so nothing to goto!" false @r)
    (chk "...having said why itself" true (boolean (re-find #"is closed" out))))
  (set-feature ::d/green-door ::f-open)
  (let [dest ((get exits ::down))]
    (chk "open door yields a room" ::d/cellar dest)
    (with-out-str (goto! dest))
    (chk "and goto! took us there" ::d/cellar (room-of ::d/you))))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))


;; ---------------------------------------------------------------------------
;; boot
;; ---------------------------------------------------------------------------

(require 'petra.test-game.game)
(alias 'g 'petra.test-game.game)

(defn- why "[message data] for the ex-info f throws, or [:no-throw nil]." [f]
  (try (f) [:no-throw nil]
       (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))

(println "\n=== the author never places the actor; boot! does ===")
(remove! ::d/you)                                        ; undo the wandering above
(clear-feature ::d/you ::f-touched)                      ; ...including its move! bookkeeping
(chk "the actor starts held by nothing" nil (location-of ::d/you))
(chk "nothing is in two places at once" {} (containment-problems))
(with-out-str (boot! g/CONFIG))
(chk "boot! put it where the config says" ::d/god-kingdom (location-of ::d/you))
(chk "and made it the actor" ::d/you @ACTOR)
(chk "seating the actor disturbed nothing" false (feature-set? ::d/you ::f-touched))

(println "\n=== place! is move! without the bookkeeping ===")
(make-object ::pebble {kw-label "pebble"})
(place! ::pebble ::d/aqua-room)
(chk "place! relocates" ::d/aqua-room (location-of ::pebble))
(chk "...and notes nothing" false (feature-set? ::pebble ::f-touched))
(move! ::pebble ::d/god-kingdom)
(chk "move! relocates too" ::d/god-kingdom (location-of ::pebble))
(chk "...and notes the disturbance" true (feature-set? ::pebble ::f-touched))
(chk "both leave exactly one parent" 1 (count (get (parent-index) ::pebble)))
(remove! ::pebble)

(println "\n=== an author who places the actor by hand is told off ===")
(let [[msg data] (why #(boot! g/CONFIG))]              ; still placed from the boot above
  (chk "already placed -> throws" "the actor is already placed in the world" msg)
  (chk "and names who is holding it" [::d/god-kingdom] (:held-by data)))
(chk "so boot! is once-only, on a fresh world" true (some? (location-of ::d/you)))

(println "\n=== the one pass also catches the invariant `contains` can break ===")
(remove! ::d/you)
(make-object ::twofer {kw-label "twofer"})                ; deliberately listed by two rooms
(swap! OBJECTS update-in [::d/god-kingdom kw-contains-local] (fnil conj #{}) ::twofer)
(swap! OBJECTS update-in [::d/aqua-room kw-contains-local] (fnil conj #{}) ::twofer)
(chk "both parents spotted" #{::d/god-kingdom ::d/aqua-room}
     (get (containment-problems) ::twofer))
(chk "boot! refuses" "objects held by more than one parent"
     (first (why #(boot! g/CONFIG))))
(swap! OBJECTS update-in [::d/god-kingdom kw-contains-local] disj ::twofer)
(swap! OBJECTS update-in [::d/aqua-room kw-contains-local] disj ::twofer)
(chk "clean again" {} (containment-problems))

(println "\n=== bad config ===")
(chk "no actor" "game config has no `actor`" (first (why #(boot! {::start ::d/god-kingdom}))))
(chk "no start" "game config has no `start`" (first (why #(boot! {::actor ::d/you}))))
(chk "start names an object, not a room" "game `start` is not a room"
     (first (why #(boot! {::actor ::d/you ::start ::d/rusty-pail}))))

(println "\n=== beginning somewhere is not arriving there ===")
(def boot-seen (atom []))
(remove! ::d/you)
(swap! OBJECTS assoc-in [::d/god-kingdom kw-on ev-enter] (fn [_] (swap! boot-seen conj :enter)))
(swap! OBJECTS assoc-in [::d/god-kingdom kw-on ev-leave] (fn [_] (swap! boot-seen conj :leave)))
(with-out-str (boot! g/CONFIG))
(chk "no ev-enter, no ev-leave" [] @boot-seen)
(swap! OBJECTS update ::d/god-kingdom update kw-on dissoc ev-enter)
(swap! OBJECTS update ::d/god-kingdom update kw-on dissoc ev-leave)

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

;; ---------------------------------------------------------------------------
;; verbs
;; ---------------------------------------------------------------------------

(println "\n=== def-verb compiles to a plain map in the registry ===")
(def-verb ::v-plain handle (fn [_] ::handled))
(chk "registered under its keyword" true (some? (verb-def ::v-plain)))
(chk "handle lands on the same key objects use" true
     (fn? (verb-handler ::v-plain)))
(chk "no pre by default" nil (pre-action ::v-plain))
(chk "consumes a turn by default" true (consumes-turn? ::v-plain))

(def-verb ::v-meta turn? false handle (fn [_] ::handled))
(chk "turn? false is respected" false (consumes-turn? ::v-meta))
(chk "and the author writes nothing for the default case" true (consumes-turn? ::v-plain))

(println "\n=== turn! seeds the clock from the verb, no no-time-passes! needed ===")
(chk "a meta-verb doesn't advance the clock"
     {:time-passed? false :handled? true :over? false} (turn! ::v-meta))
(chk "an ordinary one does"
     {:time-passed? true :handled? true :over? false} (turn! ::v-plain))
(chk "and no-time-passes! still overrides, for a one-off"
     false (:time-passed? (turn! (as-verb! (fn [_] (no-time-passes!) ::handled)))))

(println "\n=== pre-actions belong to the verb, and reach the chain ===")
(def order (atom []))
(def-verb ::v-pre
  pre    (fn [_] (swap! order conj :pre) nil)
  handle (fn [_] (swap! order conj :default) ::handled))
(reset! order [])
(turn! ::v-pre)
(chk "pre ran before the default" [:pre :default] @order)
(chk "pre-action is looked up, not passed in" true (fn? (pre-action ::v-pre)))
(reset! order [])
(chk "a pre that handles stops the chain" true
     (do (make-verb ::v-pre-stops {kw-pre-handler (fn [_] (swap! order conj :pre) ::handled)
                                   kw-handler     (fn [_] (swap! order conj :default) ::handled)})
         (turn! ::v-pre-stops)
         (= [:pre] @order)))

(println "\n=== ctx carries the verb keyword, its pre, and a direction ===")
(def seen-ctx (atom nil))
(def-verb ::v-probe
  pre    (fn [_] nil)
  handle (fn [ctx] (reset! seen-ctx ctx) ::handled))
(turn! ::v-probe :dobj ::d/rusty-pail :iobj ::d/tin-cup :direction kw-north)
(chk "verb is the keyword" ::v-probe (:verb @seen-ctx))
(chk "pre-verb is the verb's own pre" true (identical? (pre-action ::v-probe) (:pre-verb @seen-ctx)))
(chk "direct object" ::d/rusty-pail (:k-dobj @seen-ctx))
(chk "indirect object" ::d/tin-cup (:k-iobj @seen-ctx))
(chk "direction" ::north (:direction @seen-ctx))

(println "\n=== a responder can group verbs by name now ===")
(def-verb ::v-a handle (fn [_] nil))
(def-verb ::v-b handle (fn [_] nil))
(make-object ::picky
             {kw-label "picky thing"
              kw-handler (fn [{:keys [verb]}]
                           (when (#{::v-a ::v-b} verb) (tell! "Not those." :>>)))})
(defn- consumed? [v] (let [r (atom nil)] (with-out-str (reset! r (perform! v :dobj ::picky))) @r))
(chk "a named verb in the set is consumed" true (consumed? ::v-a))
(def-verb ::v-c handle (fn [_] nil))                     ; a default that declines
(chk "one outside it falls through, and nothing else claims it" false (consumed? ::v-c))

(println "\n=== author errors are loud ===")
(defn- boom [f] (try (f) :no-throw (catch Throwable e (str (ex-message e) (ex-message (or (ex-cause e) e))))))
(chk "unknown verb at dispatch" true
     (boolean (re-find #"unknown verb" (boom #(perform! ::no-such-verb)))))
(chk "a verb with no handle won't compile" true
     (boolean (re-find #"needs a `handle`" (boom #(eval '(petra.engine.core/def-verb ::nope turn? false))))))
(chk "an unknown verb property won't compile" true
     (boolean (re-find #"Unknown verb property"
                       (boom #(eval '(petra.engine.core/def-verb ::nope2 wibble 1 handle (fn [_] nil)))))))
(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))
