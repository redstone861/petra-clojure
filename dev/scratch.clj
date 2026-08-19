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

(println "\n=== `with` exits are handed over, not run ===")
(def ran (atom 0))
(def-verb ::v-w
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (resolve-exit k-here direction objects)]
             (cond
               (exit-to r)      (goto! (exit-to r))
               (exit-handler r) ((exit-handler r) ctx)
               :else            (tell! (exit-message r) :>>)))
           ::handled))

(make-object ::wroom
             {kw-label "W Room"
              kw-features #{::f-lit}
              kw-room-exits {::north {::to ::d/aqua-room
                                      ::with (wrap-exit-fn
                                              (fn [_] (swap! ran inc) ::handled))}
                             ::south {::with (wrap-exit-fn (fn [_] (swap! ran inc) nil))}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::wroom)
(remove! ::d/you) (place! ::d/you ::wroom) (set-actor! ::d/you)

(reset! ran 0)
(let [r (resolve-exit ::wroom ::north)]
  (chk "resolving does NOT run the fn" 0 @ran)
  (chk "no resolved destination -- it is undecided" nil (exit-to r))
  (chk "nothing to say either" nil (exit-message r))
  (chk "it hands over a fn" true (fn? (exit-handler r)))
  (chk "the declared destination is still catalogued" ::d/aqua-room (exit-destination r))
  (chk "and reports that it has one" true (exit-has-destination? r)))
(let [r (resolve-exit ::wroom ::south)]
  (chk "a with exit may declare no destination" false (exit-has-destination? r))
  (chk "so exit-destination is nil" nil (exit-destination r)))
(chk "resolving repeatedly still runs nothing" 0
     (do (dotimes [_ 5] (resolve-exit ::wroom ::north)) @ran))

(println "\n=== ...and run once, by the verb ===")
(with-out-str (turn! ::v-w :direction ::north))
(chk "the verb ran it exactly once" 1 @ran)

(println "\n=== a declining fn still gets an answer, from the engine ===")
(reset! ran 0)
(chk "wrap-exit-fn supplies the fallback" "You can't go that way.\n"
     (with-out-str (turn! ::v-w :direction ::south)))
(chk "and the author's fn did run" 1 @ran)

(println "\n=== the freedoms a with fn now has ===")
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::east]
       {::with (wrap-exit-fn (fn [_] (no-time-passes!) ::handled))})
(chk "it can stop the clock" false (:time-passed? (turn! ::v-w :direction ::east)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::west]
       {::with (wrap-exit-fn (fn [_] (die! "The floor was never there.")))})
(chk "it can end the game" true
     (let [st (atom nil)] (with-out-str (reset! st (turn! ::v-w :direction ::west))) (:over? @st)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::in]
       {::with (wrap-exit-fn (fn [_] (tell! "One." :>> "Two." :>>) ::handled))})
(chk "it can print several lines and not move you" "One.\nTwo.\n"
     (with-out-str (turn! ::v-w :direction ::in)))
(chk "...and the actor really did not move" ::wroom (room-of ::d/you))

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

(println "=== goto! composes with resolved exits ===")
(with-out-str (goto! ::d/aqua-room))
(open! ::d/green-door)
(let [r (resolve-exit ::d/aqua-room ::down)]
  (chk "resolution yields a room" ::d/cellar (exit-to r))
  (with-out-str (goto! (exit-to r)))
  (chk "and goto! took us there" ::d/cellar (room-of ::d/you)))

(println "\n=== `with` exits are handed over, not run ===")
(def ran (atom 0))
(def-verb ::v-w
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (resolve-exit k-here direction objects)]
             (cond
               (exit-to r)      (goto! (exit-to r))
               (exit-handler r) ((exit-handler r) ctx)
               :else            (tell! (exit-message r) :>>)))
           ::handled))

(make-object ::wroom
             {kw-label "W Room"
              kw-features #{::f-lit}
              kw-room-exits {::north {::to ::d/aqua-room
                                      ::with (wrap-exit-fn
                                              (fn [_] (swap! ran inc) ::handled))}
                             ::south {::with (wrap-exit-fn (fn [_] (swap! ran inc) nil))}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::wroom)
(remove! ::d/you) (place! ::d/you ::wroom) (set-actor! ::d/you)

(reset! ran 0)
(let [r (resolve-exit ::wroom ::north)]
  (chk "resolving does NOT run the fn" 0 @ran)
  (chk "no resolved destination -- it is undecided" nil (exit-to r))
  (chk "nothing to say either" nil (exit-message r))
  (chk "it hands over a fn" true (fn? (exit-handler r)))
  (chk "the declared destination is still catalogued" ::d/aqua-room (exit-destination r))
  (chk "and reports that it has one" true (exit-has-destination? r)))
(let [r (resolve-exit ::wroom ::south)]
  (chk "a with exit may declare no destination" false (exit-has-destination? r))
  (chk "so exit-destination is nil" nil (exit-destination r)))
(chk "resolving repeatedly still runs nothing" 0
     (do (dotimes [_ 5] (resolve-exit ::wroom ::north)) @ran))

(println "\n=== ...and run once, by the verb ===")
(with-out-str (turn! ::v-w :direction ::north))
(chk "the verb ran it exactly once" 1 @ran)

(println "\n=== a declining fn still gets an answer, from the engine ===")
(reset! ran 0)
(chk "wrap-exit-fn supplies the fallback" "You can't go that way.\n"
     (with-out-str (turn! ::v-w :direction ::south)))
(chk "and the author's fn did run" 1 @ran)

(println "\n=== the freedoms a with fn now has ===")
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::east]
       {::with (wrap-exit-fn (fn [_] (no-time-passes!) ::handled))})
(chk "it can stop the clock" false (:time-passed? (turn! ::v-w :direction ::east)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::west]
       {::with (wrap-exit-fn (fn [_] (die! "The floor was never there.")))})
(chk "it can end the game" true
     (let [st (atom nil)] (with-out-str (reset! st (turn! ::v-w :direction ::west))) (:over? @st)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::in]
       {::with (wrap-exit-fn (fn [_] (tell! "One." :>> "Two." :>>) ::handled))})
(chk "it can print several lines and not move you" "One.\nTwo.\n"
     (with-out-str (turn! ::v-w :direction ::in)))
(chk "...and the actor really did not move" ::wroom (room-of ::d/you))

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

(println "\n=== `with` exits are handed over, not run ===")
(def ran (atom 0))
(def-verb ::v-w
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (resolve-exit k-here direction objects)]
             (cond
               (exit-to r)      (goto! (exit-to r))
               (exit-handler r) ((exit-handler r) ctx)
               :else            (tell! (exit-message r) :>>)))
           ::handled))

(make-object ::wroom
             {kw-label "W Room"
              kw-features #{::f-lit}
              kw-room-exits {::north {::to ::d/aqua-room
                                      ::with (wrap-exit-fn
                                              (fn [_] (swap! ran inc) ::handled))}
                             ::south {::with (wrap-exit-fn (fn [_] (swap! ran inc) nil))}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::wroom)
(remove! ::d/you) (place! ::d/you ::wroom) (set-actor! ::d/you)

(reset! ran 0)
(let [r (resolve-exit ::wroom ::north)]
  (chk "resolving does NOT run the fn" 0 @ran)
  (chk "no resolved destination -- it is undecided" nil (exit-to r))
  (chk "nothing to say either" nil (exit-message r))
  (chk "it hands over a fn" true (fn? (exit-handler r)))
  (chk "the declared destination is still catalogued" ::d/aqua-room (exit-destination r))
  (chk "and reports that it has one" true (exit-has-destination? r)))
(let [r (resolve-exit ::wroom ::south)]
  (chk "a with exit may declare no destination" false (exit-has-destination? r))
  (chk "so exit-destination is nil" nil (exit-destination r)))
(chk "resolving repeatedly still runs nothing" 0
     (do (dotimes [_ 5] (resolve-exit ::wroom ::north)) @ran))

(println "\n=== ...and run once, by the verb ===")
(with-out-str (turn! ::v-w :direction ::north))
(chk "the verb ran it exactly once" 1 @ran)

(println "\n=== a declining fn still gets an answer, from the engine ===")
(reset! ran 0)
(chk "wrap-exit-fn supplies the fallback" "You can't go that way.\n"
     (with-out-str (turn! ::v-w :direction ::south)))
(chk "and the author's fn did run" 1 @ran)

(println "\n=== the freedoms a with fn now has ===")
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::east]
       {::with (wrap-exit-fn (fn [_] (no-time-passes!) ::handled))})
(chk "it can stop the clock" false (:time-passed? (turn! ::v-w :direction ::east)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::west]
       {::with (wrap-exit-fn (fn [_] (die! "The floor was never there.")))})
(chk "it can end the game" true
     (let [st (atom nil)] (with-out-str (reset! st (turn! ::v-w :direction ::west))) (:over? @st)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::in]
       {::with (wrap-exit-fn (fn [_] (tell! "One." :>> "Two." :>>) ::handled))})
(chk "it can print several lines and not move you" "One.\nTwo.\n"
     (with-out-str (turn! ::v-w :direction ::in)))
(chk "...and the actor really did not move" ::wroom (room-of ::d/you))

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
(println "\n=== `with` exits are handed over, not run ===")
(def ran (atom 0))
(def-verb ::v-w
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (resolve-exit k-here direction objects)]
             (cond
               (exit-to r)      (goto! (exit-to r))
               (exit-handler r) ((exit-handler r) ctx)
               :else            (tell! (exit-message r) :>>)))
           ::handled))

(make-object ::wroom
             {kw-label "W Room"
              kw-features #{::f-lit}
              kw-room-exits {::north {::to ::d/aqua-room
                                      ::with (wrap-exit-fn
                                              (fn [_] (swap! ran inc) ::handled))}
                             ::south {::with (wrap-exit-fn (fn [_] (swap! ran inc) nil))}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::wroom)
(remove! ::d/you) (place! ::d/you ::wroom) (set-actor! ::d/you)

(reset! ran 0)
(let [r (resolve-exit ::wroom ::north)]
  (chk "resolving does NOT run the fn" 0 @ran)
  (chk "no resolved destination -- it is undecided" nil (exit-to r))
  (chk "nothing to say either" nil (exit-message r))
  (chk "it hands over a fn" true (fn? (exit-handler r)))
  (chk "the declared destination is still catalogued" ::d/aqua-room (exit-destination r))
  (chk "and reports that it has one" true (exit-has-destination? r)))
(let [r (resolve-exit ::wroom ::south)]
  (chk "a with exit may declare no destination" false (exit-has-destination? r))
  (chk "so exit-destination is nil" nil (exit-destination r)))
(chk "resolving repeatedly still runs nothing" 0
     (do (dotimes [_ 5] (resolve-exit ::wroom ::north)) @ran))

(println "\n=== ...and run once, by the verb ===")
(with-out-str (turn! ::v-w :direction ::north))
(chk "the verb ran it exactly once" 1 @ran)

(println "\n=== a declining fn still gets an answer, from the engine ===")
(reset! ran 0)
(chk "wrap-exit-fn supplies the fallback" "You can't go that way.\n"
     (with-out-str (turn! ::v-w :direction ::south)))
(chk "and the author's fn did run" 1 @ran)

(println "\n=== the freedoms a with fn now has ===")
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::east]
       {::with (wrap-exit-fn (fn [_] (no-time-passes!) ::handled))})
(chk "it can stop the clock" false (:time-passed? (turn! ::v-w :direction ::east)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::west]
       {::with (wrap-exit-fn (fn [_] (die! "The floor was never there.")))})
(chk "it can end the game" true
     (let [st (atom nil)] (with-out-str (reset! st (turn! ::v-w :direction ::west))) (:over? @st)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::in]
       {::with (wrap-exit-fn (fn [_] (tell! "One." :>> "Two." :>>) ::handled))})
(chk "it can print several lines and not move you" "One.\nTwo.\n"
     (with-out-str (turn! ::v-w :direction ::in)))
(chk "...and the actor really did not move" ::wroom (room-of ::d/you))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

;; ---------------------------------------------------------------------------
;; exits
;; ---------------------------------------------------------------------------

(println "\n=== exits compile to data, not closures ===")
(def AQUA (prop ::d/aqua-room kw-room-exits))
(chk "a plain exit" {::to ::d/god-kingdom} (::north AQUA))
(chk "a never exit carries no destination at all" [::never]
     (vec (keys (::west AQUA))))
(chk "a door exit names its door" ::d/green-door (::via (::down AQUA)))
(chk "directions are enumerable now" #{::north ::east ::west ::down}
     (exit-directions ::d/aqua-room))

(println "\n=== resolve-exit computes and says nothing ===")
(remove! ::d/you)
(place! ::d/you ::d/aqua-room)
(set-actor! ::d/you)
(defn- res [dir] (resolve-exit ::d/aqua-room dir))
(chk "resolving prints nothing at all" "" (with-out-str (res ::west)))
(chk "and is safe to repeat -- the pre-action pattern depends on it"
     true (= (res ::down) (res ::down) (res ::down)))

(println "\n=== the accessors are the whole interface ===")
(let [r (res ::north)]
  (chk "open exit: destination" ::d/god-kingdom (exit-to r))
  (chk "open exit: nothing to say" nil (exit-message r))
  (chk "open exit: it exists" true (exit-exists? r))
  (chk "open exit: no door" nil (exit-door r))
  (chk "open exit: not permanent" false (exit-permanent? r)))
(let [r (res ::west)]
  (chk "never: no destination" nil (exit-to r))
  (chk "never: the author's words" "The Green Hallway is forbidden." (exit-message r))
  (chk "never: permanent" true (exit-permanent? r)))
(let [r (res ::east)]                                    ; if-gated, atom false
  (chk "failed condition: refused" nil (exit-to r))
  (chk "failed condition: falls back to the frame" "You can't go that way." (exit-message r))
  (chk "failed condition: not permanent" false (exit-permanent? r)))
(let [r (res ::in)]
  (chk "no such exit: refused" nil (exit-to r))
  (chk "no such exit: says so" "You can't go that way." (exit-message r))
  (chk "no such exit: and reports that there is none" false (exit-exists? r)))

(println "\n=== a door exit, and exit-door is about the declaration ===")
(shut! ::d/green-door)
(let [r (res ::down)]
  (chk "shut: refused" nil (exit-to r))
  (chk "shut: the door-shut frame" "Green Door is closed." (exit-message r))
  (chk "shut: names the door" ::d/green-door (exit-door r)))
(open! ::d/green-door)
(let [r (res ::down)]
  (chk "open: may go" ::d/cellar (exit-to r))
  (chk "open: STILL names the door -- it asks about the declaration"
       ::d/green-door (exit-door r)))

(println "\n=== the pre-action pattern: resolve, intervene, decline, resolve again ===")
(shut! ::d/green-door)
(def unlocked (atom []))
(def-verb ::v-walk
  pre    (fn [ctx]
           (let [door (exit-door (resolve-exit (:k-here ctx) (:direction ctx) (:objects ctx)))]
             (when (and door (not (open? door)))
               (swap! unlocked conj door)
               (open! door)
               nil)))                                    ; decline; the default proceeds
  handle (fn [{:keys [k-here direction] :as ctx}]
           (let [r (resolve-exit k-here direction (:objects ctx))]
             (if-let [to (exit-to r)] (goto! to) (tell! (exit-message r) :>>)))
           ::handled))
(with-out-str (turn! ::v-walk :direction ::down))
(chk "the pre-action opened the door" [::d/green-door] @unlocked)
(chk "and the default then walked through it" ::d/cellar (room-of ::d/you))

(println "\n=== the game's own annotations ===")
(make-object ::annotated {kw-room-exits {::north {::to ::d/aqua-room
                                                  ::notes {:my-game/gate :tidal}}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::annotated)
(let [r (resolve-exit ::annotated ::north)]
  (chk "notes come back under the game's own key" :tidal (:my-game/gate (exit-notes r)))
  (chk "and are kept out of the spec" nil (::notes (::spec r)))
  (chk "empty notes read as {}" {} (exit-notes (res ::north))))

(println "\n=== `with` exits are handed over, not run ===")
(def ran (atom 0))
(def-verb ::v-w
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (resolve-exit k-here direction objects)]
             (cond
               (exit-to r)      (goto! (exit-to r))
               (exit-handler r) ((exit-handler r) ctx)
               :else            (tell! (exit-message r) :>>)))
           ::handled))

(make-object ::wroom
             {kw-label "W Room"
              kw-features #{::f-lit}
              kw-room-exits {::north {::to ::d/aqua-room
                                      ::with (wrap-exit-fn
                                              (fn [_] (swap! ran inc) ::handled))}
                             ::south {::with (wrap-exit-fn (fn [_] (swap! ran inc) nil))}}})
(swap! OBJECTS update-in [ROOMS kw-contains-local] (fnil conj #{}) ::wroom)
(remove! ::d/you) (place! ::d/you ::wroom) (set-actor! ::d/you)

(reset! ran 0)
(let [r (resolve-exit ::wroom ::north)]
  (chk "resolving does NOT run the fn" 0 @ran)
  (chk "no resolved destination -- it is undecided" nil (exit-to r))
  (chk "nothing to say either" nil (exit-message r))
  (chk "it hands over a fn" true (fn? (exit-handler r)))
  (chk "the declared destination is still catalogued" ::d/aqua-room (exit-destination r))
  (chk "and reports that it has one" true (exit-has-destination? r)))
(let [r (resolve-exit ::wroom ::south)]
  (chk "a with exit may declare no destination" false (exit-has-destination? r))
  (chk "so exit-destination is nil" nil (exit-destination r)))
(chk "resolving repeatedly still runs nothing" 0
     (do (dotimes [_ 5] (resolve-exit ::wroom ::north)) @ran))

(println "\n=== ...and run once, by the verb ===")
(with-out-str (turn! ::v-w :direction ::north))
(chk "the verb ran it exactly once" 1 @ran)

(println "\n=== a declining fn still gets an answer, from the engine ===")
(reset! ran 0)
(chk "wrap-exit-fn supplies the fallback" "You can't go that way.\n"
     (with-out-str (turn! ::v-w :direction ::south)))
(chk "and the author's fn did run" 1 @ran)

(println "\n=== the freedoms a with fn now has ===")
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::east]
       {::with (wrap-exit-fn (fn [_] (no-time-passes!) ::handled))})
(chk "it can stop the clock" false (:time-passed? (turn! ::v-w :direction ::east)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::west]
       {::with (wrap-exit-fn (fn [_] (die! "The floor was never there.")))})
(chk "it can end the game" true
     (let [st (atom nil)] (with-out-str (reset! st (turn! ::v-w :direction ::west))) (:over? @st)))
(swap! OBJECTS assoc-in [::wroom kw-room-exits ::in]
       {::with (wrap-exit-fn (fn [_] (tell! "One." :>> "Two." :>>) ::handled))})
(chk "it can print several lines and not move you" "One.\nTwo.\n"
     (with-out-str (turn! ::v-w :direction ::in)))
(chk "...and the actor really did not move" ::wroom (room-of ::d/you))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

(println "\n=== direction-to, for \"go to the cellar\" ===")
(open! ::d/green-door)
(chk "finds a gated exit by its declared destination" ::down
     (direction-to ::d/aqua-room ::d/cellar))
(shut! ::d/green-door)
(chk "still finds it when the gate is shut" ::down
     (direction-to ::d/aqua-room ::d/cellar))
(chk "finds a with exit too" ::north (direction-to ::wroom ::d/aqua-room))
(chk "nil when nothing leads there" nil (direction-to ::d/aqua-room ::d/you))
(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

;; ---------------------------------------------------------------------------
;; the parser
;; ---------------------------------------------------------------------------

(require 'petra.engine.parser 'petra.engine.lexicon 'petra.engine.syntactic
         'petra.test-game.verbs)
(alias 'ps 'petra.engine.parser)
(alias 'lx 'petra.engine.lexicon)
(alias 'syn 'petra.engine.syntactic)
(alias 'v 'petra.test-game.verbs)

(remove! ::d/you) (place! ::d/you ::d/aqua-room) (set-actor! ::d/you)
(shut! ::d/rusty-pail)
(defn- P [s] (ps/parse s))
(defn- err [s] (::ps/error (P s)))

(println "\n=== a sentence becomes a command ===")
(chk "bare verb" {:verb ::v/look :dobj nil :iobj nil :direction nil} (P "look"))
(chk "synonym reaches the same verb" ::v/look (:verb (P "l")))
(chk "verb + object" [::v/take ::d/wet-rag] ((juxt :verb :dobj) (P "take the wet rag")))
(chk "determiner optional" ::d/wet-rag (:dobj (P "take rag")))
(chk "adjective narrows" ::d/tin-cup (:dobj (P "take tin cup")))
(chk "multi-word verb" [::v/examine ::d/wet-rag] ((juxt :verb :dobj) (P "look at rag")))
(chk "multi-word verb, other spelling" ::v/take (:verb (P "pick up rag")))

(println "\n=== direct and indirect objects, by theta-role ===")
(let [p (P "put the tin cup in the rusty pail")]
  (chk "verb" ::v/put-in (:verb p))
  (chk "direct object"   ::d/tin-cup    (:dobj p))
  (chk "indirect object" ::d/rusty-pail (:iobj p)))
(chk "and with every determiner dropped" [::d/tin-cup ::d/rusty-pail]
     ((juxt :dobj :iobj) (P "put tin cup in pail")))

(println "\n=== directions ===")
(chk "a bare direction is a sentence" [::v/walk ::north] ((juxt :verb :direction) (P "north")))
(chk "abbreviated" [::v/walk ::north] ((juxt :verb :direction) (P "n")))
(chk "or an argument of go" [::v/walk ::north] ((juxt :verb :direction) (P "go north")))
(chk "other directions too" ::down (:direction (P "go down")))

(println "\n=== failures are sentences the player can read ===")
(chk "unknown word" "[I don't know the word \"frobnitz\".]" (err "take frobnitz"))
(chk "not a sentence" "[I don't understand that sentence.]" (err "the the the"))
(chk "nothing at all" true (some? (err "")))
;; ::unicorn-horn is defined in the dungeon but placed nowhere
(chk "in the game but not here" "You can't see any horn here." (err "take horn"))
(chk "ambiguous" "Which do you mean, the clay cup or the tin cup?" (err "take cup"))
(chk "an adjective resolves the ambiguity" ::d/clay-cup (:dobj (P "take clay cup")))

(println "\n=== scope decides what is referable, not what is pronounceable ===")
(chk "the door is in scope via `share`" true
     (contains? (in-scope ::d/you) ::d/green-door))
(chk "so it can be named" ::d/green-door (:dobj (P "open green door")))
(place! (remove! ::d/you) ::d/god-kingdom)
(chk "out of scope: the word is still known" "You can't see any pail here." (err "take pail"))
(chk "...not treated as gibberish" false (boolean (re-find #"don't know" (err "take pail"))))
(place! (remove! ::d/you) ::d/aqua-room)

(println "\n=== the derivation is what carries the roles ===")
(let [lexicon (lx/lexicon ::d/you @OBJECTS)
      root (first (syn/derive-all (mapv first (syn/lexer "put the tin cup in the pail" lexicon))))]
  (chk "one derivation, rooted in V" syn/V (:cat root))
  (chk "the DO subtree" "the tin cup"
       (clojure.string/join " " (syn/words-of (syn/find-role root :DO))))
  (chk "the IO subtree" "the pail"
       (clojure.string/join " " (syn/words-of (syn/find-role root :IO))))
  (chk "and the verb is read off the head" ::v/put-in
       (::lx/verb (syn/head-leaf root))))

(println "\n=== optionality actually works now ===")
(chk "all three optional slots dropped" 1
     (count (syn/derive-all (mapv first (syn/lexer "take rag" (lx/lexicon ::d/you @OBJECTS))))))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

(println "\n=== head-to-head selection: two homophonous `put`s ===")
(remove! ::d/you) (place! ::d/you ::d/aqua-room) (set-actor! ::d/you)
(open! ::d/rusty-pail)
(chk "in -> put-in"  ::v/put-in (:verb (P "put the tin cup in the pail")))
(chk "on -> put-on"  ::v/put-on (:verb (P "put the tin cup on the pail")))
(chk "the roles fill the same way either way" [::d/tin-cup ::d/rusty-pail]
     ((juxt :dobj :iobj) (P "put tin cup on pail")))
(chk "a synonym of only one of them" ::v/put-on (:verb (P "lay tin cup on pail")))
(chk "...and it refuses the other preposition" true
     (some? (err "lay tin cup in pail")))
;; "put" is two lexical items, so every reading has to be tried -- which is what
;; the parser's `readings` does. Only one of them derives.
(defn- all-roots [sentence]
  (let [cands (syn/lexer sentence (lx/lexicon ::d/you @OBJECTS))
        readings (reduce (fn [acc alts] (for [a acc, c alts] (conj a c))) [[]] cands)]
    (mapcat syn/derive-all readings)))
(chk "two readings offered, exactly one derives" 1 (count (all-roots "put tin cup in pail")))
(chk "same the other way" 1 (count (all-roots "put tin cup on pail")))
;; :IO is borne by the N inside the PP, so the PP's own head is a separate question
(defn- pp-head [sentence]
  (let [root (first (all-roots sentence))]
    (:lex (syn/head-leaf (first (filter #(= syn/P (:cat %)) (syn/all-sos root)))))))
(chk "the selected phrase is headed by the required word" "in" (pp-head "put tin cup in pail"))
(chk "...and by the other one for the other verb" "on" (pp-head "put tin cup on pail"))
(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

(println "\n=== pragmatic assertions break ties; they never make errors ===")
(remove! ::d/you) (place! ::d/you ::d/aqua-room) (set-actor! ::d/you)
(doseq [k [::d/tin-cup ::d/clay-cup]] (place! k ::d/aqua-room))
(shut! ::d/rusty-pail)

(chk "both cups on the ground: TAKE is a genuine tie" true
     (some? (err "take cup")))
(chk "and the question offers exactly the two of them, with articles"
     "Which do you mean, the clay cup or the tin cup?" (err "take cup"))

(place! ::d/tin-cup ::d/you)                             ; now holding one of them
(chk "DROP wants a held thing -> the tin cup, no question" ::d/tin-cup
     (:dobj (P "drop the cup")))
(chk "TAKE wants a NOT-held thing -> the clay cup, no question" ::d/clay-cup
     (:dobj (P "take the cup")))
(chk "the same phrase, two verbs, two referents" [::d/tin-cup ::d/clay-cup]
     [(:dobj (P "drop cup")) (:dobj (P "take cup"))])

(place! ::d/tin-cup ::d/aqua-room)                       ; holding neither
(chk "nothing satisfies :held -> pick one rather than ask" true
     (some? (:dobj (P "drop the cup"))))
(chk "...and it is NOT an error: the verb owns that complaint" nil
     (::ps/error (P "drop the cup")))
(chk "deterministic, so the same input picks the same thing"
     (:dobj (P "drop the cup")) (:dobj (P "drop the cup")))

(println "\n=== a lone candidate is never rejected by pragmatics ===")
(remove! ::d/clay-cup)
(chk "one cup, not held, but DROP still resolves it" ::d/tin-cup
     (:dobj (P "drop the cup")))
(place! ::d/clay-cup ::d/aqua-room)

(println "\n=== the preposition itself asserts what a good complement is ===")
(chk "`in` prefers containers" #{:container}
     (:asserts (syn/find-role (first (all-roots "put tin cup in pail")) :IO)))
(chk "`on` prefers surfaces" #{:surface}
     (:asserts (syn/find-role (first (all-roots "put tin cup on pail")) :IO)))
(chk "assertions ride to the filler like roles do" #{:held}
     (:asserts (syn/find-role (first (all-roots "put tin cup in pail")) :DO)))

(println "\n=== unknown assertions are refused at declaration ===")
(chk "loud" "unknown pragmatic assertion"
     (first (why #(lx/make-words ["frob"] syn/V [:_ [syn/N :DO #{:wibble}]] {}))))
(chk "and the registry is inspectable" true
     (contains? (ps/assertion-names) :held))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

(println "\n=== PP attachment: adjoined to a DP vs argument of the VP ===")
(remove! ::d/you) (place! ::d/you ::d/cellar) (set-actor! ::d/you)
(place! ::d/table ::d/cellar)
(place! ::d/tin-cup ::d/table)
(place! ::d/clay-cup ::d/shelf)

(chk "TAKE has no P slot, so the PP must adjoin to the noun" ::d/tin-cup
     (:dobj (P "take the cup on the table")))
(chk "and it disambiguated two cups by where they are" ::d/clay-cup
     (:dobj (P "take the cup on the shelf")))
(chk "PUT-ON needs a P, so a lone PP is its argument" [::d/tin-cup ::d/shelf]
     ((juxt :dobj :iobj) (P "put the tin cup on the shelf")))

(println "\n=== both at once, in one sentence ===")
(let [p (P "put the cup on the table on the shelf")]
  (chk "the first PP narrowed the object" ::d/tin-cup (:dobj p))
  (chk "the second PP is the verb's goal" ::d/shelf (:iobj p)))
(chk "two derivations were on offer" 2 (count (all-roots "put the cup on the table on the shelf")))

(println "\n=== which attachment wins is decided by the world, not by a rule ===")
(place! ::d/table ::d/shelf)                             ; now there IS a table on the shelf
(place! ::d/tin-cup ::d/cellar)                          ; and nothing on the table
(let [p (P "put the cup on the table on the shelf")]
  (chk "same sentence, the OTHER attachment" ::d/table (:iobj p))
  (chk "...so the object is a bare cup" true (contains? #{::d/tin-cup ::d/clay-cup} (:dobj p))))
(place! ::d/table ::d/cellar) (place! ::d/tin-cup ::d/table)

(println "\n=== a modifier is not one of the noun's own words ===")
(chk "the walk stops at PRED, or {cups} n {tables} would be empty" #{::d/tin-cup}
     (let [np (syn/find-role (first (all-roots "take the cup on the table")) :DO)]
       (set (keep ::lx/objects (syn/leaves-above np #{syn/PRED})))
       (set [(:dobj (P "take the cup on the table"))])))
(chk "predicative `on` is a different category from argument `on`" true
     (not= syn/P syn/PRED))
(chk "a nonexistent location is a can't-see, not a wrong parse" true
     (boolean (re-find #"can't see" (err "take the cup on the pail"))))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))

(println "\n=== the parser says which thing it settled on ===")
(remove! ::d/you) (place! ::d/you ::d/aqua-room) (set-actor! ::d/you)
(doseq [k [::d/tin-cup ::d/clay-cup ::d/wet-rag]] (place! k ::d/aqua-room))
(place! ::d/table ::d/cellar)

(chk "nothing in doubt -> no note" nil (::ps/note (P "take the wet rag")))
(chk "nothing in doubt, even with an adjective" nil (::ps/note (P "take the tin cup")))

(place! ::d/tin-cup ::d/you)                             ; holding one of the two cups
(chk "a tie broken by pragmatics is reported" "(the clay cup)"
     (::ps/note (P "take the cup")))
(chk "and the object really is the reported one" ::d/clay-cup (:dobj (P "take the cup")))
(chk "the other verb reports the other one" "(the tin cup)"
     (::ps/note (P "drop the cup")))

(place! ::d/tin-cup ::d/aqua-room)                       ; holding neither
(chk "a doomed guess is NOT reported -- it is no help to anyone" nil
     (::ps/note (P "drop the cup")))
(chk "...but it still resolves, so the verb gets to complain" true
     (some? (:dobj (P "drop the cup"))))

(chk "an unbroken tie asks instead of noting" [nil true]
     (do (doseq [k [::d/tin-cup ::d/clay-cup]] (place! k ::d/you))
         [(::ps/note (P "drop the cup")) (some? (err "drop the cup"))]))
(doseq [k [::d/tin-cup ::d/clay-cup]] (place! k ::d/aqua-room))

(println "\n=== a structural tie is reported too, more coarsely ===")
(place! ::d/you ::d/cellar)
(place! ::d/table ::d/shelf) (place! ::d/tin-cup ::d/table) (place! ::d/clay-cup ::d/cellar)
(let [p (P "put the cup on the table on the shelf")]
  (chk "two whole readings resolved, so it names the one thing it settled on" true
       (some? (::ps/note p)))
  (chk "and the note is a single name, like any other assumption" true
       (boolean (re-find #"^\(the [a-z ]+\)$" (::ps/note p)))))
(place! ::d/table ::d/cellar) (place! ::d/tin-cup ::d/table)
(chk "only one reading resolves -> no structural note" nil
     (::ps/note (P "put the cup on the table on the shelf")))

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))
