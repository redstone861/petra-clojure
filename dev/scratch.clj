;; Scratch harness. Not a test suite -- no lein test hookup, nothing here is
;; load-bearing, and it is meant to be rewritten or thrown away freely as the
;; engine's shape changes. It exists only so that exercising the engine doesn't
;; start from nothing every session.
;;
;;   lein run -m clojure.main dev/scratch.clj

(require 'petra.dungeon 'petra.engine 'petra.text)
(in-ns 'petra.engine)
(alias 'd 'petra.dungeon)

(def fails (atom 0))
(defn chk [label expected actual]
  (when-not (= expected actual) (swap! fails inc))
  (println (if (= expected actual) "  ok " "FAIL ") label "=>" (pr-str actual)
           (if (= expected actual) "" (str "\n        EXPECTED " (pr-str expected)))))

(set-actor! ::d/you)
(move! ::d/you ::d/god-kingdom)

(println "\n=== perform! means one thing again: did anything consume the input ===")
(make-object ::mute {kw-label "mute thing"})
(make-object ::speaker {kw-label "speaker" kw-handler (fn [_] ::handled)})
(chk "nobody handled it"        false (perform! (fn [_] nil) :dir ::mute))
(chk "an object handled it"     true  (perform! (fn [_] nil) :dir ::speaker))
(chk "the verb default handled it" true (perform! (fn [_] ::handled) :dir ::mute))

(println "\n=== turn! reports the turn, and raises ev-each-turn ===")
(def ticks (atom 0))
(swap! OBJECTS assoc-in [::d/god-kingdom kw-on ev-each-turn] (fn [_] (swap! ticks inc)))
(reset! ticks 0)
(chk "a normal turn" {:time-passed? true :handled? true :over? false}
     (turn! (fn [_] ::handled)))
(chk "the room heard the turn end" 1 @ticks)
(chk "an unhandled turn still passes time" {:time-passed? true :handled? false :over? false}
     (turn! (fn [_] nil)))
(chk "so the room heard that one too" 2 @ticks)

(println "\n=== no-time-passes! -- and from arbitrary depth, the whole point ===")
(reset! ticks 0)
(chk "a handler suppresses the clock" {:time-passed? false :handled? true :over? false}
     (turn! (fn [_] (no-time-passes!) ::handled)))
(chk "so no ev-each-turn" 0 @ticks)

;; a plain helper: no ctx, no idea it's inside a turn, three frames deep
(defn- meta-verb? [] (no-time-passes!) true)
(defn- deep-helper [] (meta-verb?))
(defn- deeper-still [] (deep-helper))
(reset! ticks 0)
(chk "recorded from three frames down, with no ctx threaded"
     {:time-passed? false :handled? true :over? false}
     (turn! (fn [_] (deeper-still) ::handled)))
(chk "clock still suppressed" 0 @ticks)

(println "\n=== turn state is per-turn and doesn't leak ===")
(reset! ticks 0)
(turn! (fn [_] (no-time-passes!) ::handled))
(chk "the next turn starts clean" {:time-passed? true :handled? true :over? false}
     (turn! (fn [_] ::handled)))
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
      st (with-out-str (reset! out (turn! (fn [_] (swap! reached conj :verb-default) ::handled)
                                         :dir ::doomed)))]
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
  (with-out-str (reset! st (turn! (fn [_] ::handled))))
  (chk "die! works from a helper too" true (:over? @st)))
(swap! OBJECTS assoc-in [::d/you kw-handler] (fn [_] (throw (ex-info "boom" {:real-bug true}))))
(chk "a genuine ExceptionInfo is not swallowed as death" "boom"
     (try (turn! (fn [_] ::handled)) :no-throw
          (catch clojure.lang.ExceptionInfo e (ex-message e))))
(swap! OBJECTS update ::d/you dissoc kw-handler)

(println "\n=== the death line is authorable like any other ===")
(merge-frames! {::text/died "*** Your adventure ends here. ***"})
(swap! OBJECTS assoc-in [::d/you kw-handler] (fn [_] (die! "The floor gives way.")))
(println (clojure.string/replace (with-out-str (turn! (fn [_] ::handled)))
                                 #"(?m)^" "  | "))
(set-frames! petra.text/FRAMES)
(swap! OBJECTS update ::d/you dissoc kw-handler)

(println (if (zero? @fails) "\nALL PASS" (str "\n" @fails " FAILURE(S)")))
