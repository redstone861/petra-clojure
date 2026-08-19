(ns petra.engine.dispatch-test
  "The chain, the turn, and the two ways a game can end."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.engine.core :as e :refer [object room def-verb]]
            [petra.support :as t]))

(def log (atom []))
(defn- note! [x] (swap! log conj x) nil)
(defn- claim! [x] (swap! log conj x) ::e/handled)

(defn- build! []
  (object ::you  label "you" features [no-article] handle (fn [_] (note! :actor)))
  (object ::mute label "mute thing")
  (object ::dobj label "direct thing"   handle (fn [_] (note! :dobj)))
  (object ::iobj label "indirect thing" handle (fn [_] (note! :iobj)))
  (room ::room label "Room" features [lit] contains [::dobj ::iobj ::mute]
        handle (fn [_] (note! :room))
        on {each-turn (fn [_] (note! :each-turn))})
  (def-verb ::silent  handle (fn [_] (note! :default)))
  (def-verb ::claims  handle (fn [_] (claim! :default)))
  (def-verb ::meta    turn? false handle (fn [_] (claim! :default)))
  (def-verb ::with-pre pre (fn [_] (note! :pre)) handle (fn [_] (claim! :default))))

(use-fixtures :each (t/with-world build!))

(defn- setup! []
  (reset! log [])
  (e/set-actor! ::you)
  (e/place! ::you ::room))

(deftest perform-runs-the-chain-in-order
  (setup!)
  (e/perform! ::with-pre :dobj ::dobj :iobj ::iobj)
  (is (= [:actor :room :pre :iobj :dobj :default] @log)))

(deftest perform-reports-only-whether-the-input-was-consumed
  (setup!)
  (is (false? (e/perform! ::silent :dobj ::mute)) "nobody claimed it")
  (is (true?  (e/perform! ::claims :dobj ::mute)) "the default claimed it"))

(deftest a-responder-that-claims-stops-the-chain
  (setup!)
  (swap! e/OBJECTS assoc-in [::dobj ::e/handler] (fn [_] (claim! :dobj)))
  (e/perform! ::silent :dobj ::dobj)
  (is (= [:actor :room :dobj] @log) "the default never ran"))

(deftest an-object-without-a-responder-is-skipped
  (setup!)
  (e/perform! ::silent :dobj ::mute)
  (is (= [:actor :room :default] @log)))

(deftest a-responder-can-tell-direct-from-indirect
  (setup!)
  (let [seen (atom nil)]
    (swap! e/OBJECTS assoc-in [::dobj ::e/handler]
           (fn [ctx] (reset! seen [(= (:self ctx) (:k-dobj ctx))
                                   (= (:self ctx) (:k-iobj ctx))]) nil))
    (e/perform! ::silent :dobj ::dobj :iobj ::iobj)
    (is (= [true false] @seen))))

(deftest turn-reports-the-turn-and-ends-it
  (setup!)
  (is (= {:time-passed? true :handled? true :over? false} (e/turn! ::claims)))
  (is (= [:actor :room :default :each-turn] @log) "the room heard the turn end"))

(deftest a-meta-verb-does-not-advance-the-clock
  (setup!)
  (is (false? (:time-passed? (e/turn! ::meta))))
  (is (not (some #{:each-turn} @log)) "so no each-turn"))

(deftest no-time-passes!-works-from-any-depth
  (setup!)
  (letfn [(deep [] (e/no-time-passes!) nil)
          (deeper [] (deep))]
    (e/make-verb ::sneaky {::e/handler (fn [_] (deeper) ::e/handled)})
    (is (false? (:time-passed? (e/turn! ::sneaky))) "no ctx threaded anywhere")))

(deftest turn-state-does-not-leak-between-turns
  (setup!)
  (e/make-verb ::once {::e/handler (fn [_] (e/no-time-passes!) ::e/handled)})
  (e/turn! ::once)
  (is (true? (:time-passed? (e/turn! ::claims))) "the next turn starts clean")
  (is (nil? (e/turn-state)) "and there is no turn outside a turn"))

(deftest the-closed-list-stays-closed
  (is (= [:time-passed?] (keys e/default-turn-state)))
  (is (not (contains? (e/context {} nil) :turn)) "the turn is not in the context")
  (is (:private (meta #'e/record-turn!)) "so the list cannot quietly grow"))

(deftest die!-aborts-the-rest-of-the-turn
  (setup!)
  (swap! e/OBJECTS assoc-in [::you ::e/handler] (fn [_] (e/die! "A boulder.")))
  (let [st (atom nil)
        said (t/out #(reset! st (e/turn! ::claims :dobj ::dobj)))]
    (is (:over? @st))
    (is (= [] @log) "nothing further in the chain ran")
    (is (re-find #"A boulder" said))
    (is (re-find #"You have died" said) "and the death frame")))

(deftest end-game!-ends-without-dying
  (setup!)
  (e/make-verb ::quit {::e/handler (fn [_] (e/end-game! "Goodbye."))})
  (let [st (atom nil)
        said (t/out #(reset! st (e/turn! ::quit)))]
    (is (:over? @st))
    (is (re-find #"Goodbye" said))
    (is (not (re-find #"died" said)) "quitting is not dying")))

(deftest a-real-fault-is-not-mistaken-for-an-ending
  (setup!)
  (swap! e/OBJECTS assoc-in [::you ::e/handler] (fn [_] (throw (ex-info "boom" {}))))
  (is (t/throws-info? #"boom" #(e/turn! ::claims))))

(deftest unknown-verbs-and-missing-handles-are-loud
  (is (t/throws-info? #"unknown verb" #(e/perform! ::no-such-verb)))
  (is (t/throws-info? #"needs a `handle`"
                      #(eval '(petra.engine.core/def-verb ::nope turn? false))))
  (is (t/throws-info? #"Unknown verb property"
                      #(eval '(petra.engine.core/def-verb ::nope2 wibble 1 handle (fn [_] nil))))))
