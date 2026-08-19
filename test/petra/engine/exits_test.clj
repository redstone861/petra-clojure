(ns petra.engine.exits-test
  "Exits are data; resolving them is a pure query."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.engine.core :as e :refer [object room]]
            [petra.support :as t]))

(def GATE (atom false))
(def ran  (atom 0))

(defn- fn-exit [_] (swap! ran inc) ::e/handled)

(defn- build! []
  (object ::you  label "you" features [no-article])
  (object ::door label "oak door")
  (room ::hub label "Hub" features [lit]
      share [::door]
      to [[north ::plain]
          [east  ::gated if GATE or "The way is barred."]
          [south ::cellar via ::door]
          [west  never "There is no wall there, only sky."]
          [up    ::attic with fn-exit]
          [down  with fn-exit]])
  (room ::plain  label "Plain"  features [lit] to [[south ::hub]])
  (room ::gated  label "Gated"  features [lit])
  (room ::cellar label "Cellar" features [lit] share [::door])
  (room ::attic  label "Attic"  features [lit]))

(use-fixtures :each (t/with-world build!))

(defn- setup! [] (reset! ran 0) (reset! GATE false)
  (e/shut! ::door) (e/set-actor! ::you) (e/place! ::you ::hub))

(deftest exits-compile-to-data
  (let [x (e/prop ::hub ::e/exits)]
    (is (= {::e/to ::plain} (::e/north x)) "a plain exit is just a destination")
    (is (= [::e/never] (keys (::e/west x))) "a never exit has no destination at all")
    (is (= ::door (::e/via (::e/south x))))
    (is (= #{::e/north ::e/east ::e/south ::e/west ::e/up ::e/down}
           (e/exit-directions ::hub)) "and so they can be enumerated")))

(deftest resolving-computes-and-says-nothing
  (setup!)
  (is (= "" (t/out #(e/resolve-exit ::hub ::e/west))))
  (is (apply = (repeatedly 4 #(e/resolve-exit ::hub ::e/south)))
      "repeatable, which is what the pre-action pattern relies on"))

(deftest the-accessors-are-the-interface
  (setup!)
  (testing "an open way"
    (let [r (e/resolve-exit ::hub ::e/north)]
      (is (= ::plain (e/exit-to r)))
      (is (nil? (e/exit-message r)))
      (is (e/exit-exists? r))
      (is (nil? (e/exit-door r)))
      (is (not (e/exit-permanent? r)))))
  (testing "never"
    (let [r (e/resolve-exit ::hub ::e/west)]
      (is (nil? (e/exit-to r)))
      (is (= "There is no wall there, only sky." (e/exit-message r)))
      (is (e/exit-permanent? r))
      (is (not (e/exit-has-destination? r)))))
  (testing "a failed condition falls back to the author's message"
    (is (= "The way is barred." (e/exit-message (e/resolve-exit ::hub ::e/east))))
    (reset! GATE true)
    (is (= ::gated (e/exit-to (e/resolve-exit ::hub ::e/east)))))
  (testing "no exit that way at all"
    (let [r (e/resolve-exit ::hub ::e/in)]
      (is (not (e/exit-exists? r)))
      (is (= "You can't go that way." (e/exit-message r))))))

(deftest exit-door-is-about-the-declaration-not-the-outcome
  (setup!)
  (is (= ::door (e/exit-door (e/resolve-exit ::hub ::e/south))) "shut: named")
  (e/open! ::door)
  (is (= ::door (e/exit-door (e/resolve-exit ::hub ::e/south))) "open: still named")
  (is (= ::cellar (e/exit-to (e/resolve-exit ::hub ::e/south)))))

(deftest a-with-exit-is-handed-over-not-run
  (setup!)
  (let [r (e/resolve-exit ::hub ::e/up)]
    (is (zero? @ran) "resolving ran nothing")
    (is (nil? (e/exit-to r)) "undecided")
    (is (nil? (e/exit-message r)))
    (is (fn? (e/exit-handler r)))
    (is (= ::attic (e/exit-destination r)) "the declared destination is catalogued")
    (is (e/exit-has-destination? r)))
  (dotimes [_ 5] (e/resolve-exit ::hub ::e/up))
  (is (zero? @ran) "however many times you ask")
  (testing "and it may decline to declare a destination"
    (is (not (e/exit-has-destination? (e/resolve-exit ::hub ::e/down))))))

(deftest a-declining-with-fn-still-gets-an-answer
  (setup!)
  (e/make-verb ::walk {::e/handler
                       (fn [{:keys [k-here direction objects] :as ctx}]
                         (let [r (e/resolve-exit k-here direction objects)]
                           (cond (e/exit-to r)      (e/goto! (e/exit-to r))
                                 (e/exit-handler r) ((e/exit-handler r) ctx)
                                 :else              (e/tell! (e/exit-message r) :>>)))
                         ::e/handled)})
  (swap! e/OBJECTS assoc-in [::hub ::e/exits ::e/in]
         {::e/with (e/wrap-exit-fn (fn [_] nil))})
  (is (= "You can't go that way.\n" (t/out #(e/turn! ::walk :direction ::e/in)))
      "wrap-exit-fn supplies the fallback, so the turn is never silent"))

(deftest a-with-fn-has-a-responders-freedoms
  (setup!)
  (e/make-verb ::walk {::e/handler
                       (fn [{:keys [k-here direction objects] :as ctx}]
                         (let [r (e/resolve-exit k-here direction objects)]
                           (cond (e/exit-to r)      (e/goto! (e/exit-to r))
                                 (e/exit-handler r) ((e/exit-handler r) ctx)
                                 :else              (e/tell! (e/exit-message r) :>>)))
                         ::e/handled)})
  (testing "it can stop the clock"
    (swap! e/OBJECTS assoc-in [::hub ::e/exits ::e/in]
           {::e/with (e/wrap-exit-fn (fn [_] (e/no-time-passes!) ::e/handled))})
    (is (false? (:time-passed? (e/turn! ::walk :direction ::e/in)))))
  (testing "print several lines and not move you"
    (swap! e/OBJECTS assoc-in [::hub ::e/exits ::e/in]
           {::e/with (e/wrap-exit-fn (fn [_] (e/tell! "One." :>> "Two." :>>) ::e/handled))})
    (is (= "One.\nTwo.\n" (t/out #(e/turn! ::walk :direction ::e/in))))
    (is (= ::hub (e/room-of ::you))))
  (testing "or end the game"
    (swap! e/OBJECTS assoc-in [::hub ::e/exits ::e/in]
           {::e/with (e/wrap-exit-fn (fn [_] (e/die! "Down you go.")))})
    (let [st (atom nil)]
      (t/out #(reset! st (e/turn! ::walk :direction ::e/in)))
      (is (:over? @st)))))

(deftest direction-to-matches-the-declared-destination
  (setup!)
  (is (= ::e/south (e/direction-to ::hub ::cellar)) "even with the door shut")
  (is (= ::e/up (e/direction-to ::hub ::attic)) "and for a with exit")
  (is (nil? (e/direction-to ::hub ::you))))

(deftest goto!-orders-leave-move-enter-look
  (setup!)
  (let [seen (atom [])]
    (swap! e/OBJECTS assoc-in [::hub ::e/on ::e/leave]
           (fn [ctx] (swap! seen conj [:leave (e/in? ::you (:self ctx))])))
    (swap! e/OBJECTS assoc-in [::plain ::e/on ::e/enter]
           (fn [ctx] (swap! seen conj [:enter (:k-here ctx)])
             (e/move! ::door ::plain)))                      ; change what there is to see
    (let [said (t/out #(e/goto! ::plain))]
      (is (= [[:leave true] [:enter ::plain]] @seen)
          "leave with the actor still there; enter already arrived")
      (is (re-find #"oak door" said) "and look! saw what enter added"))))

(deftest goto!-is-loud-about-author-errors
  (setup!)
  (is (t/throws-info? #"not a room" #(e/goto! ::door)))
  (is (t/throws-info? #"no actor" (fn [] (e/set-actor! nil) (e/goto! ::plain))))
  (is (= ::hub (e/room-of ::you)) "and never moved the actor while complaining"))

(deftest malformed-exits-are-compile-errors
  (doseq [[why form]
          [["two gates"       '(petra.engine.core/def-room ::x1 to [[north ::y if (atom true) via ::d]])]
           ["never + dest"    '(petra.engine.core/def-room ::x2 to [[west ::y never "no"]])]
           ["missing dest"    '(petra.engine.core/def-room ::x3 to [[north if (atom true)]])]
           ["or without gate" '(petra.engine.core/def-room ::x4 to [[north ::y or "huh"]])]
           ["odd options"     '(petra.engine.core/def-room ::x5 to [[north ::y if]])]
           ["unknown option"  '(petra.engine.core/def-room ::x6 to [[north ::y wibble 1]])]
           ["unknown dir"     '(petra.engine.core/def-room ::x7 to [[sideways ::y]])]]]
    (is (t/throws-info? #"." #(eval form)) why)))
