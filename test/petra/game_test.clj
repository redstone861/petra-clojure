(ns petra.game-test
  "Booting a game, and playing the shipped test game end to end. These are the
  tests that would notice if the pieces stopped fitting together."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.core :as runner]
            [petra.engine.core :as e]
            [petra.support :as t]
            [petra.test-game.dungeon :as d]
            [petra.test-game.game :as game]
            [petra.test-game.verbs :as v]))

;; The shipped game's world is built by requiring its namespaces, which only ever
;; happens once -- so wipe back to a bare engine and re-run those files into it,
;; then hand every test a copy. Otherwise the suite's other namespaces have already
;; put their own "take" in the lexicon and the game's verbs are ambiguous.
(t/fresh!)
(require 'petra.test-game.dungeon :reload)
(require 'petra.test-game.verbs :reload)
(def WORLD (t/snapshot))

(use-fixtures :each (t/with-snapshot WORLD))

(defn- fresh-boot! []
  (e/remove! ::d/you)
  (e/clear-feature ::d/you ::e/f-touched)
  (t/out #(e/boot! game/CONFIG)))

;; --- the config ------------------------------------------------------------

(deftest a-game-is-data
  (is (= {::e/title "A Test Dungeon"
          ::e/author "nobody in particular"
          ::e/actor ::d/you
          ::e/start ::d/god-kingdom}
         game/CONFIG))
  (is (nil? (ns-resolve 'petra.test-game.game '-main))
      "and says nothing about being run"))

(deftest boot-places-the-actor-and-describes
  (let [said (fresh-boot!)]
    (is (= ::d/god-kingdom (e/room-of ::d/you)))
    (is (= ::d/you @e/ACTOR))
    (is (re-find #"A Test Dungeon" said))
    (is (re-find #"God's Kingdom" said))
    (is (not (e/feature-set? ::d/you ::e/f-touched))
        "seating the actor disturbed nothing")))

(deftest boot-fires-no-movement-events
  (let [seen (atom [])]
    (swap! e/OBJECTS assoc-in [::d/god-kingdom ::e/on ::e/enter] (fn [_] (swap! seen conj :enter)))
    (swap! e/OBJECTS assoc-in [::d/god-kingdom ::e/on ::e/leave] (fn [_] (swap! seen conj :leave)))
    (fresh-boot!)
    (is (= [] @seen) "beginning somewhere is not arriving there")))

(deftest boot-refuses-a-malformed-world
  (fresh-boot!)
  (is (t/throws-info? #"already placed" #(e/boot! game/CONFIG))
      "boot! is once-only, on a world nobody has placed the actor in")
  (e/remove! ::d/you)
  (is (t/throws-info? #"is not a room"
                      #(e/boot! (assoc game/CONFIG ::e/start ::d/rusty-pail))))
  (is (t/throws-info? #"no `actor`" #(e/boot! (dissoc game/CONFIG ::e/actor))))
  (is (t/throws-info? #"no `start`" #(e/boot! (dissoc game/CONFIG ::e/start))))
  (testing "and an object in two places at once"
    ;; the shelf lives in the cellar, so this really does give it a second parent
    (swap! e/OBJECTS update-in [::d/aqua-room ::e/contains-local] conj ::d/shelf)
    (is (= #{::d/aqua-room ::d/cellar} (get (e/containment-problems) ::d/shelf)))
    (is (t/throws-info? #"more than one parent" #(e/boot! game/CONFIG)))))

(deftest the-runner-finds-a-game-by-name
  (is (= game/CONFIG (runner/load-config 'petra.test-game.game)))
  (is (t/throws-info? #"no CONFIG" #(runner/load-config 'clojure.string))))

;; --- playing ---------------------------------------------------------------

(deftest walking-about
  (fresh-boot!)
  (let [said (t/play "s" "look" "n")]
    (is (re-find #"Aqua Room" said))
    (is (re-find #"God's Kingdom" said) "and back again"))
  (is (= ::d/god-kingdom (e/room-of ::d/you))))

(deftest a-shut-door-must-be-opened-and-can-be
  (fresh-boot!)
  (t/play "s")
  (is (re-find #"is closed" (t/play "d")) "the door gates the way down")
  (is (re-find #"You open" (t/play "open green door"))
      "and share makes it referable from here")
  (t/play "d")
  (is (= ::d/cellar (e/room-of ::d/you))))

(deftest taking-and-dropping
  (fresh-boot!)
  (t/play "s")
  (is (re-find #"Taken" (t/play "take tin cup")))
  (is (e/ultimately-in? ::d/tin-cup ::d/you))
  (is (re-find #"a tin cup" (t/play "i")))
  (is (re-find #"Dropped" (t/play "drop tin cup")))
  (is (not (e/ultimately-in? ::d/tin-cup ::d/you))))

(deftest things-that-cannot-be-taken-say-so
  (fresh-boot!)
  (t/play "s")
  (is (re-find #"rusted into place" (t/play "take nails"))
      "the object's own responder gets there before the verb default")
  (is (re-find #"square-cut nails" (t/play "x nails"))
      "and it tells verbs apart, which is what keywords bought"))

(deftest containers-and-surfaces-disagree-about-prepositions
  (fresh-boot!)
  (t/play "s" "take tin cup" "open pail")
  (is (re-find #"in the rusty pail" (t/play "put tin cup in pail")))
  (is (e/in? ::d/tin-cup ::d/rusty-pail))
  (testing "and a surface refuses `in`"
    (t/play "take tin cup" "open green door" "d")            ; carry it down with us
    (is (re-find #"on the stone shelf" (t/play "put tin cup on shelf")))
    (is (re-find #"inside the stone shelf" (t/play "put tin cup in shelf")))))

(deftest the-implicit-take
  (fresh-boot!)
  (t/play "s" "open pail")
  (let [said (t/play "put tin cup in pail")]
    (is (re-find #"first taking the tin cup" said))
    (is (re-find #"in the rusty pail" said))))

(deftest disambiguation-in-play
  (fresh-boot!)
  (t/play "s")
  (is (re-find #"Which do you mean, the clay cup or the tin cup" (t/play "take cup")))
  (t/play "take tin cup")
  (let [said (t/play "take cup")]
    (is (re-find #"\(the clay cup\)" said) "the note names what it assumed")
    (is (re-find #"Taken" said))))

(deftest parser-failures-cost-no-turn
  (fresh-boot!)
  (t/play "s")
  (let [drips #(count (re-seq #"Water drips" %))]
    (is (zero? (drips (t/play "xyzzy"))) "an unknown word does not advance the clock")
    (is (pos? (drips (t/play "look"))) "but a real command does")))

(deftest a-meta-verb-costs-no-turn-either
  (fresh-boot!)
  (t/play "s")
  (is (zero? (count (re-seq #"Water drips" (t/play "verbose")))))
  (e/set-verbosity! e/v-brief))

(deftest quitting-ends-the-game-without-dying
  (fresh-boot!)
  (let [said (t/play "quit")]
    (is (re-find #"Goodbye" said))
    (is (not (re-find #"died" said)))))

(deftest play-turn!-reports-a-parse-failure-and-runs-nothing
  (fresh-boot!)
  (let [st (atom :untouched)
        said (t/out #(reset! st (runner/play-turn! "xyzzy")))]
    (is (re-find #"don't know" said) "it explains itself")
    (is (nil? @st) "and reports no turn, so the clock never moved")))
