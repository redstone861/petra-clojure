(ns petra.engine.containment-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.engine.core :as e :refer [object room]]
            [petra.support :as t]))

(defn- build! []
  (object ::you   label "you" features [no-article])
  (object ::box   label "wooden box" features [container])
  (object ::coin  label "gold coin" features [takeable])
  (object ::lamp  label "brass lamp" features [lit takeable])
  (room ::hall  label "Hall"  features [lit] contains [::box ::coin])
  (room ::vault label "Vault" contains [::lamp]))

(use-fixtures :each (t/with-world build!))

(deftest contains-set-is-authoritative
  (is (= #{::box ::coin} (e/contents ::hall)))
  (testing "location is derived from it, not stored"
    (is (= ::hall (e/location-of ::coin)))
    (is (nil? (e/location-of ::you)) "nobody holds the actor yet")))

(deftest in?-is-direct-and-ultimately-in?-is-not
  (e/move! ::coin ::box)
  (is (e/in? ::coin ::box))
  (is (not (e/in? ::coin ::hall)) "in? does not see through the box")
  (is (e/ultimately-in? ::coin ::hall)))

(deftest room-of-climbs-to-the-room
  (e/move! ::coin ::box)
  (is (= ::hall (e/room-of ::coin)))
  (is (= ::hall (e/room-of ::hall)) "a room is its own room")
  (is (nil? (e/room-of (e/remove! ::coin)))))

(deftest move!-keeps-one-parent
  (e/move! ::coin ::box)
  (is (= #{::box} (e/contents ::hall)) "left the old parent")
  (is (= #{::coin} (e/contents ::box)) "joined the new one")
  (is (= 1 (count (get (e/parent-index) ::coin)))))

(deftest move!-marks-touched-and-place!-does-not
  (is (not (e/feature-set? ::coin ::e/f-touched)))
  (e/place! ::coin ::box)
  (is (not (e/feature-set? ::coin ::e/f-touched)) "place! records nothing")
  (e/move! ::coin ::hall)
  (is (e/feature-set? ::coin ::e/f-touched) "move! records the disturbance"))

(deftest remove!-empties-the-location
  (e/remove! ::coin)
  (is (nil? (e/location-of ::coin)))
  (is (= #{::box} (e/contents ::hall))))

(deftest containment-problems-finds-two-parents
  (is (= {} (e/containment-problems)) "the declared world is well formed")
  (swap! e/OBJECTS update-in [::vault ::e/contains-local] conj ::coin)
  (is (= #{::hall ::vault} (get (e/containment-problems) ::coin))))

(deftest light-does-not-pass-through-a-shut-container
  (is (e/lit? ::vault) "the vault holds the lamp")
  (e/move! ::lamp ::box)
  (e/move! ::box ::vault)
  (is (not (e/lit? ::vault)) "now the lamp is inside a shut container")
  (e/open! ::box)
  (is (e/lit? ::vault) "and open, the light gets out")
  (e/remove! ::lamp)
  (is (not (e/lit? ::vault)) "no lamp, no light")
  (testing "a room may also be lit in its own right"
    (is (e/lit? ::hall))))

(deftest vocabulary-comes-from-the-label
  (is (= #{"lamp"} (e/nouns-of ::lamp)))
  (is (= #{"brass"} (e/adjectives-of ::lamp))))

(deftest in-scope-is-the-room-its-contents-and-the-inventory
  (e/set-actor! ::you)
  (e/place! ::you ::hall)
  (e/move! ::lamp ::you)
  (let [scope (e/in-scope)]
    (is (contains? scope ::hall)  "the room itself")
    (is (contains? scope ::coin)  "what is in it")
    (is (contains? scope ::you)   "the actor")
    (is (contains? scope ::lamp)  "and what the actor carries")
    (is (not (contains? scope ::vault)) "but not another room")))

(deftest shut-containers-hide-their-contents-from-scope
  (e/set-actor! ::you)
  (e/place! ::you ::hall)
  (e/move! ::lamp ::box)
  (is (not (contains? (e/in-scope) ::lamp)))
  (e/open! ::box)
  (is (contains? (e/in-scope) ::lamp)))
