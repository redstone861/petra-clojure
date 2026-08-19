(ns petra.engine.text-test
  "Text frames: the engine holds no player-facing prose of its own."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.engine.core :as e :refer [object]]
            [petra.engine.text :as text]
            [petra.support :as t]))

(defn- build! []
  (object ::pail  label "rusty pail")
  (object ::apple label "apple")
  (object ::door  label "Green Door" features [no-article]))

(use-fixtures :each (t/with-world build!))

(deftest slots-fill-from-a-map
  (e/merge-frames! {::probe "{{the o}} / {{a o}} / {{an o}} / {{The o}} / {{label o}} / {{n}}"})
  (is (= "the rusty pail / a rusty pail / a rusty pail / The rusty pail / rusty pail / 7"
         (e/say ::probe {:o ::pail :n 7})))
  (is (= "the apple / an apple / an apple / The apple / apple / 7"
         (e/say ::probe {:o ::apple :n 7})) "the article is inferred")
  (is (= "Green Door / Green Door / Green Door / Green Door / Green Door / 7"
         (e/say ::probe {:o ::door :n 7})) "no-article drops it everywhere"))

(deftest a-frame-uses-as-many-slots-as-it-likes
  (e/merge-frames! {::zero "Nothing happens."
                    ::one  "You see {{a x}}."
                    ::three "{{The a}} hits {{the b}} with {{the c}}."})
  (is (= "Nothing happens." (e/say ::zero {:x ::pail :junk 1})) "extra args ignored")
  (is (= "You see a rusty pail." (e/say ::one {:x ::pail})))
  (is (= "The rusty pail hits the apple with Green Door."
         (e/say ::three {:a ::pail :b ::apple :c ::door})))
  (is (= "You see a rusty pail." (e/say ::one {:x ::pail :unused ::apple}))))

(deftest authoring-mistakes-are-visible-not-silent
  (e/merge-frames! {::one "You see {{a x}}."})
  (is (= "You see [?x]." (e/say ::one {})) "an unsupplied slot")
  (is (re-find #"missing text frame" (e/say ::no-such-frame)))
  (e/merge-frames! {::dollar "{{label o}} costs $5"})
  (e/make-object ::odd {::e/label "50%$ thing"})
  (is (= "50%$ thing costs $5" (e/say ::dollar {:o ::odd}))
      "regex-special characters in a label survive substitution"))

(deftest a-frame-may-be-a-fn-when-a-line-must-branch
  (e/merge-frames! {::maybe (fn [args] (if (:spooky args) "Something breathes." "Nothing."))})
  (is (= "Nothing." (e/say ::maybe)))
  (is (= "Something breathes." (e/say ::maybe {:spooky true}))))

(deftest a-game-can-reword-the-engine
  (e/merge-frames! {::text/cant-go "There's no way through."})
  (is (= "There's no way through." (e/say ::text/cant-go)))
  (e/set-frames! text/FRAMES)
  (is (= "You can't go that way." (e/say ::text/cant-go)) "and put it back"))

(deftest list-punctuation-is-authorable
  (is (= ", and " (e/say ::text/list-last)) "the Oxford comma is a frame, not a rule")
  (e/merge-frames! {::text/list-last " and "})
  (is (= " and " (e/say ::text/list-last))))
