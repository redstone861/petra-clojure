(ns petra.engine.describers-test
  "Describers return strings; look! is the only thing that prints."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [petra.engine.core :as e :refer [object room]]
            [petra.support :as t]))

(defn- build! []
  (object ::you   label "you" features [no-article])
  (object ::lamp  label "brass lamp" features [lit takeable]
        fdesc "A lamp hangs from a bracket, still burning."
        desc  "A brass lamp lies here, burning.")
  (object ::key   label "iron key"  features [takeable])       ; no desc: stock line
  (object ::rag   label "wet rag"   features [takeable])       ; likewise
  (object ::alcove label "shallow alcove" features [no-desc])  ; room prose names it
  (object ::box   label "wooden box" features [container])
  (object ::apple label "apple" features [takeable])
  (object ::shelf label "stone shelf" features [surface no-desc])

  (room ::hall label "Hall" features [lit]
        desc "A long hall with a low ceiling."
        contains [::lamp ::key ::rag ::alcove ::box ::shelf])
  (room ::pit  label "Pit" desc "Nothing to see." contains [::apple]))

(use-fixtures :each (t/with-world build!))

(defn- setup! [] (e/set-actor! ::you) (e/place! ::you ::hall))

(deftest an-object-with-nothing-to-say-declines
  (is (nil? (e/description ::key)) "no desc, no fdesc")
  (is (some? (e/description ::lamp))))

(deftest fdesc-retires-once-the-thing-is-moved
  (is (re-find #"hangs from a bracket" (e/describe-object ::lamp)))
  (e/move! ::lamp ::hall)
  (is (re-find #"lies here" (e/describe-object ::lamp)) "now the ordinary desc"))

(deftest a-desc-may-be-a-fn-of-the-world
  (swap! e/OBJECTS assoc-in [::pit ::e/description-detailed]
         (fn [ctx] (when (seq (e/contents (:self ctx) (:objects ctx)))
                     "Something is down here.")))
  (is (some? (e/description ::pit)))
  (e/remove! ::apple)
  (is (nil? (e/description ::pit)) "nil is the decline, and needs no second call"))

(deftest mute-objects-gather-into-one-stock-line
  (setup!)
  (let [s (e/describe-contents ::hall)]
    (is (= 1 (count (re-seq #"You can see" s))) "exactly one stock sentence")
    (is (re-find #"an iron key, a wet rag, and a wooden box here" s)
        "oxford comma at three")
    (is (not (re-find #"alcove" s)) "no-desc objects are not announced")
    (is (not (re-find #"\byou\b" s)) "and the actor is not scenery")))

(deftest a-no-desc-object-still-reports-what-is-on-it
  (setup!)
  (e/move! ::key ::shelf)
  (let [s (e/describe-contents ::hall)]
    (is (not (re-find #"a stone shelf" s))
        "the shelf never joins the stock line -- it is unannounced")
    (is (re-find #"Sitting on the stone shelf is an iron key" s)
        "but the key on it would otherwise vanish")))

(deftest containers-say-contains-and-surfaces-say-on
  (setup!)
  (e/open! ::box)
  (e/move! ::key ::box)
  (is (re-find #"the wooden box contains an iron key" (e/describe-contents ::hall)))
  (e/move! ::key ::shelf)
  (is (re-find #"Sitting on the stone shelf" (e/describe-contents ::hall))))

(deftest a-shut-container-hides-its-contents
  (setup!)
  (e/move! ::key ::box)
  (is (not (re-find #"contains" (e/describe-contents ::hall))))
  (e/open! ::box)
  (is (re-find #"contains" (e/describe-contents ::hall))))

(deftest describe-room-is-name-then-desc-then-contents
  (setup!)
  (let [s (e/describe-room ::hall)]
    (is (re-find #"^Hall" s))
    (is (re-find #"low ceiling" s) "first visit shows the long description")))

(deftest brief-verbose-and-an-explicit-look
  (setup!)
  (e/look!)                                                  ; marks it visited
  (is (not (re-find #"low ceiling" (e/describe-room ::hall)))
      "brief mode: seen it once, that's enough")
  (is (re-find #"low ceiling" (e/describe-room ::hall (e/context) {:full? true}))
      "an explicit LOOK always shows it")
  (e/set-verbosity! e/v-verbose)
  (is (re-find #"low ceiling" (e/describe-room ::hall)))
  (e/set-verbosity! e/v-superbrief)
  (is (= "Hall" (e/describe-room ::hall)) "superbrief: the name only"))

(deftest darkness-short-circuits
  (setup!)
  (e/place! ::you ::pit)
  (is (= "It's too dark to see." (e/describe-room ::pit))))

(deftest description-order-is-stable
  (setup!)
  (is (apply = (repeatedly 8 #(e/describe-contents ::hall)))))

(deftest look!-is-the-only-printer
  (setup!)
  (is (= "" (t/out #(e/describe-room ::hall))) "computing prints nothing")
  (is (re-find #"Hall" (t/out #(e/look!))))
  (is (e/feature-set? ::hall ::e/f-visited) "and look! is what marks it visited"))

(deftest articles-are-inferred-from-the-label
  (is (= "a brass lamp"  (e/stringify-tell-token :a ::lamp)))
  (is (= "an iron key"   (e/stringify-tell-token :a ::key)))
  (is (= "An iron key"   (e/stringify-tell-token :A ::key)))
  (is (= "the brass lamp" (e/stringify-tell-token :the ::lamp)))
  (testing "flags exist only for the words English disagrees with"
    (e/set-feature ::key ::e/f-consonant-article)
    (is (= "a iron key" (e/stringify-tell-token :a ::key)) "forced, however wrong")))

(deftest tell!-tokens-and-carriage-returns
  (is (= "You see the brass lamp.\n"
         (t/out #(e/tell! "You see " :the ::lamp "." :>>))))
  (is (= "one\n\ntwo\n" (t/out #(e/tell! "one" :>> :>> "two" :>>)))
      "two returns give a blank line"))
