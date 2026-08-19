(ns petra.support
  "Test support. The engine keeps its state in global atoms, so the one thing a
  suite must do is isolate: each test gets the world its namespace declared at
  load time, and nothing it does leaks into the next.

  That is the whole reason the old dev/scratch.clj harness had to go. It was one
  linear script, so tests depended on the order they ran in and several 'failures'
  turned out to be state left behind three sections earlier."
  (:require [clojure.string :as string]
            [clojure.test :refer [is]]
            [petra.engine.core :as e]
            [petra.engine.lexicon :as lx]
            [petra.engine.parser :as ps]
            [petra.engine.text :as text]))

;; ---------------------------------------------------------------------------
;; isolation
;; ---------------------------------------------------------------------------

(def ^:private guarded
  "the atoms holding engine state. Note these are the atoms themselves, not vars:
  deref on a var yields the atom rather than its value, which is a good way to
  reset an atom to an atom and spend a while wondering why every label is nil."
  [e/OBJECTS e/ACTOR e/VERBS e/FRAMES e/VERBOSITY lx/WORDS ps/ASSERTIONS])

(defn snapshot [] (mapv deref guarded))
(defn install! [snap] (dorun (map reset! guarded snap)))

(defn fresh!
  "wipe back to a bare engine: the four root containers, no objects, no verbs, no
  words, the shipped frames. Namespaces share these atoms, so without this a
  lexicon accumulates across the whole suite and two `take`s collide."
  []
  (install! [{e/ROOMS {} e/SHARED {} e/GLOBALS {} e/INTANGIBLES {}}
             nil {} text/FRAMES e/v-brief [] @ps/ASSERTIONS]))

(defn isolate
  "a clojure.test :each fixture. Snapshots every engine atom, runs the test, and
  puts them all back -- so a test may move the world about freely."
  [t]
  (let [saved (snapshot)]
    (try (t) (finally (install! saved)))))

(defn with-world
  "an :each fixture that rebuilds the world from scratch before every test.

  `build!` may use the ordinary authoring macros -- object, room, def-verb,
  def-word all expand to plain runtime calls -- so a test namespace declares its
  world the way a game would, and gets a clean one each time."
  [build!]
  (fn [t]
    (let [saved (snapshot)]
      (try (fresh!) (build!) (t)
           (finally (install! saved))))))

(defn with-snapshot
  "an :each fixture that installs a prepared world before every test. For the
  shipped game, whose world is built by requiring its namespaces rather than by
  calling a fn."
  [snap]
  (fn [t]
    (let [saved (snapshot)]
      (try (install! snap) (t)
           (finally (install! saved))))))

;; ---------------------------------------------------------------------------
;; talking to the game
;; ---------------------------------------------------------------------------

(defn out
  "what `f` printed, as a string."
  [f]
  (with-out-str (f)))

(defn lines
  "what `f` printed, as a vector of non-blank lines."
  [f]
  (->> (string/split-lines (out f))
       (remove string/blank?)
       vec))

(defn said?
  "did `f` print something matching `re`?"
  [re f]
  (boolean (re-find re (out f))))

(defn parse
  "parse for the current actor and world."
  [input]
  (ps/parse input @e/ACTOR @e/OBJECTS))

(defn err [input] (::ps/error (parse input)))
(defn note [input] (::ps/note (parse input)))

(defn do!
  "parse `input` and run the turn, quietly. Returns the turn state, or the parse
  error string if it never became a command."
  [input]
  (let [p (parse input)]
    (if-let [e (::ps/error p)]
      e
      (out #(e/turn! (:verb p) :dobj (:dobj p) :iobj (:iobj p) :direction (:direction p)))
      )))

(defn play
  "run several inputs and return everything printed."
  [& inputs]
  (out (fn [] (doseq [i inputs]
                (let [p (parse i)]
                  (if-let [msg (::ps/error p)]
                    (e/tell! msg :>>)
                    (do (when-let [n (::ps/note p)] (e/tell! n :>>))
                        (e/turn! (:verb p) :dobj (:dobj p) :iobj (:iobj p)
                                 :direction (:direction p)))))))))

;; ---------------------------------------------------------------------------
;; small assertions with useful failure output
;; ---------------------------------------------------------------------------

(defn throws-info?
  "true if `f` throws an ex-info whose message matches `re`."
  [re f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo ex
         (boolean (re-find re (or (ex-message ex) ""))))
       (catch Throwable ex
         (boolean (re-find re (str (some-> (ex-cause ex) ex-message)
                                   (ex-message ex)))))))

(defn verb-of  [input] (:verb (parse input)))
(defn dobj-of  [input] (:dobj (parse input)))
(defn iobj-of  [input] (:iobj (parse input)))
(defn dir-of   [input] (:direction (parse input)))
