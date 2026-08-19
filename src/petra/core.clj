(ns petra.core
  "The runner. Boots a game named at the command line and knows no game itself --
  there is no compile-time require of any world in here, so the arrow points
  runner -> game -> engine and never back.

  Two configs, because they answer different questions:

    the GAME's config   what the game IS -- title, author, actor, starting room.
                        Declared with engine's `def-game`, lives in the game
                        folder as a CONFIG var, pure data.
    the RUN's options   how THIS run is set up -- which game to load, where saves
                        go. None of the game's business; it lives here."
  (:require [petra.engine.core :as engine]
            [petra.engine.parser :as parser]))

(def DEFAULT-OPTS
  {:game     'petra.test-game.game
   :save-dir nil})                                          ; TODO: where save files will live

(defn load-config
  "require a game namespace and read the CONFIG that `def-game` put there.
  requiring it is what loads the game's rooms, objects and verbs, so this is the
  whole of the runner's knowledge about any game."
  [game-ns]
  (require game-ns)
  (if-let [v (ns-resolve game-ns 'CONFIG)]
    @v
    (throw (ex-info "game namespace has no CONFIG"
                    {:game-ns game-ns
                     :hint "declare one with petra.engine.core/def-game"}))))

(defn play-turn!
  "one full cycle: parse, and either report the failure or run the turn. Returns
  the turn state, or nil if the input never became a command -- a parser failure
  costs no time, which is 6.2."
  [input]
  (let [p (parser/parse input)]
    (if-let [err (:petra.engine.parser/error p)]
      (do (engine/tell! err :>>) nil)
      (do
        ;; when the parser had to choose a referent, say which -- above the result,
        ;; so the player can see it was understood before seeing what happened
        (when-let [note (:petra.engine.parser/note p)]
          (engine/tell! note :>>))
        (engine/turn! (:verb p) :dobj (:dobj p) :iobj (:iobj p) :direction (:direction p))))))

(defn main-loop
  "read, parse, perform, until the game ends or the input does."
  []
  (loop []
    (print "\n> ") (flush)
    (when-let [line (read-line)]
      ;; echo only when input isn't a terminal, so piped transcripts read properly
      (when-not (System/console) (println line))
      (if (clojure.string/blank? line)
        (recur)
        (let [st (play-turn! line)]
          (cond
            (:over? st) nil                                 ; died, or quit
            :else (recur)))))))

(defn -main
  "usage: lein run [game-namespace]"
  [& args]
  (let [opts (cond-> DEFAULT-OPTS
               (seq args) (assoc :game (symbol (first args))))]
    (engine/boot! (load-config (:game opts)))
    (main-loop)
    (println)))
