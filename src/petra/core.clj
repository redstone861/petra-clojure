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
  (:require [petra.engine.core :as engine]))

(def DEFAULT-OPTS
  {:game     'petra.test-game.game
   :save-dir nil})                                          ; TODO: where save files will live

(defn load-config
  "require a game namespace and read the CONFIG that `def-game` put there.
  requiring it is what loads the game's rooms and objects, so this is the whole
  of the runner's knowledge about any game."
  [game-ns]
  (require game-ns)
  (if-let [v (ns-resolve game-ns 'CONFIG)]
    @v
    (throw (ex-info "game namespace has no CONFIG"
                    {:game-ns game-ns
                     :hint "declare one with petra.engine.core/def-game"}))))

(defn -main
  "usage: lein run [game-namespace]"
  [& args]
  (let [opts (cond-> DEFAULT-OPTS
               (seq args) (assoc :game (symbol (first args))))]
    (engine/boot! (load-config (:game opts)))
    ;; TODO: no parser yet, so there is no input to loop over. engine/turn! takes
    ;; a verb and returns {:time-passed? :handled? :over?}; the loop is that in a
    ;; loop, breaking on :over?.
    (engine/tell! :>> "[no parser yet -- nothing to type at]" :>>)))
