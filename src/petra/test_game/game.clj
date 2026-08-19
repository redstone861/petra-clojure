(ns petra.test-game.game
  "The head of the test game: what it is, and nothing about being run.

  Requiring this namespace loads the whole game's data -- that is the only
  contract a runner needs. There is deliberately no -main here, no boot call, and
  no reference to petra.core: dependencies point game -> engine only."
  (:require [petra.engine.core :refer [def-game]]
            [petra.test-game.dungeon :as d]
            [petra.test-game.verbs]))                       ; its verbs and syntax file

(def-game
  title  "A Test Dungeon"
  author "nobody in particular"
  actor  ::d/you
  start  ::d/god-kingdom)
