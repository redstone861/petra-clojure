(ns petra.dungeon
  (:require [petra.engine :as engine]
            [petra.handlers :as handlers :refer :all])
  )

;alias attribute functions
(def object engine/def-object)
(def room engine/def-room)

(object ::rusty-pail 
        label "rusty pail")

(object ::three-nails
        label "three nails"
        noun ['nail] ; todo: add allomorphs (so that we don't have to duplicate the syntax with N "nail")
        adj ['three] ;TODO adjectives will not work like this
        features [::engine/f-no-article])

(def CYCLOPS-DEAD (atom false))

(room ::god-kingdom
          label "God's Kingdom"
          features [::engine/f-no-article]
          to [[:south ::aqua-room]])

(room ::aqua-room
          label "Aqua Room"
          features [::engine/f-vowel-article]
          to [[:north ::god-kingdom]
              [:east ::green-room :if CYCLOPS-DEAD]])
