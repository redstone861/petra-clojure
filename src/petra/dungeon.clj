(ns petra.dungeon
  (:require [petra.engine :as engine]
            [petra.handlers :as handlers :refer :all])
  )

;alias attribute functions
(def label engine/with-label)
(def adjective engine/with-label-heads)
(def features engine/with-features)
(def to engine/with-to)
(def handler engine/with-handler)
(def object engine/def-object)
(def room engine/def-room)

(object ::rusty-pail (label "rusty pail"))

(object ::three-nails
        (label "three nails")
        (adjective "three") ;TODO adjectives will not work like this
        (features ::engine/f-no-article))

(def CYCLOPS-DEAD (atom false))

(room ::god-kingdom
          (label "God's Kingdom")
          (features ::engine/f-no-article)
          (to
            [:south ::aqua-room]))

(room ::aqua-room
          (label "Aqua Room")
          (features ::engine/f-vowel-article)
          (to
            [:north ::god-kingdom]
            [:east ::green-room :if CYCLOPS-DEAD]))