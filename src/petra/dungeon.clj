(ns petra.dungeon
  "A test dungeon: exercises the engine's surface, not a game."
  (:require [petra.engine :as engine :refer [object room]]
            [petra.handlers :as h]))

(def CYCLOPS-DEAD (atom false))

;; a game-defined event. the engine has never heard of it; ::on is an open map,
;; so registering and raising it needs no engine change.
(def ^:const bell-rings ::bell-rings)

(object ::rusty-pail
        label "rusty pail"
        features [container lit]                            ; also the room's light source
        fdesc "Someone has left a rusty pail upturned beside the door."
        desc h/pail-desc)                                   ; fn: speaks only when open

(object ::three-nails
        label "three nails"
        noun ['nail] ; todo: add allomorphs (so that we don't have to duplicate the syntax with N "nail")
        adj ['three] ;TODO adjectives will not work like this
        features [no-article]
        desc "Three nails are driven deep into the wall."    ; string
        handle h/nails-h)

;; no desc and no fdesc: these fall into the stock "You can see ... here." line
(object ::tin-cup label "tin cup")
(object ::wet-rag label "wet rag")

;; mentioned by the room's own prose, so the describers must leave it alone
(object ::alcove
        label "shallow alcove"
        features [no-desc])

(object ::unicorn-horn
        label "unicorn horn"
        features [consonant])                                ; "a unicorn horn", not "an"

(object ::hourglass
        label "hourglass"
        features [vowel])                                    ; "an hourglass" is inferred anyway

(object ::green-door
        label "Green Door"
        features [no-article])

(room ::god-kingdom
      label "God's Kingdom"
      features [no-article lit]
      desc "Light without a source, and no floor you can find."
      to [[south ::aqua-room]])

(room ::aqua-room
      label "Aqua Room"
      ;; no `vowel` flag needed -- "Aqua Room" is inferred. dark until the pail arrives.
      desc h/wet-room-desc                                  ; fn: varies with contents
      contains [::rusty-pail ::three-nails ::tin-cup ::wet-rag ::alcove]
      on {enter        h/announce-arrival
          leave        h/announce-departure
          each-turn    h/drip
          ::bell-rings h/bell-response}                     ; game-defined event
      to [[north ::god-kingdom]
          [east ::green-room if CYCLOPS-DEAD]
          [west ::green-hallway never "The Green Hallway is forbidden."]
          [down ::cellar via ::green-door]])

(object ::you
        label "you"
        features [no-article])

(defn ring-bell!
  "raise the game-defined event, to show that notify! does not privilege the
  engine's own events."
  []
  (engine/notify! ::aqua-room bell-rings))
