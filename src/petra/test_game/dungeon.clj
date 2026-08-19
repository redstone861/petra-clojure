(ns petra.test-game.dungeon
  "A test dungeon: exercises the engine's surface, not a game."
  (:require [petra.engine.core :as engine :refer [object room]]
            [petra.test-game.handlers :as h]))

(def CYCLOPS-DEAD (atom false))

;; a game-defined event. the engine has never heard of it; ::on is an open map,
;; so registering and raising it needs no engine change.
(def ^:const bell-rings ::bell-rings)

(object ::rusty-pail
        label "rusty pail"
        features [container lit takeable]                            ; also the room's light source
        fdesc "Someone has left a rusty pail upturned beside the door."
        desc h/pail-desc)                                   ; fn: speaks only when open

(object ::three-nails
        label "three nails"
        noun ["nail" "nails"]                               ; beyond "nails" from the label
        adj  ["rusty"]
        features [no-article]
        desc "Three nails are driven deep into the wall."    ; string
        handle h/nails-h)

;; no desc and no fdesc: these fall into the stock "You can see ... here." line
(object ::tin-cup  label "tin cup"  features [takeable])
(object ::clay-cup label "clay cup" features [takeable])   ; so "cup" is ambiguous
(object ::wet-rag label "wet rag" features [takeable])

;; mentioned by the room's own prose, so the describers must leave it alone
(object ::alcove
        label "shallow alcove"
        features [no-desc])

(object ::unicorn-horn
        label "unicorn horn"
        features [consonant takeable])                                ; "a unicorn horn", not "an"

(object ::hourglass
        label "hourglass"
        features [vowel takeable])                                    ; "an hourglass" is inferred anyway

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
      contains [::rusty-pail ::three-nails ::tin-cup ::clay-cup ::wet-rag ::alcove]
      share [::green-door]                                  ; a door is referable from both sides
      on {enter        h/announce-arrival
          leave        h/announce-departure
          each-turn    h/drip
          ::bell-rings h/bell-response}                     ; game-defined event
      to [[north ::god-kingdom]
          [east ::green-room if CYCLOPS-DEAD]
          [west never "The Green Hallway is forbidden."]
          [down ::cellar via ::green-door]])

;; the far sides of aqua-room's exits, so every exit flavour has somewhere to go
(room ::green-room
      label "Green Room"
      features [lit]
      desc "Moss carpets the floor from wall to wall."
      to [[west ::aqua-room]])

(object ::shelf
        label "stone shelf"
        features [surface no-desc])                         ; the room's prose names it

(object ::table
        label "trestle table"
        features [surface]
        desc "A trestle table stands under the shelves.")

(room ::cellar
      label "Cellar"
      features [lit]
      share [::green-door]
      contains [::shelf ::table]
      desc "Stone shelves, the lowest of them within reach."
      to [[up ::aqua-room via ::green-door]])

(object ::you
        label "you"
        features [no-article])

(defn ring-bell!
  "raise the game-defined event, to show that notify! does not privilege the
  engine's own events."
  []
  (engine/notify! ::aqua-room bell-rings))
