(ns petra.test-game.handlers
  (:require [petra.engine.core :as engine]
            [petra.engine.macros :refer [handler]]
            [petra.test-game.verbs :as v]))

;; an action routine returns truthy when it has handled the input, and nil/false
;; to let the input fall through to the next handler in perform!'s chain. see
;; petra.engine.macros/handler for everything the body has in scope.

(handler nails-h
  ;; verbs are keywords now, so a responder can be specific instead of catching
  ;; everything aimed at it. This is a COND over verb identity -- ZIL's <VERB? ...>.
  (cond
    (= verb ::v/take)    (engine/tell! "The nails are rusted into place." :>>)
    (= verb ::v/examine) (engine/tell! "Three square-cut nails, thick with rust." :>>)
    :else nil))

(handler wet-room-desc
  (str "Water sheets down every wall of this cramped chamber."
       (when (seq (engine/contents self objects))
         " Something has been left on the floor.")))

(handler pail-desc
  (when (engine/open? self)
    "A rusty pail lies on its side, lid off."))

(handler announce-arrival
  (engine/tell! "A cold wind cuts through you as you enter " :the self "." :>>))

(handler announce-departure
  (engine/tell! "The air warms as you leave " :the self "." :>>))

(handler drip
  (engine/tell! "Water drips somewhere in the dark." :>>))

(handler bell-response
  (engine/tell! "The bell's note hangs in the damp air." :>>))
