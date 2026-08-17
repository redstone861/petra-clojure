(ns petra.handlers
  (:require [petra.engine :as engine]
            [petra.macros :refer [handler]]))

;; test fixtures for the engine's three call-out contracts. none of these
;; reference world keys -- they work off `self` and `objects`, which is what
;; makes a fn like this reusable across many objects. (ZIL action routines
;; hardcoded their own identity, as in AVOCADO-F's <REMOVE ,AVOCADO>, so they
;; could not be shared.)

;; --- responders: truthy = I consumed the input -------------------------------
;; TODO: verbs do not exist yet, so there is nothing to dispatch `verb` against.
;; once there are verb defaults this becomes a cond over verb identity, the way a
;; ZIL action routine is a COND over <VERB? ...>.

(handler nails-h
  (cond
    ;; ZIL distinguished these by comparing the PRSO/PRSI globals; here the same
    ;; question is asked of the context, and needs no tag either way.
    (= self k-ind) (engine/tell! "You can't use the nails for that." :>>)
    (= self k-dir) (engine/tell! "The nails are rusted into place." :>>)
    :else nil))

;; --- describers: a string, or nil to decline ---------------------------------

(handler wet-room-desc
  (str "Water sheets down every wall of this cramped chamber."
       (when (seq (engine/contents self objects))
         " Something has been left on the floor.")))

(handler pail-desc
  ;; returning nil is the whole of ZIL's M-OBJDESC? query: no second call needed
  ;; to ask whether this object intends to describe itself. when this declines,
  ;; the describers fold the pail into the stock "You can see ..." line instead.
  (when (engine/open? self)
    "A rusty pail lies on its side, lid off."))

;; --- notifications: return value is discarded --------------------------------

(handler announce-arrival
  (engine/tell! "A cold wind cuts through you as you enter " :the self "." :>>))

(handler announce-departure
  (engine/tell! "The air warms as you leave " :the self "." :>>))

(handler drip
  (engine/tell! "Water drips somewhere in the dark." :>>))

(handler bell-response
  (engine/tell! "The bell's note hangs in the damp air." :>>))
