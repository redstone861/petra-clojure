(ns petra.macros)

(defmacro handler
  "define a fn the engine can call out to, with the turn context destructured
  into scope:

    verb      the verb fn identified by the parser (ZIL's PRSA)
    pre-verb  the verb's pre-action, if it has one
    k-dir     key of the direct object, or nil (PRSO)
    k-ind     key of the indirect object, or nil (PRSI)
    k-actor   key of whoever is acting this turn (WINNER)
    k-here    key of the room k-actor is in (HERE)
    self      key of the object this fn was installed on
    objects   snapshot of the world, for the `objects`-arity readers

  there is no tag saying why you were called. what your return value means is
  decided by the property you install this under:

    handle <fn>          responder    truthy = I consumed the input; nil/false
                                      passes the input to the next in the chain
    desc <fn>            describer    a string, or nil to decline and let the
                                      caller use a default
    on {enter <fn> ...}  notification return value is discarded

  which is why one macro serves all three."
  [name & body]
  `(defn ~name [ctx#]
     (let [{:keys [~'verb ~'pre-verb ~'k-dir ~'k-ind ~'k-actor ~'k-here
                   ~'self ~'objects]} ctx#]
       ~@body)))
