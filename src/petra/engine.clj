(ns petra.engine)

(def ^:const kw-location ::location)
(def ^:const kw-label ::label)
(def ^:const kw-room-exits ::exits)
(def ^:const kw-handler ::handler)
(def ^:const kw-pre-handler ::pre-handler)
(def ^:const kw-features ::features)

#_ "
features:
::f-vowel-article : print name with /an/ instead of /a/ for :a tell!-token
::f-no-article : print name with no article, like /God/ instead of /the God/
"

;(def ^:const kw-contains-local ::contains-local)
(def ^:const kw-contains-shared ::contains-shared)
(def ^:const kw-description-first ::description-first)
(def ^:const kw-description-detailed ::description-detailed)
(def ^:const kw-description-function ::description-function)
(def ^:const kw-label-heads ::label-heads)
(def ^:const kw-label-modifiers ::label-modifiers)

(def ^:const kw-north ::north)
(def ^:const kw-east ::east)
(def ^:const kw-south ::south)
(def ^:const kw-west ::west)
(def ^:const kw-up ::up)
(def ^:const kw-down ::down)
(def ^:const kw-in ::in)
(def ^:const kw-out ::out)

(def OBJECTS (atom {}))
(def ACTOR (atom nil))                                      ; actor key

(def ^:const ROOMS ::ROOMS)
(def ^:const SHARED ::SHARED)
(def ^:const GLOBALS ::GLOBALS)
(def ^:const INTANGIBLES ::INTANGIBLES)

(defn object [object-key objects]
  (object-key objects)
  )

(defn with-in [location]
  "returns a feature mapping the location keyword to the given location."
  [kw-location location]
  )

(defmacro def-object [object-key properties]
  (let [pairs (partition 2 clauses)]
    (doseq [[prop _] pairs]
      (when-not (contains? (keys prop-symbols) prop)
        (throw (ex-info "Unknown preposition" {:prep prop}))))
    `(make-object ~name
       ~(into {}
              (map (fn [[p v]] [(prop-symbols p) v]) pairs))))
) ; this doesn't yet have the object key anywhere i don't think

(defn make-object [obj]
  (swap! OBJECTS assoc ) ; need to put the key somewhere. also, where does it go? rn it's literally just floating in the OBJECTS root list... i guess this is fine maybe??
  )

(defn make-room [room-key & properties]
  (apply def-object room-key (conj properties (with-in ROOMS)))
  )

(defn set-actor [actor-key]
  (swap! ACTOR actor-key)
  )

(def-object ROOMS)
(def-object SHARED)
(def-object GLOBALS)
(def-object INTANGIBLES)

(defn with-features [& features-kw-list]
  [kw-features (apply hash-set features-kw-list)]
  )

(defn with-handler [handler-fn]
  [kw-handler handler-fn]
  )

(defn with-pre-handler [pre-handler-fn]
  [kw-pre-handler pre-handler-fn]
  )

(defn with-contains-shared [& shared-keys]
  [kw-contains-shared (apply hash-set shared-keys)]
  )

(defn with-label-heads [& heads]
  [kw-label-heads (apply hash-set heads)]
  )

(defn with-label-modifiers [& modifiers]
  [kw-label-modifiers (apply hash-set modifiers)]
  )

(defn with-description-first [msg]
  [kw-description-first msg]
  )

(defn with-description-detailed [msg]
  [kw-description-detailed msg]
  )

(defn with-description-function [fun]
  [kw-description-function fun]
  )

(defn feature-set? [obj feature]
  (let [feat (kw-features obj)]
    (and feat (feature feat))
    )
  )

(defn handler [obj] (kw-handler obj))

; tell!

(defn o:label [obj]                                  ;todo consider: valid usage outside of tell!? (no, right?)
  (kw-label obj)
  )

(defn stringify-tell-token [token obj]
    (case token
      :a (str (if (feature-set? obj ::f-vowel-article) "an " "a ") (o:label obj))
      :the (str (if (feature-set? obj ::f-no-article) "" "the ") (o:label obj))
      nil
      )
  )

(defn split-by [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [[xs ys] (split-with pred s)]
        (if (seq xs)
          (cons xs (split-by pred ys))
          (let [!pred (complement pred)
                skip (take-while !pred s)
                others (drop-while !pred s)
                [xs ys] (split-with pred others)]
            (cons (concat skip xs)
                  (split-by pred ys))))))))

(defn cr? [x] (= :>> x))

(defn split-lines-cr [tell-msg]
  (split-by cr? tell-msg)                            ; usage: (tell! "Line 1" :>> "Line 2, handled now." :>>)  <-- note cr symbol at end of tell!
  )

(defn msg-line->str [msg objects]
  (let [f (first msg)
        r (rest msg)]
    (if-let [object-key (first r)]
      (if-let [token-applied (stringify-tell-token f (object-key objects))]
        (str token-applied (msg-line->str (rest r) objects))
        (str f (msg-line->str r objects))
        )
      (str f))))

(defn tell! [& msg]
  (let [lines (split-lines-cr msg)]
    (doseq [line lines]
      (let [[msg cr] (split-with (complement cr?) line)
            objects @OBJECTS]
        (print (msg-line->str msg objects))
        (if (not-empty cr) (newline))))
    "TELL-RET-TRUE"))                                          ;todo: something like (tell! "I don't think " ::the :dungeon/green-wall " would agree with you"), or (tell! "Staring at " :a :dungeon/enemy "? Dangerous!"), or (tell! "You eat the :l :dungeon/berries ".")

;PERFORM

(defn get-k-here [actor objects]
  "return the key of the room the actor is in (the nearest ancestor whose loc is ROOMS)"
  (let [k-loc (kw-location actor)
        loc (k-loc objects)]
    (if k-loc
      (if (= ROOMS k-loc)
        actor
        (get-k-here loc objects))
      nil)))

(defn perform-pass-up? [ret]
  "returns true if the value is one that perform! should pass up to its caller
  (i.e. the perform had a result that should affect the main loop in some way)."
  nil                                                       ;TODO check for things like ::pf-dead, ::pf-instant..?
  )

(defn perform-internal!
  [verb pre-verb k-dir k-ind]
    (let []
      (let [ret (or
                   (let [objects @OBJECTS
                         k-actor @ACTOR]
                     ((handler (k-actor objects)) verb pre-verb k-dir k-ind k-actor objects))
                   (let [objects @OBJECTS
                         k-actor @ACTOR]
                     ((handler (object (get-k-here (k-actor objects) objects) objects)) verb pre-verb k-dir k-ind k-actor objects ::room ::room-first))
                   (and pre-verb (pre-verb verb k-dir k-ind @ACTOR @OBJECTS))
                   (let [objects @OBJECTS
                         k-actor @ACTOR]
                     (and k-ind ((handler (k-ind objects)) verb pre-verb k-dir k-ind k-actor objects)))
                   (let [objects @OBJECTS
                         k-actor @ACTOR]
                     (and k-dir ((handler (k-dir objects)) verb pre-verb k-dir k-ind k-actor objects)))
                   (verb k-dir k-ind @ACTOR @OBJECTS))]
        (if (perform-pass-up? ret)
          ret
          false)))                                                  ; TODO M-END called at end of mainloop
  )

(defn perform!
  ([verb & {:keys [dir ind pre] :or {dir nil ind nil pre nil}}]
   (perform-internal! verb pre dir ind))
  )

(defn open? [door-key]
  (feature-set? door-key :f-open)
  )

(defn in? [obj k-in]
  (= (kw-location obj) k-in)
  )

(defn swap-object-attr [object-key attr-key fun & args]
  "basically, swaps the value of the attribute of the given key for the object with (apply fun current-value-of-attribute args)"
  (swap!
    OBJECTS
    #(assoc
       %
       object-key
       (let [old (object-key %)]
         (assoc old attr-key (apply fun (attr-key old) args))))))

(defn set-feature [object-key feature]
  (swap-object-attr object-key kw-features conj feature)
  )

(defn clear-feature [object-key feature]
  (swap-object-attr object-key kw-features disj feature)
  )

(def CANT-GO-MSG "You can't go that way.")                  ;todo

(defn exit-in-direction
  "Takes a direction keyword and some arguments (see design doc),
  returning [dir-key exitfn] where (exitfn) returns the new room or false if no movement should occur."
  ([dir-key room-key]
   [dir-key room-key]
   )
  ;if [atom]: conditional with global flag atom
    ;else [msg]: reject with message (otherwise, default) - never use else w/o if or if-open
  ;with [fun]: function-exit
  ;if-open [door]: conditional with door
  ;never [msg]: reject with message
  ([dir-key room-key & {if-atom :if else-msg :else with-fun :with if-open-object-key :if-open never-msg :never}]
    (if if-atom
      [dir-key #(if @if-atom room-key (tell! (or else-msg CANT-GO-MSG)))]
      (if with-fun
        [dir-key #(with-fun room-key)]
        (if if-open-object-key
          [dir-key
           (fn []
             (if (open? if-open-object-key)
               room-key
               (tell! (or else-msg CANT-GO-MSG))))]
          (if never-msg
            [dir-key #(tell! never-msg)]
            [dir-key room-key]))))))

(defn with-to [& exits]
  [kw-room-exits (into {} (map (fn [[dir-key & args]] (apply exit-in-direction dir-key args)) exits))]
  )

(defn with-label [label-string]
  [kw-label label-string]
  )
