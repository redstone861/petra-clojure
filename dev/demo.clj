;; A little playthrough of what the engine can do so far. No parser yet, so the
;; "commands" are verb fns handed straight to turn! -- but everything downstream
;; of the parser is real.
;;
;;   lein run -m clojure.main dev/demo.clj

(require '[petra.engine.core :as e]
         '[petra.engine.text :as t])
(refer 'petra.engine.core :only '[object room def-game def-verb])

;; ---------------------------------------------------------------------------
;; verbs
;; ---------------------------------------------------------------------------
;; A verb is a keyword; the registry holds its behaviour. Note what this bought:
;; one ::walk instead of one fn per direction, and a responder below that says
;; (= verb ::walk) instead of comparing function objects.

(def-verb ::look
  handle (fn [_] (e/look!)))

(def-verb ::take
  handle (fn [{:keys [k-dobj k-actor]}]
           (e/move! k-dobj k-actor)
           (e/tell! "Taken." :>>)))

(def-verb ::drop
  handle (fn [{:keys [k-dobj k-here]}]
           (e/move! k-dobj k-here)
           (e/tell! "Dropped." :>>)))

(def-verb ::open
  handle (fn [{:keys [k-dobj]}]
           (e/set-feature k-dobj ::e/f-open)
           (e/tell! "You open " :the k-dobj "." :>>)))

(def BELL-RUNG (atom false))

(def-verb ::ring
  handle (fn [{:keys [k-dobj]}]
           (reset! BELL-RUNG true)
           (e/tell! "You shake " :the k-dobj ". The note goes on much too long." :>>)))

;; a meta-verb: it says something, but the world does not move. `turn? false`
;; replaces having to remember to call no-time-passes! in the body.
(def-verb ::verbose
  turn?  false
  handle (fn [_]
           (e/set-verbosity! e/v-verbose)
           (e/tell! "Maximum verbosity." :>>)))

;; Resolving is the engine's job and returns data; deciding what to DO about it is
;; the verb layer's. Not one engine keyword in here -- the exit-* accessors are the
;; whole interface, and a `with` exit hands back a fn for us to run.
(def-verb ::walk
  handle (fn [{:keys [k-here direction objects] :as ctx}]
           (let [r (e/resolve-exit k-here direction objects)]
             (cond
               (e/exit-to r)      (e/goto! (e/exit-to r))
               (e/exit-handler r) ((e/exit-handler r) ctx)
               :else              (e/tell! (e/exit-message r) :>>)))
           ::e/handled))

;; ---------------------------------------------------------------------------
;; handlers
;; ---------------------------------------------------------------------------

;; A room responder, sitting in the chain ahead of the verb default -- this is
;; ZIL's M-BEG, and vetoing movement is exactly what it was for.
(defn crypt-h [{:keys [verb k-actor]}]
  (when (and (= verb ::walk) (e/ultimately-in? ::deed k-actor))
    (e/tell! "You have the deed in your hands, and the shelves do not like it." :>>)))

;; A `with` exit is a responder bound to a direction, so it has every freedom a
;; handler has: it prints what it likes, changes the world, and decides whether the
;; actor moves at all. This is the "effects but no movement" case.
(def ASKED (atom false))

(defn tower-stair [ctx]
  (if @ASKED
    (do (e/tell! "The verger stands aside." :>>)
        (e/goto! ::belfry))
    (do (reset! ASKED true)
        (e/tell! "The verger steps in front of the stair." :>>
                 "\"Not without asking,\" he says, so you ask." :>>)
        ::e/handled)))                                   ; consumed; no movement

;; An each-turn listener with a counter, which is how ZIL's I-TRUCK worked.
(def DRIPS (atom 0))

(defn crypt-tick [_]
  (let [n (swap! DRIPS inc)]
    (cond
      (>= n 7) (do (e/tell! "The seventh shelf comes down all at once." :>>)
                   (e/die! "You are underneath it."))
      (odd? n) (e/tell! "Somewhere further in, water is dripping." :>>)
      :else    (e/tell! "Grit sifts down from the ceiling." :>>))))

;; ---------------------------------------------------------------------------
;; the world
;; ---------------------------------------------------------------------------

(object ::you label "you" features [no-article])

(object ::lantern
        label "brass lantern"
        features [lit]                                   ; lights whatever holds it
        fdesc "A brass lantern hangs from a bracket by the arch, still burning."
        desc  "A brass lantern lies here, still burning.")

(object ::key label "iron key")                          ; no desc: joins the stock line
(object ::sack label "sodden sack")                      ; likewise

(room ::gatehouse
      label "Gatehouse"
      features [lit]
      desc "A roofless stone gatehouse. The rain has got at everything. An arch opens north."
      contains [::lantern ::key ::sack]                     ; NOT ::you -- boot! places the actor
      on {leave (fn [_] (e/tell! "The gate groans on its hinges as you pass." :>>))}
      to [[north ::hall]])

(object ::trapdoor
        label "trapdoor"
        features [container no-desc])                    ; the room's prose mentions it

(object ::strongbox
        label "strongbox"
        features [container]
        desc "A strongbox is bolted to the floor."
        contains [::bell])

(object ::bell label "handbell")

(room ::hall
      label "Great Hall"
      features [lit]
      desc (fn [_] (str "Roof beams, most of them fallen. A stair goes down into the dark."
                        (if (e/open? ::trapdoor)
                          " The trapdoor at its foot stands open."
                          " A trapdoor at its foot is shut.")))
      contains [::trapdoor ::strongbox]
      to [[south ::gatehouse]
          [down ::crypt via ::trapdoor]
          [east ::chapel if BELL-RUNG]
          [west never "The gallery floor is long gone. There is nothing to walk on."]])

(object ::urn label "alabaster urn" features [container] contains [::deed])
(object ::deed label "deed of tenure")

(room ::crypt
      label "Crypt"
      ;; deliberately not `lit`: bring your own light
      desc "Six stone shelves, and room cut for a seventh."
      contains [::urn]
      handle crypt-h
      on {enter     (fn [_] (e/tell! "The cold comes up through your boots." :>>))
          each-turn crypt-tick}
      to [[up ::hall via ::trapdoor]])

(room ::chapel
      label "Chapel"
      features [lit]
      desc "Whitewash, and a rood screen with most of its saints prised off."
      ;; an enter listener that changes what there is to see, so the description
      ;; that follows has to notice
      on {enter (fn [_] (e/move! ::verger ::chapel)
                  (e/tell! "Someone straightens up from the far pew." :>>))}
      to [[west ::hall]
          [up with tower-stair]])                        ; no destination: the fn decides

(object ::verger label "verger" desc "The verger is watching you and saying nothing.")

(room ::belfry
      label "Belfry"
      features [lit]
      desc "A single bell, and a long way down through the boards."
      to [[down ::chapel]])

;; ---------------------------------------------------------------------------
;; the playthrough
;; ---------------------------------------------------------------------------

(def over? (atom false))

(defn cmd [label verb & opts]
  (when-not @over?                                       ; a real loop stops here
    (println)
    (println (str "> " label))
    (let [st (apply e/turn! verb opts)]
      (when-not (:time-passed? st) (println "   -- the clock did not advance --"))
      (when (:over? st)
        (reset! over? true)
        (println "   -- over; a main loop would break out of the loop here --"))
      st)))

;; what the game IS -- the same declaration a game folder would hold
(def-game
  title  "The Gatehouse"
  author "a demo"
  actor  ::you
  start  ::gatehouse)

(println)
(e/boot! CONFIG)

(cmd "take lantern"   ::take :dobj ::lantern)
(cmd "drop lantern"   ::drop :dobj ::lantern)
(cmd "look"           ::look)
(cmd "north"         ::walk :direction e/kw-north)
(cmd "west"          ::walk :direction e/kw-west)
(cmd "east"          ::walk :direction e/kw-east)
(cmd "down"          ::walk :direction e/kw-down)
(cmd "open trapdoor"  ::open :dobj ::trapdoor)
(cmd "down"          ::walk :direction e/kw-down)
(cmd "up"            ::walk :direction e/kw-up)
(cmd "south"         ::walk :direction e/kw-south)
(cmd "take lantern"   ::take :dobj ::lantern)
(cmd "verbose"        ::verbose)
(cmd "north"         ::walk :direction e/kw-north)
(cmd "open strongbox" ::open :dobj ::strongbox)
(cmd "take bell"      ::take :dobj ::bell)
(cmd "ring bell"      ::ring :dobj ::bell)
(cmd "east"          ::walk :direction e/kw-east)
(cmd "up"            ::walk :direction e/kw-up)         ; the verger objects
(cmd "up"            ::walk :direction e/kw-up)         ; ...and then does not
(cmd "down"          ::walk :direction e/kw-down)
(cmd "west"          ::walk :direction e/kw-west)
(cmd "down"          ::walk :direction e/kw-down)
(cmd "open urn"       ::open :dobj ::urn)
(cmd "take deed"      ::take :dobj ::deed)
(cmd "up"            ::walk :direction e/kw-up)                              ; the room refuses
(cmd "drop deed"      ::drop :dobj ::deed)
(cmd "up"            ::walk :direction e/kw-up)                              ; now it lets you
(cmd "down"          ::walk :direction e/kw-down)                            ; back down once too often

(println "\n=======================================")
