;; A little playthrough of what the engine can do so far. No parser yet, so the
;; "commands" are verb fns handed straight to turn! -- but everything downstream
;; of the parser is real.
;;
;;   lein run -m clojure.main dev/demo.clj

(require '[petra.engine :as e]
         '[petra.text :as t])
(refer 'petra.engine :only '[object room])

;; ---------------------------------------------------------------------------
;; verbs
;; ---------------------------------------------------------------------------
;; Verbs are still bare fns rather than keywords, which is the next thing to fix:
;; note how a responder below has to compare them by identity, and how walking
;; needs one fn per direction because there's nowhere in the context to put one.

(def v-look (fn [_] (e/look!)))

(def v-take
  (fn [{:keys [k-dir k-actor]}]
    (e/move! k-dir k-actor)
    (e/tell! "Taken." :>>)))

(def v-drop
  (fn [{:keys [k-dir k-here]}]
    (e/move! k-dir k-here)
    (e/tell! "Dropped." :>>)))

(def v-open
  (fn [{:keys [k-dir]}]
    (e/set-feature k-dir ::e/f-open)
    (e/tell! "You open " :the k-dir "." :>>)))

(def BELL-RUNG (atom false))

(def v-ring
  (fn [{:keys [k-dir]}]
    (reset! BELL-RUNG true)
    (e/tell! "You shake " :the k-dir ". The note goes on much too long." :>>)))

;; a meta-verb: it says something, but the world does not move
(def v-verbose
  (fn [_]
    (e/set-verbosity! e/v-verbose)
    (e/no-time-passes!)
    (e/tell! "Maximum verbosity." :>>)))

;; The bit we deliberately kept OUT of the engine: resolving a direction against
;; the room's exits is the verb layer's job. goto! does the moving.
(defn- walking [dir]
  (fn [{:keys [k-here]}]
    (if-let [thunk (get (e/prop k-here e/kw-room-exits) dir)]
      (when-let [dest (thunk)] (e/goto! dest))
      (e/tell! (e/say ::t/cant-go) :>>))
    ::e/handled))

(def go-north (walking e/kw-north))
(def go-south (walking e/kw-south))
(def go-east  (walking e/kw-east))
(def go-west  (walking e/kw-west))
(def go-down  (walking e/kw-down))
(def go-up    (walking e/kw-up))

(def walk-verbs #{go-north go-south go-east go-west go-down go-up})

;; ---------------------------------------------------------------------------
;; handlers
;; ---------------------------------------------------------------------------

;; A room responder, sitting in the chain ahead of the verb default -- this is
;; ZIL's M-BEG, and vetoing movement is exactly what it was for.
(defn crypt-h [{:keys [verb k-actor]}]
  (when (and (walk-verbs verb) (e/ultimately-in? ::deed k-actor))
    (e/tell! "You have the deed in your hands, and the shelves do not like it." :>>)))

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
      contains [::lantern ::key ::sack]
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
          [west ::gallery never "The gallery floor is long gone. There is nothing to walk on."]])

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
      to [[west ::hall]])

(object ::verger label "verger" desc "The verger is watching you and saying nothing.")

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

(e/set-actor! ::you)
(println "\n================ PETRA ================")
(e/goto! ::gatehouse)

(cmd "take lantern"  v-take :dir ::lantern)
(cmd "drop lantern"  v-drop :dir ::lantern)
(cmd "look"          v-look)
(cmd "north"         go-north)
(cmd "west"          go-west)
(cmd "east"          go-east)
(cmd "down"          go-down)
(cmd "open trapdoor" v-open :dir ::trapdoor)
(cmd "down"          go-down)
(cmd "up"            go-up)
(cmd "south"         go-south)
(cmd "take lantern"  v-take :dir ::lantern)
(cmd "verbose"       v-verbose)
(cmd "north"         go-north)
(cmd "open strongbox" v-open :dir ::strongbox)
(cmd "take bell"     v-take :dir ::bell)
(cmd "ring bell"     v-ring :dir ::bell)
(cmd "east"          go-east)
(cmd "west"          go-west)
(cmd "down"          go-down)
(cmd "open urn"      v-open :dir ::urn)
(cmd "take deed"     v-take :dir ::deed)
(cmd "up"            go-up)                              ; the room refuses
(cmd "drop deed"     v-drop :dir ::deed)
(cmd "up"            go-up)                              ; now it lets you
(cmd "down"          go-down)                            ; back down once too often

(println "\n=======================================")
