# Petra — engine status and next steps

*Last updated 2026-08-18.*

## What this project is

Petra is a text-adventure engine in Clojure. Two halves:

1. **The engine** (`src/petra/engine/core.clj`) — objects, rooms, containment,
   descriptions, event dispatch, and the handler chain that decides who responds
   to the player's input. Its lineage is Infocom's ZIL, via
   `design/Learning_ZIL_Steven_Eric_Meretzky_1995.pdf`.

2. **The parser** (`src/petra/engine/syntactic.clj`) — the actual idea, and the reason
   this isn't just a ZIL port. Instead of ZIL's flat syntax file
   (`<SYNTAX GIVE OBJECT TO OBJECT = V-GIVE>`), verbs carry real **selectional
   frames** on their lexical entries, and parsing is Merge: `entry`/`psel` build
   frames, `mrg` labels a mother from its head, `greedy-sel-merge` returns every
   possible next workspace state. The verb and its arguments are then recovered
   from the derivation by θ-role (`highest-matching {:role :DO}`), so PRSA/PRSO/PRSI
   aren't parser outputs — they're queries over a tree.

   Consequences: no separate syntax file, verb synonymy is just several lexical
   entries sharing one internal keyword, prepositions become heads that assign
   roles, and ZIL's switch-verb hack (`V-SGIVE`) disappears. Design notes in
   `design/lexer-parser.txt` (lexical ambiguity, `CAT :DIR`, and the decision to
   lex separable verbs like "pick up" as single items rather than model movement).

**Guiding principle:** ZIL is a source of design problems and vocabulary, not a
spec. Before porting a ZIL mechanism, ask what Z-machine limitation produced it —
a 240-global budget, a 64-property ceiling, one function pointer per object. Most
of them don't survive the question.

**Second principle:** the person authoring a game is a programmer, but should feel
like they're programming as little as possible. The DSL should read as its own
language, and the engine should never make an author bookkeep something it can
work out itself.

`src/petra/test_game/dungeon.clj` and `.../handlers.clj` are **test fixtures**, not
game content. There is no dungeon that isn't a test dungeon. Rewrite them freely
to exercise new engine surface.

## Layout

    src/petra/core.clj      the runner: boots a game named on the command line.
                            project.clj's :main. Knows no game -- it resolves one
                            by namespace at runtime.
    src/petra/engine/       the engine
      core.clj              objects, containment, describers, dispatch, the turn
      text.clj              every line of English the engine can print
      macros.clj            the `handler` macro
      syntactic.clj         the Merge parser
      parser.clj            stub: will drive syntactic and hand turn! a verb
    src/petra/test_game/    a test game, not content -- rewrite freely
      game.clj              def-game: the head of the game, pure data
      dungeon.clj  handlers.clj
    dev/                    scratch.clj (harness), demo.clj (playthrough)

Directory `test_game`, namespace `petra.test-game.*` — Clojure munges hyphens to
underscores in paths, so the two must differ.

## What the engine has now

| Area | What's there |
|---|---|
| Registry | one atom `OBJECTS`, flat map of key → property map |
| Convention | everything public takes/returns object **keys**, never maps; `obj`/`prop` have an `objects`-snapshot arity so one turn reads one consistent world |
| Containment | parent's `::contains-local` set is authoritative; location is derived. `contents` `location-of` `in?` `ultimately-in?` `room-of` `see-inside?` `visible-descendants` `lit?` `move!` `remove!` |
| Features | a set of keywords; authored as bare symbols — `features [lit open]` |
| Output | `tell!` with `:a :the :A :The :>>` tokens; `indefinite-article` infers a/an from the label |
| Text | all engine prose lives in `src/petra/engine/text.clj` as frames with `{{named slots}}`; `say`/`fill`/`merge-frames!` |
| Describers | `describe-object` `describe-contents` `describe-room` `look!`; brief/verbose/superbrief |
| Events | `::on {event fn}` map, open to game-defined namespaced events; `notify!` `listener` |
| Verbs | keyword → behaviour in `VERBS`; `def-verb` with `handle`/`pre`/`turn?`; `verb-def` `verb-handler` `pre-action` `consumes-turn?` |
| Dispatch | `perform!` runs actor → room → pre-action → indirect → direct → verb default, and returns whether anything consumed the input |
| Turn | `turn!` runs one turn and returns its state; `*turn*`/`record-turn!`/`no-time-passes!` for turn-scoped facts; `die!`/`game-over?` |
| Exits | `to [[north ::room via ::door]]` compiles to DATA. `resolve-exit` is a pure query; `exit-to` `exit-message` `exit-exists?` `exit-door` `exit-permanent?` `exit-destination` `exit-has-destination?` `exit-handler` `exit-notes` `exit-directions` `direction-to` |
| Definition | `def-object`/`def-room`, aliased `object`/`room`; property table in `prop-symbols-pre` |

Everything the engine calls out to takes **one** argument: the turn context
(`verb pre-verb k-dobj k-iobj direction k-actor k-here self objects`).

## Decisions already made — don't relitigate these

- **RARG is deleted, not renamed.** ZIL passed a tag saying *why* a routine was
  called, because an object held one function pointer and had to multiplex. Here
  dispatch is the data structure, and nothing is ever told why it was called:
  - `handle <fn>` — responder; truthy return = "I consumed the input"
  - `desc <string-or-fn>` — describer; returns a string, or `nil` to decline
  - `on {event <fn>}` — notification; return value is discarded

  The three have genuinely different return-value contracts, which is why they're
  three properties and not one. `M-WINNER` became `(= self k-actor)`; `M-BEG`
  became chain position; PRSO-vs-PRSI became `(= self k-dir)` / `(= self k-ind)`.

- **Containment has one direction.** The parent set is the source of truth;
  `location-of` scans. Fine at text-adventure scale, and it's one function to
  index behind if that ever changes.

- **Describers return strings; `look!` is the only thing that prints.** This is
  what makes ZIL's `M-OBJDESC?` query pass structurally impossible rather than
  merely unused — you can ask an object what it *would* say without it saying it.

- **The engine contains no player-facing prose.** It's all in `text.clj`.

- **The engine maintains its own bookkeeping features.** `move!` sets
  `::f-touched` (which retires an `fdesc`); `look!` sets `::f-visited` (which
  makes a long description appear once). The author never sets either. Same
  reasoning killed `VOWELBIT`: the article is inferred from the label, and the
  `vowel`/`consonant` flags exist only for the words English disagrees with.

- **The actor is not scenery in its own room** — `describe-contents` skips it, so
  no game has to remember to flag its own player object.

- **A responder's return value means exactly one thing: did you consume the
  input.** Anything else a handler learns about the *turn* goes to `*turn*`. ZIL
  tried the other way first and retracted it (§8.4): passing `M-FATAL` up through
  returns meant every intermediate frame had to forward it, "lots of extra code
  and lots of chances to screw up."

- **`*turn*` has exactly ONE door and a CLOSED list of contents.** The list today
  is `:time-passed?`, and that is all. To qualify, a fact must be *meaningless
  outside one turn* — which rules out score, move count, what "it" refers to (it
  persists into the next turn), and the daemon queue: all of those are
  game-scoped and belong in their own atoms. `:handled?`/`:over?` are outcomes
  `turn!` computes, so they live in its return value, keeping the invariant that
  everything *in* the atom was recorded there deliberately during the turn. The
  only plausible future addition is `:output?` for a `WAIT` verb, and it isn't
  added because nothing consumes it. `record-turn!` is private, so the list can't
  grow without also adding a named fn and editing the documented list. Expanding
  it needs an argument first.

  The turn is deliberately *not* in the context. Two paths to one atom invited raw
  `swap!`s that bypassed the closed list, and bought visibility only for handlers
  that skip the `handler` macro — which doesn't destructure it anyway.

- **The actor's start position is stated in ONE place — the config's `start` — and
  the author never puts the actor into the world.** `boot!` places it. Asking the
  author to also declare it structurally (`contains [::you]`) forced the engine to
  interpret what "in the start room" means: held by the room? by a chair in it? and
  the answer is going to change — the actor may end up in local-globals — which is
  an implementation matter no author should have to hold an opinion about, still
  less revise. So `boot!` requires the actor to be held by *nothing*, and errors if
  it finds it placed.

  `boot!` runs one pass over the containment tree (`parent-index`) answering two
  questions at once: is the actor unplaced, and is anything held by two parents.
  `move!`/`remove!` preserve the one-parent invariant but `contains` in a
  definition cannot, and two rooms both listing an object would otherwise show up
  as something that teleports depending on which scan won.

  Consequence: `boot!` is once-only on a fresh world. Booting twice throws, which
  is right — restarting means rebuilding the world, not re-booting a used one.

- **`boot!` does not `goto!`, so no `ev-leave`/`ev-enter` fire at startup.**
  Beginning somewhere is not arriving there, and firing either on a room you never
  left or entered would be a lie. ZIL's GO did a LOOK, not a GOTO. Only `look!`
  runs. (Consequence: the "is `start` a room?" check `goto!` used to provide for
  free had to be written out explicitly, or a `start` naming an object surfaced as
  a vaguer complaint about the actor's position.)

- **Exits are data, and resolving them is a query.** The DSL compiles to a spec
  map per direction rather than a closure, so a room's exits are inspectable —
  `exit-directions` and `direction-to` exist because of it. `resolve-exit` returns
  a result and does nothing: exactly one of `::to` (may go), `::say` (may not, and
  these are the words), or `::run` (undecided — a `with` exit).

  That purity is structural, not a promise about author discipline: a `with` fn is
  never run during resolution, only handed over. Which is what makes the
  pre-action pattern safe — resolve, intervene (open a door you have the key to),
  decline, and let the verb default resolve *again*. Under the old
  thunk-that-prints design, resolving twice printed the refusal twice.

  **A `with` fn is a responder**, with the same contract as an object's `handle`
  and a verb's: truthy means it dealt with the attempt. It may `goto!`, print,
  mutate, `no-time-passes!`, or `die!` — so "effects but no movement" is a property
  of the exit itself and needn't be split into a room handler. `wrap-exit-fn` wraps
  it at definition time so a decline still says something, which keeps the
  `::cant-go` frame inside the engine.

  **Keys are implementation; functions are the interface** — the same rule that
  makes `kw-contains-local` public but never typed. Game code asks named questions.
  A game's own annotations come back under its own keywords via `exit-notes`, so
  there is no table of `::e/` keys to memorise. Note `exit-to` (resolved) and
  `exit-destination` (declared) are different facts: a shut `if` exit declares a
  destination it will not take you to, and a `with` exit's declared destination is
  a hint for cataloguing that nothing reads at resolution.

- **A verb is a keyword; the registry maps it to behaviour.** That indirection is
  the point: `(= verb ::take)` becomes possible, and many input words can name one
  internal verb (ZIL's `SLICE → V-CUT`). The registry maps **keyword → behaviour**;
  a lexicon will map **words → keyword** — so synonymy is entirely lexical and has
  no business in `def-verb`, which is what ZIL's `VERB-SYNONYM` looks like it gets
  wrong until you notice it lives in the syntax file rather than the verbs file.

  `handle` reuses the object property key on purpose: same contract, different
  chain position. `pre` belongs to the *verb*, not to a call site — §9.4 requires a
  PRSA to carry the same pre-action across all its syntaxes — so `perform!` lost
  its `:pre` argument. `turn?` defaults true and is stated only on meta-verbs
  (ZIL's `GAME-VERB?`); `turn!` seeds `:time-passed?` from it, with
  `no-time-passes!` remaining the dynamic override for a one-off.

  `handle` is required, because a verb with no last resort leaves inputs
  unanswered and "a non-response is always a no-no" (§1.2). Unknown verb at
  dispatch, a missing `handle`, and an unknown verb property all throw.

  `pre-verb` stays in ctx even though it's derivable, so that ctx alone describes
  the whole dispatch without consulting the registry. Note this is *not* a licence
  to group verbs by their shared pre-action — that's coincidental coupling that
  breaks when the pre-action is split. An explicit `tags` property would be the
  honest mechanism if verb families ever matter.

- **A game is data, and says nothing about being run.** `def-game` compiles to a
  `CONFIG` map in the game's own namespace; the game folder holds no `-main`, no
  boot call and no reference to the runner, so dependencies point runner → game →
  engine and never back. Two configs, answering different questions: the *game's*
  config is what the game IS (title, author, actor, starting room), while the
  *run's* options are how this run is set up (which game, where saves go) and live
  in `petra.core`. Config keys are engine keywords because they are universal
  behaviours the engine interprets, but they're authored as bare symbols like
  every other property in the DSL — extend `config-symbols` as more turn out to be
  universal.

- **Where the engine ends: the engine owns what its data model ENTAILS; the game
  owns what the data model leaves open.** Petra commits to a classic Connected
  Rooms layout — rooms holding objects, movement between them, `::exits`,
  `::f-visited` — so "arriving in a room shows you the room" is not one policy
  among several, it is an entailment. `describe-room`'s structure is the read side
  of the commitment `def-room` makes on the write side, and `goto!`'s ordering is
  a mechanism guarantee rather than a preference. Both are engine.

  It comes out as two* layers, with a third one somewhere in between (text):

  | layer | owns | e.g. |
  |---|---|---|
  | `engine/core.clj` | what the data model entails | describer structure, brief/verbose/first-visit, `goto!`'s ordering |
  | `text.clj` | the wording | `::contents-listing`, `::too-dark`, `::died` |
  | game / lexicon | bindings and content | which words mean "look", which rooms exist |

  At its simplest, the distinction is between the engine and the author's game. 
  There's also the text which the engine prints, which will fundamentally stay 
  similar across games because of what the engine expects from it, but is worth
  separating because an authored game might change its style. 

  There is **no fourth "substrate" layer coming**, and the describers are not
  destined to leave. ZIL's substrate/game split was not an architectural
  boundary — §14.3 describes it as what's left after taking a previous game and
  stripping out its specifics, and every game compiled its own editable copy. It
  was a workflow artifact, so it is not evidence about layering here. (The one
  thing ZIL did treat as inviolable was the parser.)

  Corollary for later: exit *resolution* is engine, being the reader for
  `::exits`. Only the *binding* is open — which words mean north, whether the verb
  is WALK or GO, whether a refused move costs a turn. Likewise the eventual
  `::look` verb default is an empty wrapper around `look!`, which is why `look!`
  belongs in the engine: making it a verb would move the wrapper and leave
  everything of substance behind.

- **Death is an abort, not a flag.** ZIL's "fatal" lumped together three
  unrelated things. Discarding queued input and "the clock didn't advance" are
  *notes the turn carries to its end*; death must stop the rest of the handler and
  the rest of the chain, or you get "You are crushed by the boulder. Taken." So
  flags go to `*turn*` and `die!` throws. ZIL's `JIGS-UP` didn't return either.

## Next steps, in order

**0. ~~Real tests~~ — deliberately deferred (2026-08-18).** Sam's call, and a
reasonable one: the engine's shape is still moving, so tests pinning today's API
would be rewritten before catching anything. What *did* earn its keep is running
code — every real bug found so far (`:A` recursion, exits returning `tell!`'s
truthy value, the actor listed as scenery, `feature-set?` handed a map) came from
executing, not reading. So `dev/scratch.clj` is the compromise: a throwaway
harness that survives the session, with no `lein test` hookup and no maintenance
obligation. Rewrite it freely.

Still outstanding: `test/petra/core_test.clj` holds the generated failing
`(is (= 0 1))`. Eventually worth pinning a handful of *invariants* that are design
decisions rather than implementation ("a key appears in at most one parent's
contains-set" survives every API change contemplated) — about five assertions,
not a suite.

**1. ~~`goto!`~~ — done (2026-08-18).** Raises `ev-leave`, `move!`s the actor,
raises `ev-enter`, then `look!`. That order is the reason it's engine and not just
any handler calling `move!`: a leave listener must still see the room it's losing,
an enter listener's `k-here` must already be the destination, and the description
must come last because an enter listener can change what there is to see (ZIL's
crypt moves a poltergeist in on `M-ENTER`). Throws loudly on a destination that
isn't a room, or with no actor set — an exit pointing at a typo would otherwise
leave `k-here` nil and the game silently describing nothing. First consumer of
`ev-enter`/`ev-leave`, and the first time the exit DSL has actually executed as
part of engine flow.

Caught while building it: `goto!` calling `(look!)` forced the long description on
every arrival, because `look!`'s 0-arity means "the player typed LOOK". That made
`::f-visited` inert — arrival now passes `full? false`, so brief mode shows the
long description only on a first visit (§11.3).

**No `walk!`.** A verb default is the same thing as a handler — a fn taking ctx —
so `walk!` would be a verb default, and the only open part of it is the binding
(which words mean north, whether a refused move costs a turn). The resolution
logic is engine, and lands with the exits overhaul.

**2. ~~Verb identity~~ — done (2026-08-19).** *Was: small to build, wide in
consequence.*
`verb` in ctx is currently a **function**, so `(= verb ::take)` is impossible.
It should be a namespaced keyword plus a registry:

```clojure
(def-verb ::take
  turn? true
  pre    (fn [ctx] ...)      ; ZIL's PRE-TAKE
  handle (fn [ctx] ...))     ; ZIL's V-TAKE, the default
```

The parser needs this because the lexicon has to *name* verbs, and ZIL's
input-word-vs-internal-name split (`SLICE` → `V-CUT`, §9.1) is exactly the
indirection a keyword registry preserves and a bare fn destroys. Get it right and
verb synonymy is free: many lexical entries, one keyword. Two things fall out —
`perform!` loses its `:pre` argument (the registry knows), and the currently-unread
`pre` property moves from the object to the verb, where ZIL had it.

`dev/demo.clj` is the evidence. Because verbs are bare fns there, walking needs
one fn per direction (`go-north`, `go-south`, …) since the context has nowhere to
put a direction; and the crypt's responder has to compare verbs by **object
identity**, which forces the walk verbs to be `def`d rather than built inline —
`(walking dir)` returns a fresh fn each call, so an inline one would never match.
That friction is the argument, in working code.

Design note from the exits discussion: a direction wants its **own context slot**,
not `k-dir`. `k-dir` means direct object, a direction isn't an object, and
handlers now rely on `(= self k-dir)` to tell PRSO from PRSI. `design/lexer-parser.txt`
already leans this way with `CAT :DIR`.

**3. ~~Exits overhaul~~ — done (2026-08-19).** Exits compile to data, one spec map
per direction. `resolve-exit` is the single interpreter and a *pure query* — it
computes, prints nothing, and never runs a `with` fn. The `exit-*` accessors are
the whole interface, so game code contains no engine keywords. Compile-time
validation on all seven malformed shapes; `never` takes no destination; `with` may
omit one. Removed: `exit-in-direction`, `with-to`, `no-exit`, `tell-door-cant-go`,
and `postprocess-props`/`prop-keys-post` (exits were that hook's only user).

**4. `in-scope`** — *the parser contract.* "Which objects can the player refer to
this turn?" = the room, what's visibly in it, the actor's inventory, the room's
`share` list, and `GLOBALS`. Then the boundary is clean: **engine supplies scope,
parser matches `noun`/`adj` against it.**

The concrete bite: the exit DSL already writes `via ::green-door`, but the door
isn't in either room's `share` list — so the moment scope exists, the player won't
be able to refer to the door in order to open it. Doors are the canonical
local-global (§7.4). Use that as the test case. (ZIL also split `VISIBLE?` from
`ACCESSIBLE?` — build one `in-scope` now, split it when a verb needs it.)

**5. ~~The turn~~ — done (2026-08-18).** `turn!` runs one whole turn: the
responder chain, then `ev-each-turn` on the room if the clock advanced. Returns
`{:time-passed? :handled? :over?}` plus anything else a handler recorded, so new
turn-scoped facts never change its signature. `perform!` is now just the chain,
and stays separately callable — ZIL's habit of re-dispatching an input as another
verb from inside a handler.

`no-time-passes!` is ZIL's `GAME-VERB?` and works from arbitrary depth with no
ctx threaded, which is the property the whole design was chosen for. `die!`
aborts. `pf-fatal`/`pf-dead`/`perform-pass-up?` are deleted.

What remains is the *loop*, which needs the parser for input: read, parse, `turn!`,
stop if `:over?`. Multi-command input lines are deliberately out of scope — with
one input per turn there are no queued commands to discard, so ZIL's `M-FATAL`
has no reason to exist here. If chaining ever arrives, it's `record-turn!` with
one more key and a caller that stops early.

**6. `take`/`drop`, a `PLAYER` object, `f-takeable`.** The first two real verbs,
which prove verb identity and `in-scope` together. (`dev/demo.clj` fakes both
today, without any takeability check at all.)

After that the parser has a stable surface to target: a verb keyword, a scope
set, and `perform!`.

**Deferrable, none of it gating the parser:** score, status line, death/`JIGS-UP`,
size and capacity, vehicles, `CLOCKER`/`QUEUE` daemons. Save/restore is nearly
free whenever wanted — the world is one atom — with the one wrinkle that handler
fns don't serialise, so persist `OBJECTS` minus fn-valued properties and re-run
the world definitions on load.

## Open questions and known weak spots

- **Dead vocabulary, waiting on `in-scope`:** `share`, `noun`, `adj`, and the
  `GLOBALS`/`SHARED`/`INTANGIBLES` root containers have zero readers. (`pre` is
  live now, on verbs.) `::exits` is compiled but never executed, so the whole exit DSL is
  still untested in situ — step 1 fixes that. `ev-enter`/`ev-leave` are declared
  but never raised (`ev-each-turn` now is, by `turn!`).

- **Dynamic-var caveat.** `*turn*` is bound by `turn!`, so anything that escapes
  that binding loses it — a lazy seq realised later, or work handed to another
  thread. Fine today (handlers run synchronously, the describers realise eagerly),
  worth remembering if either changes. `record-turn!` no-ops outside a turn rather
  than throwing, so repl and scratch calls are safe.

- **Description order** is alphabetical by label, ties broken by key. Arbitrary
  but deterministic, and at least explicable from the output rather than from the
  internal keywords. An author who wants a specific order writes the prose into
  the room's `desc` and marks those objects `no-desc`.

- **Fixture prose** still lives in `dungeon.clj`/`handlers.clj`. That's deliberate
  — object descriptions *are* the game's writing; `text.clj` is for the engine's
  stock phrasing.

- `lein check` is clean apart from one pre-existing reflection warning at
  `syntactic.clj:130` (`.indexOf` on an untyped target).

- `lein run` works, and `lein run <game-ns>` boots any game. `petra.core` has no
  compile-time require of any world.

## Running it

```
lein check                  # compile everything
lein run -m clojure.main <script.clj>
```

There's no playable loop yet — `petra.core/main-loop` prints "Nothing to see
here." Step 1 changes that.
