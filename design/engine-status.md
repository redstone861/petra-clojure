# Petra — engine status and next steps

*Last updated 2026-08-18.*

## What this project is

Petra is a text-adventure engine in Clojure. Two halves:

1. **The engine** (`src/petra/engine.clj`) — the substrate: objects, rooms,
   containment, descriptions, event dispatch, and the handler chain that decides
   who responds to the player's input. Its lineage is Infocom's ZIL, via
   `design/Learning_ZIL_Steven_Eric_Meretzky_1995.pdf`.

2. **The parser** (`src/petra/syntactic.clj`) — the actual idea, and the reason
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

`src/petra/dungeon.clj` and `src/petra/handlers.clj` are **test fixtures**, not
game content. There is no dungeon that isn't a test dungeon. Rewrite them freely
to exercise new engine surface.

## What the engine has now

| Area | What's there |
|---|---|
| Registry | one atom `OBJECTS`, flat map of key → property map |
| Convention | everything public takes/returns object **keys**, never maps; `obj`/`prop` have an `objects`-snapshot arity so one turn reads one consistent world |
| Containment | parent's `::contains-local` set is authoritative; location is derived. `contents` `location-of` `in?` `ultimately-in?` `room-of` `see-inside?` `visible-descendants` `lit?` `move!` `remove!` |
| Features | a set of keywords; authored as bare symbols — `features [lit open]` |
| Output | `tell!` with `:a :the :A :The :>>` tokens; `indefinite-article` infers a/an from the label |
| Text | all engine prose lives in `src/petra/text.clj` as frames with `{{named slots}}`; `say`/`fill`/`merge-frames!` |
| Describers | `describe-object` `describe-contents` `describe-room` `look!`; brief/verbose/superbrief |
| Events | `::on {event fn}` map, open to game-defined namespaced events; `notify!` `listener` |
| Dispatch | `perform!` runs actor → room → pre-action → indirect → direct → verb default, and returns whether anything consumed the input |
| Turn | `turn!` runs one turn and returns its state; `*turn*`/`record-turn!`/`no-time-passes!` for turn-scoped facts; `die!`/`game-over?` |
| Exits | `to [[north ::room via ::door]]`, five flavours (plain / `if` atom / `with` fn / `via` door / `never`), all compiling to thunks |
| Definition | `def-object`/`def-room`, aliased `object`/`room`; property table in `prop-symbols-pre` |

Everything the engine calls out to takes **one** argument: the turn context
(`verb pre-verb k-dir k-ind k-actor k-here self objects turn`).

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

**1. `goto!` / `walk!`** — ~25 lines, and the fastest route to something playable.
Nothing in the engine currently *runs* an exit thunk, so the whole exit DSL is
untested in situ. `walk!` looks up the room's exit for a direction and runs it;
`goto!` raises `ev-leave`, `move!`s the actor, raises `ev-enter`, and calls
`look!`. Also the first consumer of `ev-enter`/`ev-leave`, which are declared and
never raised.

**2. Verb identity** — *small to build, wide in consequence. Do before the parser.*
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

**3. `in-scope`** — *the parser contract.* "Which objects can the player refer to
this turn?" = the room, what's visibly in it, the actor's inventory, the room's
`share` list, and `GLOBALS`. Then the boundary is clean: **engine supplies scope,
parser matches `noun`/`adj` against it.**

The concrete bite: the exit DSL already writes `via ::green-door`, but the door
isn't in either room's `share` list — so the moment scope exists, the player won't
be able to refer to the door in order to open it. Doors are the canonical
local-global (§7.4). Use that as the test case. (ZIL also split `VISIBLE?` from
`ACCESSIBLE?` — build one `in-scope` now, split it when a verb needs it.)

**4. ~~The turn~~ — done (2026-08-18).** `turn!` runs one whole turn: the
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

**5. `take`/`drop`, a `PLAYER` object, `f-takeable`.** The first two real verbs,
which prove steps 2 and 3 together.

After that the parser has a stable surface to target: a verb keyword, a scope
set, and `perform!`.

**Deferrable, none of it gating the parser:** score, status line, death/`JIGS-UP`,
size and capacity, vehicles, `CLOCKER`/`QUEUE` daemons. Save/restore is nearly
free whenever wanted — the world is one atom — with the one wrinkle that handler
fns don't serialise, so persist `OBJECTS` minus fn-valued properties and re-run
the world definitions on load.

## Open questions and known weak spots

- **Dead vocabulary, all of it waiting on steps 2–3:** `share`, `noun`, `adj`,
  `pre`, and the `GLOBALS`/`SHARED`/`INTANGIBLES` root containers have zero
  readers. `::exits` is compiled but never executed, so the whole exit DSL is
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

- `src/petra/world.clj` and `src/petra/syntax.clj` are empty namespaces;
  `project.clj` still points `:main` at `petra.world`. `src/petra/#core.clj#` is
  an Emacs autosave and should be gitignored along with `target/`, `.lsp/`,
  `.clj-kondo/`, `.nrepl-port`.

## Running it

```
lein check                  # compile everything
lein run -m clojure.main <script.clj>
```

There's no playable loop yet — `petra.core/main-loop` prints "Nothing to see
here." Step 1 changes that.
