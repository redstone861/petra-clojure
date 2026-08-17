# Petra — engine status and next steps

*Last updated 2026-08-17.*

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
| Dispatch | `perform!` runs actor → room → pre-action → indirect → direct → verb default |
| Exits | `to [[north ::room via ::door]]`, five flavours (plain / `if` atom / `with` fn / `via` door / `never`), all compiling to thunks |
| Definition | `def-object`/`def-room`, aliased `object`/`room`; property table in `prop-symbols-pre` |

Everything the engine calls out to takes **one** argument: the turn context
(`verb pre-verb k-dir k-ind k-actor k-here self objects`).

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

## Next steps, in order

**0. Real tests.** All verification so far has been throwaway scripts in a
session-scoped tmp dir; they're gone. `test/petra/core_test.clj` still contains
the generated failing `(is (= 0 1))`. Port the coverage into `test/petra/` as
`clojure.test`: containment invariants, the PERFORM chain order, the describers,
and frame rendering. Cheap, and it stops the next refactor from being a
leap of faith.

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

**4. Main loop, turn counter, `ev-each-turn`.** There's no notion of a turn at all
yet. Two things to settle here: which verbs consume time (ZIL's `GAME-VERB?` —
that's the `turn?` field above, much cheaper to design in now than to retrofit),
and how `pf-fatal` travels. See open questions.

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

- **`pf-fatal` is a bucket brigade, and ZIL abandoned that exact design.** §8.4:
  passing `M-FATAL` up through nested routine calls meant "lots of extra code and
  lots of chances to screw up," so they replaced it with settable-from-any-depth
  state. `perform-pass-up?` reintroduces the brigade. The main loop is its only
  consumer, so step 4 is the moment to decide — recommendation: put it in the
  turn's state, not the return value.

- **Dead vocabulary, all of it waiting on steps 2–3:** `share`, `noun`, `adj`,
  `pre`, and the `GLOBALS`/`SHARED`/`INTANGIBLES` root containers have zero
  readers. `::exits` is compiled but never executed. `ev-enter`/`ev-leave`/
  `ev-each-turn` are declared but never raised.

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
