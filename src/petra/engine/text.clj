(ns petra.engine.text
  "Every line of English the engine itself can print. Edit this file to change the
  game's voice; engine.clj holds no player-facing prose of its own.

  A frame is a string with {{named slots}}. The engine fills them from a map, so
  a frame uses as many or as few slots as it likes and ignores any it doesn't
  mention -- there is no argument count to keep in step with the engine.

  Slot forms:

    {{items}}       the value, printed as-is: already-assembled prose, a number
    {{the door}}    that object's label with \"the\"   -- obeys `no-article`
    {{a door}}      with \"a\" or \"an\"               -- the engine picks which
    {{an apple}}    same thing; write whichever reads better in the source
    {{The door}}    capitalised, article included
    {{label door}}  the bare label, no article at all

  Naming a slot the engine doesn't supply prints [?name], so a typo shows up the
  first time you playtest the line rather than silently vanishing.

  A frame may also be a fn of the args map, if a line needs real branching. Keep
  that rare: prose that depends on the state of the world usually wants to live
  in an object's `desc` instead, which has the whole engine available to it."
  (:refer-clojure :exclude []))

(def FRAMES
  {;; --- the parser talking to the player -------------------------------------
   ::unknown-word   "[I don't know the word \"{{word}}\".]"
   ::not-a-sentence "[I don't understand that sentence.]"
   ::say-something  "[I beg your pardon?]"
   ::cant-see       "You can't see any {{thing}} here."
   ::which-one      "Which do you mean, {{things}}?"
   ;; printed above the action when the parser had to choose for you
   ::chose          "({{things}})"

   ;; --- movement --------------------------------------------------------------
   ::cant-go   "You can't go that way."
   ::door-shut "{{The door}} is closed."

   ;; --- starting and ending --------------------------------------------------
   ::banner "{{title}}\nby {{author}}"

   ;; --- the end ---------------------------------------------------------------
   ;; printed after whatever message was passed to `die!`
   ::died "*** You have died. ***"

   ;; --- looking around -------------------------------------------------------
   ::too-dark "It's too dark to see."

   ;; the stock line for everything in a room that had nothing specific to say.
   ;; {{items}} arrives already punctuated -- see the list frames below.
   ::contents-listing "You can see {{items}} here."

   ;; printed just after whatever container is holding them
   ::container-holds "It seems that {{the container}} contains {{items}}."
   ::surface-holds   "Sitting on {{the container}} is {{items}}."

   ;; --- how lists of things get punctuated -----------------------------------
   ;; these three build "a", "a and b", "a, b, and c".
   ;; to drop the Oxford comma, set ::list-last to " and ".
   ::list-separator ", "
   ::list-two       " and "
   ::list-last      ", and "})
