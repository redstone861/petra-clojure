(ns petra.engine.parser
  (:require [petra.engine.core :as engine]))

(defn pkg-parse
  "package a parse result into the shape perform! takes, so that a parse can be
  applied with (apply engine/perform! (:verb p) (apply concat (dissoc p :verb)))
  or destructured directly. keys are unqualified to match the turn context that
  handlers see."
  [verb k-dir k-ind & {:keys [pre-verb]}]
  {:verb verb :dir k-dir :ind k-ind :pre pre-verb})

(defn parse
  "Big shit parse function. Returns (pkg-parse a di io)."
  [input]
  ;; TODO: drive petra.engine.syntactic -- lex, then search the merge space, then read
  ;; the verb off the root and the objects off the theta-roles (:DO, :IO) with
  ;; syntactic/highest-matching.
  )
