(ns petra.parser
  (:require [petra.engine :as engine]))

(defn pkg-parse
  [action direct-object indirect-object]
  {::verb action ::dir direct-object :ind indirect-object :pre-verb nil}
  )


(defn parse
  [input]
  "Big shit parse function. Returns (pkg-parse a di io)."

  )