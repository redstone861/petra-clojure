(ns petra.macros)

;(defmacro handler [name body & {:keys [args]}]
;  `(defn ~name [verb# pre-verb# k-dir# k-ind# k-actor# objects# & {:keys ~args}] ~body)
;  )

(defmacro handler [name body]
  (list 'defn name ['verb 'pre-verb 'k-dir 'k-ind 'k-actor 'objects] body))


(handler nails-h (println verb))