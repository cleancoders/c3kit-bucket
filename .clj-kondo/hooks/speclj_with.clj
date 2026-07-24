(ns hooks.speclj-with
  (:require [clj-kondo.hooks-api :as api]))

;; Speclj's `with` / `with!` / `with-all` / `with-all!` declare a lazily
;; evaluated, deref-able symbol accessed as `@name` (its value is the last form
;; of the body). Mapping them to `clojure.core/def` via :lint-as binds the
;; symbol to the value directly, so `@name` becomes a deref of a non-derefable
;; type — and with :type-mismatch escalated to :error every `@with-binding`
;; gets flagged "Expected: deref, received: <type>".
;;
;; This hook rewrites `(with sym & body)` into
;;   `(def sym (clojure.core/atom (do body...)))`
;; modelling the binding as deref-able so `@sym` type-checks, while the body is
;; still analysed for its own warnings.
(defn with-binding [{:keys [node]}]
  (let [[_ sym & body] (:children node)
        new-node (api/list-node
                  (list (api/token-node 'def) sym
                        (api/list-node
                         (list (api/token-node 'clojure.core/atom)
                               (api/list-node (list* (api/token-node 'do) body))))))]
    {:node (with-meta new-node (meta node))}))
