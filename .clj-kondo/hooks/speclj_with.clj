(ns hooks.speclj-with
  "clj-kondo expansion for speclj's `with` family. Each declares a lazily
   evaluated, deref-able symbol whose value is the last form of the body
   (accessed via `@name`). clj-kondo's built-in view treats the binding as a
   plain value, so `@name` trips the :type-mismatch deref check. Rewrite the
   form to bind the symbol to an atom over the body, which models the
   deref-able correctly and keeps the body analyzed for its own findings."
  (:require [clj-kondo.hooks-api :as api]))

(defn with-binding [{:keys [node]}]
  (let [[_ sym & body] (:children node)
        new-node (api/list-node
                  (list
                   (api/token-node 'def)
                   sym
                   (api/list-node
                    (list
                     (api/token-node 'clojure.core/atom)
                     (api/list-node
                      (list* (api/token-node 'do) body))))))]
    {:node (with-meta new-node (meta node))}))
