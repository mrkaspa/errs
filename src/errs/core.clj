(ns errs.core)

(defn if-ok [res continuation]
  (if-let [vars (:ok res)]
    (apply continuation [vars])
    res))

(defn ok [res]
  {:ok res})

(defn error [err]
  {:error err})

(defn try-ok
  ([f]
   (fn [& args]
     (apply try-ok f args)))
  ([f & args]
   (try (ok (apply f args))
        (catch Exception e
          (error (.getMessage e))))))

(defn- wrap [item]
  `(if-ok (try-ok ~item)))

(defn- wrap-last [item]
  `(#(if-ok % (try-ok ~item))))

(defmacro ok-> [init & list]
  `(-> (ok ~init)
       ~@(map wrap list)))

(defmacro ok->> [init & list]
  `(->> (ok ~init)
        ~@(map wrap-last list)))
