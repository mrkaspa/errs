(ns errs.core)

(defn if-ok [res ^clojure.lang.IFn f]
  "If res has a shape {:ok any} calls continuation with 
   the any value"
  (if-let [vars (:ok res)]
    (apply f [vars])
    res))

(defn ok [res]
  {:ok res})

(defn error [err]
  {:error err})

(defn try-ok
  ([^clojure.lang.IFn f]
   (fn [& args]
     (apply try-ok f args)))
  ([^clojure.lang.IFn f & args]
    (try (let [res (apply f args)]
           (if (or (:ok res) (:error res))
             res
             (ok res)))
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
