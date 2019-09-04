(ns errs.core
  (:require [clojure.spec.alpha :as s]))

(s/def ::ok int?)
(s/def ::error int?)
(s/def ::option (s/or :ok (s/keys :req-un [::ok])
                      :error (s/keys :req-un [::error])))

(defn if-ok [res f]
  "If res has a shape {:ok any} calls continuation with 
   the any value"
  (if-let [vars (:ok res)]
    (apply f [vars])
    res))
(s/fdef if-ok
  :args (s/cat
         :res ::option
         :f (s/fspec
             :args (s/cat :val ::ok)
             :ret ::option))
  :ret ::option)

(defn ok [res]
  {:ok res})

(defn error [err]
  {:error err})

(defn try-ok
  ([^clojure.lang.IFn f]
   (fn [& args]
     (apply try-ok f args)))
  ([^clojure.lang.IFn f & args]
   (try
     (let [res (apply f args)]
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
