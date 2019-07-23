(ns errs.core)

(defn if-ok [res cont]
  (let [atom (first res)
        vars (rest res)]
    (if (= atom :ok)
      (apply cont vars)
      res)))

(defn ok [res]
  [:ok res])

(defn error [err]
  [:error err])

(defn try-ok
  ([f]
   (fn [& args]
     (apply try-ok f args)))
  ([f & args]
   (try (apply f args)
        (catch Exception e
          (error (.getMessage e))))))
