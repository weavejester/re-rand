(ns rend
  (:use rend.parser.rules))

(defn rend
  [re]
  (let [[parsed remainder] (regex (str re))]
    (if (empty? remainder)
      (apply str (map apply parsed)))))
