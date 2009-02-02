;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; rend
;;
;; Generate random strings that match the supplied regular expression.

(ns rend
  (:use rend.parser.rules))

(defn rend
  "Returns a random string that matches the regular expression."
  [re]
  (let [[parsed remainder] (pattern (str re))]
    (if (empty? remainder)
      (apply str (map apply parsed)))))
