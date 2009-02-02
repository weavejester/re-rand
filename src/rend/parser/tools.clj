;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; rend.parser.tools:
;;
;; Generic functions for creating a LL recursive descent parser.

(ns rend.parser.tools)

(defn match
  "Create a rule to match a regular expression."
  [re]
  (fn [src]
    (let [m (re-matcher re src)]
      (if (.lookingAt m)
        [(re-groups m)
         (.substring src (.end m))]))))

(defn observe
  "Creates a rule, but doesn't reduce the source if it matches."
  [re]
  (let [f (match re)]
    (fn [src]
      (let [[m _] (f src)] 
        [m src]))))

(defn attach
  "Attach a function to transform the result of a rule."
  [rule f]
  (fn [src]
    (if-let [[grps src] (rule src)]
      [(f grps) src])))

(defn series
  "Create a new rule out of a series of individual rules."
  [& rules]
  (fn [src]
    (reduce
      (fn [[xs s] rule]
        (if (seq s)
          (if-let [[x s] (rule s)]
            [(conj xs x) s])))
      [[] src]
      rules)))

(defn choice
  "Create a new rule by returning the first rule that matches."
  [& rules]
  (fn [src]
    (some
      (fn [rule] (rule src))
      rules)))

(defn many
  "Match zero or more for a rule."
  [rule]
  (fn [src]
    (loop [xs [], src src]
      (if-let [[x src] (rule src)]
        (recur (conj xs x) src)
        [xs src]))))

(defmacro forward
  "Use a rule from a var that has yet to be defined."
  [rule]
  `(fn [src#] (~rule src#)))
