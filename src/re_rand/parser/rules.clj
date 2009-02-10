;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; re-rand.parser.rules:
;;
;; Rules to parse a regular expression into a series of string generating
;; functions.

(ns re-rand.parser.rules
  (:use re-rand.parser.tools)
  (:use [clojure.set :only (difference)]))

(defn rnd-choice
  [coll]
  (let [v (vec coll)]
    (v (rand-int (count v)))))

(defn take-fn
  [n f]
  (take n (repeatedly f)))

(defn rnd-seq
  [f min max]
  (take-fn (+ (rand-int (- (inc max) min)) min) f))

(defn parse-int
  [n]
  (Integer/parseInt n))

(defn char-range
  [from to]
  (map char
    (range (int (first from))
           (inc (int (first to))))))

(def digits
  (char-range "0" "9"))

(def whitespace
  " \t\n\f\r")

(def alphanumeric
  (concat
    (char-range "A" "Z")
    (char-range "a" "z")
    digits))

(def valid-any-chars
  (concat
    alphanumeric
    "_-/+*=%()[]{}!?:;,. \t\n"))

(defn invert
  "Return a set of characters that do not contain any of chars."
  [chars]
  (difference (set valid-any-chars) (set chars)))

(defn combine-groups
  "Combine output groups using a function."
  [func tokens]
  (reduce
    (fn [groups token]
      (if (vector? token)
        (apply vector
          (apply str (groups 0) token)
          (func (subvec groups 1) token))
        (assoc groups 0
          (str (groups 0) token))))
    [""]
    tokens))

(def repeat-limit 20)

(def escaped
  (attach
    (match #"\\(.)")
    (fn [[_ char]]
      (cond
        (= char "d") #(rnd-choice digits)
        (= char "s") #(rnd-choice whitespace)
        (= char "w") #(rnd-choice alphanumeric)
        (= char "D") #(rnd-choice (invert digits))
        (= char "S") #(rnd-choice (invert whitespace))
        (= char "W") #(rnd-choice (invert alphanumeric))
        :otherwise    (constantly char)))))

(def literal
  (attach
    (match #"[^\\{}.+*()\[\]^$]")
    constantly))

(def any-char
  (attach
    (match #"\.")
    (fn [_] #(rnd-choice valid-any-chars))))

(defn sequence-of-chars
  [src]
  (let [f (match #"((\\.|[^\^\-\[\]\\])+)([^-]|$)")]
    (if-let [[[_ m _ s] src] (f src)]
      [(.replace m "\\" "")
       (str s src)])))

(def range-of-chars
  (attach
    (match #"(\\.|[^\^\-\[\]\\])-(\\.|[^\^\-\[\]\\])")
    (fn [[_ from to]] (char-range from to))))

(defn get-char-list
  [char-groups invert?]
  (let [chars (apply concat char-groups)]
    (if invert?
      (invert chars)
      chars)))

(def char-class
  (attach
    (series
      (match #"\[")
      (match #"\^?")
      (many (choice sequence-of-chars range-of-chars))
      (match #"\]"))
    (fn [[_ invert? char-groups _]]
      (let [chars (get-char-list char-groups (seq invert?))]
        #(rnd-choice chars)))))

(declare pattern)

(def sub-pattern
  (attach
    (series
      (match #"\(")
      (forward pattern)
      (match #"\)"))
    (fn [[_ fs _]]
      #(apply str (map apply fs)))))

(def single
  (choice escaped
          sub-pattern
          any-char
          char-class
          literal))

(def zero-or-more
  (attach
    (series single (match #"\*"))
    (fn [[f _]] #(apply str (rnd-seq f 0 repeat-limit)))))

(def one-or-more
  (attach
    (series single (match #"\+"))
    (fn [[f _]] #(apply str (rnd-seq f 1 repeat-limit)))))

(def zero-or-one
  (attach
    (series single (match #"\?"))
    (fn [[f _]] #(apply str (rnd-seq f 0 1)))))

(def exactly-n
  (attach
    (series single (match #"\{(\d+)\}"))
    (fn [[f [_ n]]]
      #(apply str
         (take-fn (parse-int n) f)))))

(def between-n-and-m
  (attach
    (series single (match #"\{(\d+),\s*(\d+)\}"))
    (fn [[f [_ n m]]]
      #(apply str
         (rnd-seq f (parse-int n) (parse-int m))))))

(def pattern
  (many
    (choice zero-or-more
            one-or-more
            zero-or-one
            exactly-n
            between-n-and-m
            single)))
