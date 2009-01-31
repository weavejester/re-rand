(ns rend.parser.rules
  (:use rend.parser.tools)
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
  (take-fn (+ (rand-int (- max min)) min) f))

(defn parse-int
  [n]
  (Integer/parseInt n))

(defn char-range
  [from to]
  (map char
    (range (int (first from))
           (inc (int (first to))))))

(def valid-any-chars
  (concat
    (char-range "A" "Z")
    (char-range "a" "z")
    (char-range "0" "9")))

(def repeat-limit 20)

(def escaped
  (attach
    (match #"\\(.)")
    (fn [[_ char]]
      (cond
        (= char "d") #(rnd-choice (char-range "0" "9"))
        (= char "s") #(rnd-choice " \t\n\f\r")
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
      (difference (set valid-any-chars) (set chars))
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

(def single
  (choice escaped
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

(def regex
  (many (choice zero-or-more
                one-or-more
                exactly-n
                between-n-and-m
                single)))