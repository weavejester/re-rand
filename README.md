re-rand is a [Clojure](http://clojure.org) library for generating random strings
from regular expressions.

Examples
--------

    => (use 're-rand)
    => (re-rand #"[A-Z]{4}")
    "UTFR"
    => (re-rand #".+:\d+")
    "s9QziMg1:85592"
    => (re-rand #"[^0-9][0-9]")
    "i2"
    => (re-rand #"b(an)+a")
    ["bananananananana" "an"]
