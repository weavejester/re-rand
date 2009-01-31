Rend is a [Clojure](http://clojure.org) library for generating random strings
from regular expressions.

Examples
--------

    => (use 'rend)
    => (rend #"[A-Z]{4}")
    "UTFR"
    => (rend #".+:\d+")
    "s9QziMg1:85592"
    => (rend #"[^0-9][0-9]")
    "i2"
