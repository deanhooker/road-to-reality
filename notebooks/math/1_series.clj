;; # Notes on Series

^{:nextjournal.clerk/toc true}
(ns notebooks.math.1-series
    (:refer-clojure
     :exclude [+ - * / zero? compare divide numerator denominator
               infinite? abs ref partial =])
    (:require [emmy.clerk :as ec]
              [emmy.env :as e :refer :all]
              [emmy.leva :as leva]
              [emmy.mafs :as mafs]
              [emmy.mathbox.plot :as p]
              [emmy.viewer :as ev]
              [nextjournal.clerk :as clerk]))

{::clerk/width :wide}

^{::clerk/visibility {:code :hide :result :hide}}
(ec/install!)

;; A series, or infinite series, is a an addition of infinitely many
;; terms, one after the other. The mathematics of series are a key
;; component of calculus and its generalization: mathematical analysis.

;; The terms of a series can be anything which can be added: numbers,
;; functions, matrices, etc.

;; ## Key Concepts
;; - Notation of series
;; - Summation (and partial summation) of a series
;; - Testing for convergence or divergence
;; - Ordering of terms

;; ## Notation
;; In general a series is an expression of the form (also written
;; in capital-sigma notation):

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}a_n = a_1 + a_2 + a_3 + ... + a_n + ...
")

;; e.g. Euler's number

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=0}^{\\infty}\\frac{1}{N!} = 1 + 1 + \\frac{1}{2} + \\frac{1}{6} + ... + \\frac{1}{N!} + ...
")
;; which we can construct in Clojure (displaying the first 5 terms):
^{::clerk/visibility {:code :show :result :hide}}
(defn render-series
  "Render a series in TeX. n parameter specifies number of terms
  to display. Defaults to 5."
  ([series]
   (render-series series 5))
  ([series n]
   (clerk/tex
    (str
     (->TeX (cons + (take n series)))
     " + ..."))))

(let [euler-term-fn (fn [n] (/ 1 (factorial n)))
      euler-series (->> (range)
                        (map euler-term-fn))]
  (render-series euler-series))

;; ## Summation
;; ### Partial Sum
;; Given a series

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}a_n
")

;; its partial sum is

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
s_n = \\sum_{n=1}^{n}a_n = a_0 + a_1 + a_2 + ... + a_n
")

;; ## Types of Series
