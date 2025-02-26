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
\\sum_{n=0}^{\\infty}a_n
")

;; its $n$th partial sum is

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
s_n = \\sum_{n=0}^{n}a_n = a_0 + a_1 + a_2 + ... + a_n
")

;; Some partial sums have simpler closed form expressions, for instance
;; an arithmetic series:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
s_n = \\sum_{k=0}^{n}(a+kd) = a + (a+d) + (a+2d) + ... + (a+nd) = (n+1)(a+\\frac{1}{2}nd)
")

;; and a geometric series:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
s_n = \\sum_{k=0}^{n}ar^k = a + ar + ar^2 + ... + ar^n = a\\frac{1-r^{n-1}}{1-r}
")

;; if $r\not= 1$ or $s_n = a(n+1)$ if $r=1$.

;; ### Sum of a series
;; If the sequence of partial sums approaches a limit then a series
;; converges, otherwise it diverges. If it converges the limit is called
;; the _sum of the series_.

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
s = \\sum_{k=0}^{\\infty}a_k = \\lim_{n \\to \\infty}\\sum_{k=0}^{\\infty}a_k = \\lim_{n \\to \\infty}s_n
")

;; The difference between the sum and the $n$th partial sum is known as
;; the $n$th truncation error: $s-s_n = \sum_{k=n+1}^{\infty}a_k$.

;; ## Testing for convergence or divergence TODO

;; ## Operations
;; ### Addition
;; Two (or more) series can be added by the termwise sum:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{k=0}^{\\infty}a_k + \\sum_{k=0}^{\\infty}b_k = \\sum_{k=0}^{\\infty}a_k + b_k
")

;; In Clojure could write this as
^{::clerk/visibility {:code :show :result :hide}}
(defn add-series [s1 s2]
  (map + s1 s2))
(render-series (add-series (range) (range)))

;; The series resulting from addition is summable if the series' added
;; are summable, and is equal to the addition of the two sums of the
;; added series' sums. Two divergent series can sometimes be added to
;; create a convergent series (e.g. for divergent s1: s1 + -s1 gives a
;; series where each term is 0). For the addition of any convergent
;; and divergent series the result diverges.

;; Series addition, for real and complex numbers, is **associative**,
;; **commutative** and **invertible**.

;; ### Multiplication by a scalar
;; A series can be multiplied by a scalar _c_:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
c\\sum_{k=0}^{\\infty}a_k = \\sum_{k=0}^{\\infty}ca_k
")
^{::clerk/visibility {:code :show :result :hide}}
(defn scale-series [c series]
  (map #(* c %) series))
(render-series (scale-series 10 (range)))

;; Multiplication by a scalar is **associative**, **commutative**,
;; **invertible** and **distributes over** series addition.

;; Series addition and scalar multiplication gives the set of convergent
;; series and the set of series of real numbers the structure of a **real
;; vector space**. For series and convergent series of complex numbers
;; we get **complex vector spaces**. These vector spaces are infinite
;; dimensional.

;; ### Multiplication by a series
;; A series can be multiplied by a series, also called the _Cauchy product_.
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
(\\sum_{k=0}^{\\infty}a_k) \\cdot (\\sum_{k=0}^{\\infty}b_k) = \\sum_{k=0}^{\\infty}c_k = \\sum_{k=0}^{\\infty}\\sum_{j=0}^{k}a_{k}b_{k-j}
")
^{::clerk/visibility {:code :show :result :hide}}
(defn sum-of-series [series]
  (apply + series))
^{::clerk/visibility {:code :show :result :hide}}
(defn partial-sum-of-series [series n]
  (apply + (take n series)))
^{::clerk/visibility {:code :show :result :hide}}
(defn multiply-series [s1 s2]
  (let [f (fn [n]
            (let [t1 (take n s1)
                  t2 (reverse (take n s2))]
              (sum-of-series (map * t1 t2))))]
    (map (comp f inc) (range))))
(render-series (multiply-series (range) (range)))

;; Here the convergence of the partial sums is not as simple to establish
;; as for addition. If both series are **absolutely convergent** then
;; the resulting series also converges absolutely with a sum equal to
;; the product of the two sums of the multiplied series:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\lim_{n \\to \\infty}s_{c,n} = (\\lim_{n \\to \\infty}s_{a,n}) \\cdot (\\lim_{n \\to \\infty}s_{b,n})
")

;; ## Types of Series
;; ### Numeric Series Examples
;; #### Geometric series
;; Each successive term is formed by multiplying the previous by a fixed
;; number (called the common ratio).

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=0}^{\\infty}ar^n
")
;; E.g.
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
1 + \\frac{1}{2} + \\frac{1}{4} + \\frac{1}{8} + ... = \\sum_{n=0}^{\\infty}\\frac{1}{2^n} = 2
")

;; In general for a geometric series with initial term $a$ and common
;; ratio $r$, the series is convergent if and only if $|r|<1$, in which
;; case it converges to $\frac{a}{1-r}$.

;; #### Harmonic series
;; The harmonic series is the following series and is divergent.
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
1 + \\frac{1}{2} + \\frac{1}{3} + \\frac{1}{4} + ... = \\sum_{n=0}^{\\infty}\\frac{1}{n}
")

;; #### Alternating series
;; Alternating series by definition have terms with alternating signs.
;; For example the alternating harmonic series:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
1 - \\frac{1}{2} + \\frac{1}{3} - \\frac{1}{4} + ... = \\sum_{n=1}^{\\infty}\\frac{(-1)^{n-1}}{n} = \\ln(2)
")
;; or the Liebniz formula for $\pi$:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
-1 + \\frac{1}{3} - \\frac{1}{5} + \\frac{1}{7} - ... = \\sum_{n=1}^{\\infty}\\frac{(-1)^n}{2n-1} = \\frac{\\pi}{4}
")

;; #### Pi
;; An example of a series representation of $\pi$:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{1}{n^2} = \\frac{1}{1^2} + \\frac{1}{2^2} + \\frac{1}{3^2} + ... = \\frac{\\pi^2}{6}
")

;; #### Natural logarithm of 2
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{(-1)^{n+1}}{n} =  \\ln(2)
")

;; #### Natual logarithm base _e_
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{1}{n!} = \\frac{1}{0!} + \\frac{1}{1!} + \\frac{1}{2!} + ... = e
")
