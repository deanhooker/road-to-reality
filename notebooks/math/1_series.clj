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

;; ## Testing for convergence or divergence
;; A series is convergent if $\lim_{n\to\infty}S_n = S$, i.e. if the
;; partial sums $S_n$ of a series tend to a limit $S$.

;; ### Preliminary test
;; If the terms of an infinite series do not tend towards zero then the
;; series diverges, otherwise we must test further.

;; ### Comparison test
;; We can compare the series we are testing to a series we know converges
;; or diverges. For our series $a_1+a_2+a_3+...$ we can compare with known
;; series $m_1+m_2+m_3+...$ (we can ignore the first _n_ terms).

;; If the _m_ series **converges** then the _a_ series is convergent if
;; $|a_n| \le m_n$.

;; Similarly if the _m_ series **diverges** then the _a_ series is divergent
;; if $|a_n| \ge m_n$.

;; ### Integral test
;; If terms are decreasing, $0<a_{n+1}<a_n$, then \sum^{\infty}a_n$
;; converges if $\int^{\infty}a_n dn$ is finite and diverges if the
;; integral is infinite.

;; ### Ratio test
;; Let $\rho_n = |\frac{a_{n+1}}{a_n}|$ and $\rho = \lim_{n\to\infty}\rho_n$.
;; Then

;; The series converges if $\rho < 1$
;;
;; Use a different test if $\rho = 1$
;;
;; The series diverges if $\rho > 1$

;; ### Special comparison test
;; This test has two parts:
;; 1) If $\sum_{n=1}^\infty b_n$ is a convergent series of positive terms
;; and $a_n\ge0$ and $\frac{a_n}{b_n}$ tends to a finite limit then
;; $\sum_{n=1}^\infty a_n$ converges.
;; 2) If $\sum_{n=1}^\infty d_n$ is a divergent series of positive terms
;; and $a_n\ge0$ and $\frac{a_n}{d_n}$ tends to a limit greater than 0
;; (or $+\infty$) then $\sum_{n=1}^\infty a_n$ diverges.

;; ### Testing alternating series
;; An alternating series which is **absolutely convergent** is also
;; convergent. Otherwise we must test further.
;; An alternating series converges if the absolute value of the terms
;; decreases steadily to zero.

;; If an alternating series converges, but is not **absolutely convergent**
;; then it is called **conditionally convergent**.

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

;; ## Numeric Series
;; ### Geometric series
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

;; ### Harmonic series
;; The harmonic series is the following series and is divergent.
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
1 + \\frac{1}{2} + \\frac{1}{3} + \\frac{1}{4} + ... = \\sum_{n=0}^{\\infty}\\frac{1}{n}
")

;; ### Alternating series
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

;; ### Pi
;; An example of a series representation of $\pi$:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{1}{n^2} = \\frac{1}{1^2} + \\frac{1}{2^2} + \\frac{1}{3^2} + ... = \\frac{\\pi^2}{6}
")

;; ### Natural logarithm of 2
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{(-1)^{n+1}}{n} =  \\ln(2)
")

;; ### Natual logarithm base _e_
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=1}^{\\infty}\\frac{1}{n!} = \\frac{1}{0!} + \\frac{1}{1!} + \\frac{1}{2!} + ... = e
")

;; ## Series of functions
;; ### Power series
;; Power series have the form (where _c_ can be 0):

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\sum_{n=0}^{\\infty}a_n(x-c)^n = a_0 + a_1(x-c) + a_2(x-c)^2 + ...
")

;; Since these new series are a function of _x_ it becomes important
;; to find the values for _x_ which converge or diverge: the **interval
;; of convergence**. We often use the (previously mentioned) _ratio test_
;; for this.

;; #### Examples
;; Consider the series

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
1 - \\frac{x}{2} + \\frac{x^2}{4} - \\frac{x^3}{8}+...+\\frac{(-x)^n}{2^n}+...
")

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\rho_n = |\\frac{(-x)^{n-1}}{2^{n+1}} \\div \\frac{(-x)^n}{2^n}|=|\\frac{x}{2}|
")
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\rho = |\\frac{x}{2}|
")

;; the series converges for $\rho<1$ so it converges for $|x|<2$ and
;; diverges for $|x|>2$. We must also consider the boundary points:
;; $x=2$ and $x=-2$. When $x=2$ the series is $1-1+1-1+...$ which is
;; divergent. For $x=-2$ the series is $1+1+1+1...$ which is also divergent.
;; Thus the interval for convergence is $-2<x<2$.

;; #### Thereoms about power series

;; Within the interval of convergence a power series with converge on
;; some value which depends on $x$, i.e. a function of $x$.

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
S(x) = \\sum_{n=0}^{\\infty} a_nx^n
")

;; We can say that a power series (within the interval of convergence)
;; converges to the function $S(x)$ or that function $S(x)$ is represented
;; by the series.

;; 1) A power series can be differentiated or integrated term by term;
;; the resulting series converges to the derivative or integral of the
;; function represented by the series within the same interval of
;; convergence (not necessarily at the endpoints though).

;; 2) Two power series may be added, subtracted or multiplied; the resultant
;; series converges at least in the common interval of convergence. Two
;; series can be divided provided that the denominator is not 0 at
;; $x=0$, or if it is then the zero is canceled by the numerator e.g.
;; $\frac{sin(x)}{x}$. The resulting series has **some** interval of
;; convergence but will have to be calculated (e.g. with the ratio test).

;; 3) One series may be substituted for another provided that the values
;; substituted are in the interval of convergence.

;; 4) The power series of a function is unique, i.e. there is just one
;; power series of the from $\sum_{n=0}^{\infty}a_nx^n$ which converges
;; to a given function.

;; #### Expanding functions in power series

;; We can obtain a power series representation by assuming there is a
;; series of the form $a_0 + a_1x + a_2x^2 + a_3x^3 + ...$ for a function.
;; Repeatedly taking the derivative of each side we can find the values
;; of the coefficients.

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
f(x) = sin(x) = a_0 + a_1x + a_2x^2 + a_3x^3 + ... \\\\
f'(x) = cos(x) = a_1 + 2a_2x + 3a_3x^2 + 4a_4x^3 + ... \\\\
f''(x) = -sin(x) = 2a_2 + 3\\cdot2a_3x + 4\\cdot3a_4x^2 + ... \\\\
f'''(x) = -cos(x) = 3\\cdot2a_3 + 4\\cdot3\\cdot2a_4x + 5\\cdot4\\cdot3x^2 + ...
")

;; At each stage we use $x=0$ which gives 0 for each x term:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
f(0) = sin(0) = 0 = a_0 \\\\
f'(0) = cos(0) = 1 = a_1 \\\\
f''(0) = -sin(0) = 0 = 2a_2 \\\\
f'''(0) = -cos(0) = -1 = 3\\cdot2a_3
")

;; Therefore:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
sin(x) = 1 - \\frac{x^3}{3!} + \\frac{x^5}{5!} - ...
")

;; Series found this way are called _Maclaurin series_ or _Taylor series
;; about the origin_.

;; #### Useful power series to remember
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{3}
sin(x) = & 1 - \\frac{x^3}{3!} + \\frac{x^5}{5!} - \\frac{x^7}{7!} + ...
& \\qquad \\text{all } x \\\\
cos(x) = & 1 - \\frac{x^2}{2!} + \\frac{x^4}{4!} - \\frac{x^6}{6!} + ...
& \\qquad \\text{all } x \\\\
e^x = & 1 + x + \\frac{x^2}{2!} + \\frac{x^3}{3!} + \\frac{x^4}{4!} + ...
& \\qquad \\text{all } x \\\\
ln(1+x) = & x - \\frac{x^2}{2} + \\frac{x^3}{3} - \\frac{x^4}{4} + ...
& \\qquad -1 < x \\le 1 \\\\
(1+x)^p = & x + px + \\frac{p(p-1)}{2!}x^2 + \\frac{p(p-1)(p-2)}{3!}x^3 + ...
& \\qquad |x| < 1 \\\\
\\end{alignedat}
")

;; #### Deriving series for more complicated functions
;; Using power series for functions we already know we can derive the
;; series for more complex functions. E.g. $e^x \cdot cos(x)$:

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{2}
e^x \\cdot cos(x) & = (1+x+\\frac{x^2}{2!}+\\frac{x^3}{3!}...) \\cdot
(1-\\frac{x^2}{2!}+\\frac{x^4}{4!}...) \\\\
& = (1 + x + \\frac{x^2}{2!} + \\frac{x^3}{3!} + \\frac{4^3}{4!} ...) \\\\
& \\quad +  (0 + 0 - \\frac{x^2}{2!} - \\frac{x^3}{2!} - \\frac{x^4}{4!} ...) \\\\
& \\quad +  (0 + 0 + 0 + 0 + \\frac{x^4}{4!} ...) \\\\
& = 1 + x - \\frac{x^3}{3} - \\frac{x^4}{6} ...

\\end{alignedat}
")

;; ## Practical Uses
;; ### Numerical Computation
;; It can be easier to use series for problems which cannot be easily
;; solved with a calculator.

;; #### Example 1
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{2}
ln(\\sqrt{\\frac{1+x}{1-x}}) - tan(x) \\biggr\\rvert_{x=0.0015} & =
(x + \\frac{x^3}{3} + \\frac{x^5}{5}  ...) -
(x + \\frac{x^3}{3} + \\frac{2x^5}{15}  ...)
 \\biggr\\rvert_{x=0.0015} \\\\
& = \\frac{x^5}{15} + \\frac{4x^7}{45}  \\biggr\\rvert_{x=0.0015} \\\\
& = 5.06\\cdot10^{-16}
\\end{alignedat}
")
;; with an error of the order of $x^7$ or $10^{-21}$. Both numbers being
;; subtracted are approximately 0.0015 and do not differ until the 16th
;; decimal place.

;; #### Example 2
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\frac{d^4}{dx^4}(\\frac{1}{x}sin(x^2))\\biggr\\rvert_{x=0.1}
")
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{2}
\\frac{1}{x}sin(x^2) & = \\frac{1}{x}(x^2-\\frac{x^6}{3!}+
\\frac{x^10}{5!})\\biggr\\rvert_{x=0.1} \\\\
& = x-\\frac{x^5}{3!}+\\frac{x^9}{5!}\\biggr\\rvert_{x=0.1} \\\\
\\end{alignedat}
")

;; Differentiate x4:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
-\\frac{5\\cdot4\\cdot3\\cdot2x}{3!}+\\frac{9\\cdot8\\cdot7\\cdot6x^5}{5!}
\\biggr\\rvert_{x=0.1} = -2+0.00025
")
;; or $\approx-2$ with an error of order $10^{-4}$.


;; ### Summing Series
;; If you recognize a numerical series as the series for a particular
;; function at a particular value:

;; #### Example
;; Find the sum $1-\frac{1}{2} + \frac{1}{3} - \frac{1}{4}...$.
;;
;; We are familiar with $ln(1+x) = x - \frac{x^2}{2} + \frac{x^3}{3} - \frac{x^4}{4}...$.
;; If we put $x=1$ then we get:
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
ln(2) = 1 - \\frac{1}{2} + \\frac{1}{3} - \\frac{1}{4} ...
")

;; ### Evaluation of Definite Integrals
;; Many integrals found in applied problems cannot be evaluated in terms
;; of elementary functions. One way to find the definite integral when
;; the indefinite cannot be found is to expand the integral in a power
;; series and integrate term-by-term.

;; #### Example
^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{2}
\\int^1_0 sin(x^2)dx & = \\int^1_0 (x^2 - \\frac{x^6}{3!} + \\frac{x^10}{5!}...) dx\\\\
& = \\frac{1}{3} - \\frac{1}{7\\cdot3!} + \\frac{1}{11\\cdot5!}...  \\biggr\\rvert_0^1 \\\\
& = 0.33333 - 0.02381 + 0.00076...
& \\approx 0.3102
\\end{alignedat}
")
;; with an error $<\frac{1}{15\cdot7!}$ or about $10^{-5}$.

;; ### Evaluation of Indeterminate Forms
;; Suppose we want to find $\lim_{x \to\infty} \frac{1-e^x}{x}$.
;; If we try to substitute $x=0$ we get $\frac{0}{0}$. Expressions that
;; lead to such meaningless results are called _indeterminate forms_.
;; Many times they can be evaluated using series.

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\begin{alignedat}{2}
\\lim_{x\\to\\infty}\\frac{1-e^x}{x} & = \\lim_{x\\to\\infty}
(\\frac{1-(1+x+\\frac{x^2}{2!}+...)}{x})\\\\
& = \\lim_{x\\to\\infty}(-1-\\frac{x}{2!}-\\frac{x^2}{3!}...)\\\\
& = -1
\\end{alignedat}
")

;; Examples of indeterminate forms are: $\frac{0}{0}$, $\frac{\infty}{\infty}$
;; and $0\cdot\infty$. For a given problem you need to decide between
;; differentiating and using L'Hôpital's rule (e.g. when we do not know
;; the series expansion for either $f$ or $\phi$):

^{::clerk/visibility {:code :hide :result :show}}
(clerk/tex "
\\lim_{x\\to a}\\frac{f(x)}{\\phi(x)} = \\lim_{x\\to a}\\frac{f'(x)}{\\phi'(x)}
")

;; or using series (for example when the derivatives are too complicated.
;;
;; Series are most useful for the $\frac{0}{0}$ form with $\lim_{x\to0}$
;; since the series collapses to the constant term.

;; ## Useful Facts
;; 1) The convergence or divergence of a series is not affected by
;; multiplying each term by the same constant. Nor is it affected by
;; changing a finite number of terms (e.g. removing the first n terms).

;; 2) Two convergent series may be added or subtracted term by term.
;; The resulting series is convergent and its sum is the addition or
;; subtraction of the sums of the original series.

;; 3) The terms of an **absolutely convergent series** may be arranged
;; in any order without affecting the convergence or the sum. This is
;; **not true** of **conditionally convergent** series.
