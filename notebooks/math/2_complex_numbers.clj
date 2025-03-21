;; # Notes on Complex Numbers
^{:nextjournal.clerk/toc true}
(ns notebooks.math.2-complex-numbers
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

;; In order to talk about the square root of negative numbers we
;; introduce imaginary numbers. An imaginary number is made up of a
;; coefficient of $i$, which is a real number, and $i$, where
;; $i=\sqrt{-1}$ and $i^2=-1$.

;; Complex numbers are numbers made up of a real part and an imaginary
;; part. $5+3i$ is an example of a complex number. Interestingly it is
;; really made up of two real numbers: 5 and 3, suggesting a geometrical
;; representation: a real axis and an imaginary axis.

;; Using the complex plane we can represent complex numbers in rectangular
;; form (x, y)

;; TODO: graphic representation of the number using x and y

;; or its polar coordinates

;; TODO: graphic representation of the number using r and theta

;; ## Terminology
;; For a complex number $z=(x,y)=(r,\theta)$
;; - $x =$ real part
;; - $y =$ imaginary part
;; - $r = |z| = \sqrt{x^2+y^2}$
;; - $\theta =$ angle / phase / argument / amplitude

;; ### Angle
;; The angle of a complex number can have integer $n$ multiple of $2\pi$
;; added and it will result in the same number, thus there are an infinite
;; number of angles per complex number.

;; ### Complex Conjugate
;; The complex number $x-iy$ obtained by changing the sign of $i$ in
;; $z=x+iy$ is called the **complex conjugate** of $z$.

;; TODO: Show graphical representation of complex conjugate.
