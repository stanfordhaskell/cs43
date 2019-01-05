## CS43: Introduction to Haskell

Adithya Ganesh and Isaac Scheinfeld

---

## A brief history

---

## The lambda calculus

* The lambda calculus defines a way to express the behavior of functions.  Created by Alonzo Church in 1930s.

* For example, a function that increments its argument by one can be defined by

$$ f = \lambda \, x \, . \, x + 1 $$

* [Church's Thesis] Effectively computable functions from positive integers to positive integers are just those definable in the lambda calculus.

* In 1937, Turing proved that Turing computability is equivalent to lambda-definability.

Ref: http://www.cse.psu.edu/~gxt29//historyOfFP/historyOfFP.html#turing37

---

## LISP

* In 1950s, John McCarthy created LISP, which uses Church's lambda notation for functions.

* In its time, LISP was used as a cutting-edge programming language for AI.

* John McCarthy helped found the Stanford AI lab, and served as director from 1965 to 1980.

* LISP lives on today, and is still widely used in many dialects (e.g. Scheme, Clojure).

* Papers: Recursive functions of symbolic expressions and their computation by machine; Communications of the ACM, 1960.

Ref: http://www.cse.psu.edu/~gxt29//historyOfFP/historyOfFP.html#turing37

---

## Haskell

(Image)

* 1978, Backus' Turing Award lecture. 

* In 1987, a meeting was held at conference on Functional Programming Languages and Computer Architecture in Portland Oregon.

* First version of Haskell: 1990.

* Widely used in academia and industry.


Ref: A history of Haskell, Being Lazy with Class
http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf

---

## Why Haskell?

* Functionality
* Expressivity
* Immutability
* Safety
* Orthogonality
 
---

## Course logistics

* Lectures: Mon/Wed 4:30pm - 5:50pm.  Building 200, Room 030.
* Office hours by appointment (for now).
* Prerequisites: "Programming / mathematical maturity."  CS107 and CS103 should suffice.

---

## Course expectations

To pass the class (C/NC), we expect:

* Attendance in $\geq$ 80% of lectures.
* Successful completion of $ \geq 6/8$ assignments, which should take no more than an hour each. 

---

## Open source

We would like to open source the course materials.  We welcome contributions to the course repo, and will offer extra-credit on a case-by-case basis to replace assignments.

---

## Examples in Haskell
