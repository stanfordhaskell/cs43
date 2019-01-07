**CS43: Functional Programming Abstractions**

_Lecture 1: Introduction to Haskell_

Adithya Ganesh and Isaac Scheinfeld

---

**Why Haskell?**

* Functionality <!-- .element: class="fragment" -->
* Expressivity <!-- .element: class="fragment" -->
* Immutability <!-- .element: class="fragment" -->
* Safety <!-- .element: class="fragment" -->
* Orthogonality <!-- .element: class="fragment" -->

 
---

**Course logistics**

* Lectures: Mon/Wed 4:30pm - 5:50pm.  Building 200, Room 030. 
* Office hours by appointment.
* Prerequisites: "Programming / mathematical maturity."  CS107 and CS103 should suffice.

---

**Course expectations**

To pass the class (C/NC), we expect:

* Attendance in $ \geq $ 80% of lectures. <!-- .element: class="fragment" -->

* Successful completion of $ \geq 6/8$ assignments, which should take no more than an hour each.  <!-- .element: class="fragment" -->

---

**Open source**

* All course materials will be on Gitlab: https://gitlab.com/stanford-lambda/stanford-lambda.gitlab.io

* We welcome PRs / contributions to the course repo, and will offer extra-credit on a case-by-case basis.

* Instead of Piazza, we will use Gitlab issues.

---

**Resources**

* No formal textbook, but we recommend:

* Haskell wikibook: https://en.wikibooks.org/wiki/Haskell

* _Real World Haskell_: http://book.realworldhaskell.org/

* Miscellanous articles, tutorials, and research papers, which we will share as the course goes along.

---

**A brief history**

---

**The lambda calculus**

* The lambda calculus defines a way to express the behavior of functions (Alonzo Church, 1930s). 
<!-- .element: class="fragment" -->


* For example, a function that increments its argument by one can be defined by  
<!-- .element: class="fragment" -->


$$ f = \lambda \, x \, . \, x + 1 $$
<!-- .element: class="fragment" -->
 

* _Church's Thesis._ Effectively computable functions from positive integers to positive integers are just those definable in the lambda calculus.
<!-- .element: class="fragment" -->

* In 1937, Turing proved that Turing computability is equivalent to lambda-definability.
<!-- .element: class="fragment" -->



---

**LISP**

* In 1950s, John McCarthy created LISP, which uses Church's lambda notation for functions.
<!-- .element: class="fragment" -->

* In its time, LISP was used as a cutting-edge programming language for AI.
<!-- .element: class="fragment" -->

* John McCarthy helped found the Stanford AI lab, and served as director from 1965 to 1980.
<!-- .element: class="fragment" -->

* LISP lives on today, and is still widely used in many dialects (e.g. Scheme, Clojure).
<!-- .element: class="fragment" -->

* Paper: Recursive functions of symbolic expressions and their computation by machine; Communications of the ACM, 1960.
<!-- .element: class="fragment" -->

---

Haskell

(Image)

* 1978, Backus' Turing Award lecture. 
<!-- .element: class="fragment" -->

* In 1987, a meeting was held at conference on Functional Programming Languages and Computer Architecture in Portland Oregon.
<!-- .element: class="fragment" -->

* First version of Haskell: 1990.
<!-- .element: class="fragment" -->

* Widely used in academia and industry.
<!-- .element: class="fragment" -->


---

# Further reading

* History of FP: http://www.cse.psu.edu/~gxt29//historyOfFP/historyOfFP.html#turing37
<!-- .element: class="fragment" -->


Ref: A history of Haskell, Being Lazy with Class
http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf

## Projects