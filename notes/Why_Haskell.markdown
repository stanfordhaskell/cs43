---
title: Why Haskell?
---

In this article, we will distill how functional programming is intellectually
stimulating and practically useful.

Let's start with an example.  Here's an implementation of `quicksort`.  It's a
striking example of Haskell's expressivity and concision.  


In what follows, we'll talk more about the implementation details of Haskell
that enables this.

Feel free to play around with it in the embedded interpreter:

```haskell
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs
```

<figure class="repl-wrapper" style="height:30rem;">
<iframe
src="https://repl.it/@cs43/QuickSortRepl?lite=true&outputonly=1"
scrolling="no" frameborder="no" allowtransparency="true"
allowfullscreen="true" sandbox="allow-forms
allow-pointer-lock allow-popups allow-same-origin
allow-scripts allow-modals"></iframe>
</figure>

## Functionality

- Noun, "The quality of being suited to serve a purpose well; practicality" (source: OED)

- Noun, "Of or relating to functions in computer science or mathematics." (source: us)

Functional programming, pertains to both definitions.  By
programming at a higher level of abstraction, it is often easier to get things
done with less code.

On the other hand, functional programming is fundamentally about programming with functions.


## Expressivity

Haskell enables concise but powerful programs.  Nearly all types can be inferred
by the compiler.  Functional programming emphasizes higher order functions and
writing software at a higher level of abstraction.

Collectively, this makes it possible to express ideas in fewer lines of code.

## Immutability

In Haskell, data is immutable by default.  Pure, functional code is easier to
reason about than stateful code.  Immutability prevents an entire class of bugs
from the get go.

Immutable, functionally pure languages are often well suited to applications of
concurrency.

## Safety

A strong, static type system makes it possible to write safe code for systems
that matter.  In Haskell, GHC compiler identifies bugs before code is even run.
Libraries like [`quickcheck`](https://github.com/nick8325/quickcheck) can even
automatically generate tests to accelerate development time.

## Orthogonality

Haskell, and functional programming more broadly, is uncommon and orthogonal to
the typical norm in software engineering.

We argue that this orthogonality is a feature, not a bug.   For startups, it is
often essential to move faster than industry competitors, and Haskell's unique
tooling allows one to do so.

