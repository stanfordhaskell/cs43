---
title: Why Functional Programming?
published: 2000-01-01
---

This is an example of an embedded repl. Notice that running the code doesn't
work; you need to add an empty main function for it to compile. You could
probably fix this my making it a module; but that also adds bloat. This could
still work for a limited number of code examples, but definitely not for all.

```haskell
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs
```

<figure class="repl-wrapper" style="height:30rem;">
<iframe src="https://repl.it/@cs43/QuickSortRepl?lite=true&outputonly=1" scrolling="no" frameborder="no" allowtransparency="true" allowfullscreen="true" sandbox="allow-forms allow-pointer-lock allow-popups allow-same-origin allow-scripts allow-modals"></iframe>
</figure>

## Functionality

Lorem ipsum dolor sit amet, nec munere legendos te. Sea putant appellantur eu, in prima adhuc sed, labitur dissentiet vis ea. Accusata splendide mei id, his et eruditi temporibus, est novum iudico ei. Eu liber nonumes cotidieque has, detracto perfecto per ne, ut eius aperiri eripuit his. In aliquid utroque duo, simul percipitur cum no, tota facete assueverit nec ex.

## Elegance

Lorem ipsum dolor sit amet, nec munere legendos te. Sea putant appellantur eu, in prima adhuc sed, labitur dissentiet vis ea. Accusata splendide mei id, his et eruditi temporibus, est novum iudico ei. Eu liber nonumes cotidieque has, detracto perfecto per ne, ut eius aperiri eripuit his. In aliquid utroque duo, simul percipitur cum no, tota facete assueverit nec ex.

## Immutability

## Concurrency

## Safety

## Orthogonality
