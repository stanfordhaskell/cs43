---
title: Immutability and recursion
published: 2000-01-03
---

The following code does not compile.

```haskell
x = 1
x = x + 1
```

In Haskell, variables are 

