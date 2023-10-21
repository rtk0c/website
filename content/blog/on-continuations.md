---
title: "On Continuations"
date: 2023-10-21T12:14:54-07:00
draft: true
---

**Theofanis:** Are you free right now, Asimoula?

**Asimoula:** Well, surely if your matter of attention is short and concise, I shall give mine too regardless; and if it is long, we shall look upon its intriguability, and then making a decision.

**Theofanis:** Well, to the very honest, it goes as such: this "continuation" thing has been perplexing me for days on end. [Some](https://en.wikipedia.org/wiki/Continuation)[^refs] state it very short simple, "a stored and resuamble state of computation", but do not seem all that useful, or hint at why at all someone should use it. Others give very length examples,

```scheme
> (define cont '())
> (+ 5 (* 12 (- 3 (* 3 7)) (call/cc (lambda (cc) (set! cont cc) 4))))
-859
> (cont 1)
-211
> (cont 2)
-427
> (cont 4)
-859
```

which is indeed fascinating, though now _how_ it works and _why_ seem to hide themselves even further in the thick fog. [Calling them](http://community.schemewiki.org/?call-with-current-continuation-for-C-programmers) `setjmp`/`longjmp` makes perfect and clear sense, but at the cost of summoning a satan while blasting its putruid demonic breathe all over your face[^demonic-breathe]. What's more, everybody seems so inclined to mention this thing called "Continuation-Passing Style", which are mostly followed by some nonsensical rewriting of perfectly fine programs to pass [callbacks](https://stackoverflow.com/a/14022348) everywhere.

**Asimoula:** A length topic indeed. But nonetheless an interesting one, so let us go through it! So I think I'll do this: that example you cited is good enough at demonstrating its power, and so hereby I too will give a short and simple, but foggy and slimey statement: continuations are values (objects, if you like that word) that store a callstack, that you can jump back to while passing in some values.

**Theofanis:** ...sure?

**Asimoula:** We should now then take a detours out of mathematics and theory land, and dive into the dirty waters of real CPUs and implementations. After that, we'll climb on mountain of asynchronous programming, so we can see a different way for using continuations--at the same time so we can look at it from a different, perhaps much higher vantage point. In this way, we'll not be blinded by the great city of Scheme and its fortress of walls, and be able to see the cloud of continuation in its full shape.

But enough babbling about, let's jump in:

As we know, in assembly land (so that means C and its ancestors and descendents), function calls happen through the callstack and a few registers. Inside some place, let's say the function `foo()`, we decide to call another function `bar()`, so we push all the parameters onto the same, and then jump to the first instruction of `bar()`.

<!-- TODO diagram -->


[^refs]: If you, the reader, is really coming here confused, reading these existing tutorials is probably just going to make you more confused. Do it at your own risk (or benefit).

[^demonic-breathe]: What I mean by this is `setjmp`/`longjmp` traditionally have a bad reputation among programmers, that it allows non-local control flow ("goto considered harmful"), and also
