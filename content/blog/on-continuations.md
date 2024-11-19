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
> (+ (/ 12 2)
     (* (+ 1 1)
        (call/cc (lambda (cc) (set! cont cc) 4))))
;; As if the (call/cc) part is just the constant 4
;; (which we returned at in the lambda)
14
> (cont 1) 
;; As if we've evaluated the original expression, but with the (call/cc) part
;; replaced with the constant 1
8
> (cont 2)
10
> (cont 4)
14
```

which is indeed fascinating, though now _how_ it works and _why_ seem to hide themselves even further in the thick fog. [Calling them](http://community.schemewiki.org/?call-with-current-continuation-for-C-programmers) `setjmp`/`longjmp` makes perfect and clear sense, but at the cost of summoning a satan while blasting its putruid demonic breathe all over your face[^demonic-breathe]. What's more, everybody seems so inclined to mention this thing called "Continuation-Passing Style", which are mostly followed by some nonsensical rewriting of perfectly fine programs to pass [callbacks](https://stackoverflow.com/a/14022348) everywhere.

**Asimoula:** A length topic indeed. But nonetheless an interesting one, so let us go through it! So I think I'll do this: that example you cited is good enough at demonstrating its power, and so hereby I too will give a short and simple, but foggy and slimey statement: continuations are values (objects, if you like that word) that store a callstack, that you can jump back to while passing in some values.

**Theofanis:** ...sure?

**Asimoula:** We should now then take a detours out of mathematics and theory land, and dive into the dirty waters of real CPUs and implementations. After that, we'll climb on mountain of asynchronous programming, so we can see a different way for using continuations--at the same time so we can look at it from a different, perhaps much higher vantage point. In this way, we'll not be blinded by the great city of Scheme and its fortress of walls, and be able to see the cloud of continuation in its full shape.

But enough babbling about, let's jump in:

As we know, in assembly land, function calls happen through the callstack and a few registers. Inside some place, let's say the function `foo()`, we decide to call another function `bar()`, so we push all the parameters onto the same, and then jump to the first instruction of `bar()`.

```
The Callstack
+----------------+
| <variables>    |                   --+
+----------------+                     |
| <return value> |                     |-- frame for bar()
+----------------+                     |
| foo(): 0xA3    | <- return address --+
+----------------+
| <variables>    |                   --+
+----------------+                     |
| <return value> |                     |-- frame for foo()
+----------------+                     |
| main(): 0x1E   | <- return address --+
+----------------+
|       .        |
|       .        |
|       .        |
```

**Theofanis:** Yes, that seems to make sense, now that you talk about. I do seem to recollect about this.
_hesitating, for he being a JavaScript programmer by trade doesn't quite have the C model on top of his head_

**Asimoula:** And so as you see, when we return from `bar()`, for hypothetically, we place the return value at a predetermined location, and read the "return address" from another predetermined location, and `jmp` to it. The frame of `bar()` is now free reign for anybodty else to write on top of.[^calling-convention] In particular that return address points to a special chunk inside the assembly of `foo()`, that takes care of things after `call`ing `bar()`. But the details are unimportant for us right now.

**Theofanis:** Right.

**Asimoula:** Now if you take your hand, and cover up the part of the stack for variables of `bar()` and its babbage, and squint your eyes a little bit, so might realize a magical thing that seems to be happening here: for all we know, Deina who works on `bar()` has just wrote a few bytes into `<return value>` and `jmp`ed to another address at `[rsp+4]`, and he seem to have magically teleported to a place where the blinking lights and whistling crowd resumes into motion, in middle of `foo()`.

**Theofanis:** That does seem sort of magical, if you put such metaphors and _literacy_ on top of it.

**Asimoula:** Right-- all I'm really saying is, by doing these two things--reading and writing a few values to a fixed location (relative to the stack pointer, of course), and `jmp`ing, we can in essense _resume_ to a place in time.

**Theofanis:** That is right, but do not see how it is useful. How does that make function calls special?

// TODO, make generalization of "what if we can jump to any of the previous functions" and "what if we can jump to some _historical_ functions"


[^refs]: If you, the reader, is really coming here confused, reading these existing tutorials is probably just going to make you more confused. Do it at your own risk (or benefit).

[^demonic-breathe]: What I mean by this is `setjmp`/`longjmp` traditionally have a bad reputation among programmers, that it allows non-local control flow ("goto considered harmful"), and also

[^calling-convention]: These are all made up for the convenience for the demonstration. I don't remember if any real calling convetions work in this exact way, but even if they do, things like parameters, register spillage, and stack pointer handling are omitted here. Don't take it too seriously.
