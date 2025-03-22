---
title: "On continuations"
date: 2025-03-22T15:51:27-07:00
---

A short piece for teaching continuations, in the Platonic dialectic style. Whether it is helpful is for you to decide. Cheers to burritos![^refs]

---

**Theofanis:** Are you free right now, Asimoula?

**Asimoula:** Well, surely if your matter of attention is short and concise, I shall give mine too regardless; and if it is long, we shall look upon its intriguability, and then making a decision.

**Theofanis:** Well, to the very honest, it goes as such: this "continuation" thing has been perplexing me for days on end. [Some](https://en.wikipedia.org/wiki/Continuation) state it very short simple, "a stored and resuamble state of computation", but do not seem all that useful, or hint at why at all someone should use it. Others give very length examples,

```scheme
> (define cont '())
> (+ (/ 12 2)
     (* (+ 1 1)
        (call/cc (lambda (cc) (set! cont cc) 4))))
;; As if the (call/cc) part is just 4, returned by the lambda
14
> (cont 1) 
;; As if we've evaluated the expression *again*, but the (call/cc) is just 1
8
> (cont 2)
10
> (cont 4)
14
```

which is indeed fascinating, though now _how_ it works and _why_ seem to hide themselves even further in the thick fog. [Calling them](http://community.schemewiki.org/?call-with-current-continuation-for-C-programmers) `setjmp`/`longjmp` makes perfect and clear sense, but at the cost of summoning Satanic monster who blasts putrid breathe all over my face[^demonic-breathe]. What's more, everybody seems so inclined to mention this thing called "continuation-passing style", which are mostly followed by some nonsensical rewriting of perfectly fine programs to pass [callbacks](https://stackoverflow.com/a/14022348) everywhere.

**Asimoula:** A length topic indeed. But nonetheless an interesting one, so let us go through it! So I think I'll do this: that example you cited is good enough at demonstrating its power, and so hereby I too will give a short and simple, but foggy and slimy statement: continuations are values (objects, if you like that word) that store a callstack, that you can jump back to while passing in some values.

**Theofanis:** ...sure?

**Asimoula:** We should now then take a detours out of mathematics and theory land, and dive into the dirty waters of real CPUs and implementations. After that, we'll climb on mountain of asynchronous programming, so we can see a different way for using continuations—at the same time so we can look at it from a different, perhaps much higher vantage point. In this way, we'll not be blinded by the great city of Scheme and its fortress of walls, and be able to see the cloud of continuation in its full shape.

But enough babbling about, let's jump in:

As we know, in assembly land, function calls happen through the callstack and a few registers. Inside some place, let's say the function `foo()`, we decide to call another function `bar()`, so we push all the parameters onto the same, and then jump to the first instruction of `bar()`.

```
The Callstack
+----------------+
| <variables>    |                   --+
| <return value> |                     |-- frame for bar()
| foo(): 0xA3    | <- return address --+
+----------------+
| <variables>    |                   --+
| <return value> |                     |-- frame for foo()
| main(): 0x1E   | <- return address --+
+----------------+
|       .        |
|       .        |
|       .        |
```

**Theofanis:** Yes, that seems to make sense, now that you talk about. I do seem to recollect about this.
_hesitating, for he being a JavaScript programmer by trade doesn't quite have the C model on top of the head_

**Asimoula:** And so as you see, when we return from `bar()`, for hypothetically, we place the return value at a predetermined location, and read the "return address" from another predetermined location, and `jmp` to it. The frame of `bar()` is now free reign for anybody else to write on top of.[^calling-convention] In particular that return address points to a special chunk inside the assembly of `foo()`, that takes care of things after `call`ing `bar()`. But the details are unimportant for us right now.

**Theofanis:** Right.

**Asimoula:** Now if you take your hand, and cover up the part of the stack for variables of `bar()` and its babbage, and squint your eyes a little bit, so might realize a magical thing that seems to be happening here: for all we know, Deina who works on `bar()` has just wrote a few bytes into `<return value>` and `jmp`ed to another address at `[rsp+4]`, and he seem to have magically teleported to a place where the blinking lights and whistling crowd resumes into motion, in middle of `foo()`.

**Theofanis:** That does seem sort of magical, if you put such metaphors on top of it.

**Asimoula:** Right— all I'm really saying is, by doing these two things—reading and writing a few values to a fixed location (relative to the stack pointer, of course), and `jmp`ing, we can in essence resume the state of execution to an arbitrary place in time.

**Theofanis:** That is right, but do not see how it is useful. How does that make function calls special?

**Asimoula:** This is the crucial behavior of continuations that make them both theoretically _and_ practically powerful. They are a single vocabulary to describe function calls (replacing the stack upwards[^stack-dir]), exceptions (replacing the stack downwards to the handler), generators (restoring the stack to inside the generator function, and then restoring the stack to the caller), coroutines (same as generators), or even goto! goto within a function is just replacing the stack to _some state_ of the  same function associated with some line number. So many more, countless uses.

Now, what I just described in its full glory is not without simplifications compared to the machinery which Scheme offers. Yet nor is Scheme `call/cc` the true continuation. It is merely one physical incarnation of the concept. It is but rather the law of universal gravitation incarnation to the much more fundamental pattern that is inverse squares in nature.

If you do intend to learn to wield Scheme's full power, sit down, grab a cup of tea (or coffee) and learn from the docs and examples. And documentation. And actual code. But... I digress.

Of course, _power_ does not equate to _usefulness_, and as you can see: it's a heavyweight vocabulary to be wielding around. In fact, it's so powerful that most people find it too flexible.

**Theofanis:** Now I see why these people are excited over it. For sure this demonstrates their power beautifully, Asimoula, but now can you also tell me what does these lovely continuation have to do with the "continuation-passing style"?

**Asimoula:** Look at this ordinary program:

```scheme
(define (add a b)
  (+ a b))

;; evaluate `add` first, then evaluate `display`
(let ((res (add 40 2)))
  (display res))
;; => prints 42
```

compare that with:

```scheme
(define (add a b cont)
  (cont (+ a b)))

;; `cont` is the thing (i.e. `display) we want to eval after `(add 40 2)`
(let (cont (lambda (res)
             (display res)))
  (add 40 2 cont))
;; => also prints 42
```

compare that with:

```scheme
;; same `add` definition
(define (add a b cont)
  (cont (+ a b)))

;; Similarly, right outside of `call/cc` is the thing we want to eval after `(add 40 2)`
(print (call/cc (add 40 2)))
```

Both latter examples are using continuations. Both of them do functions by using CPS. I shall reiterate: `call/cc` is but a specific incarnation of the general idea of continuations.

To that idea, _CPS is a property that some programs have. CPS can be obtained either by voluntarily writing it like so, or by applying a mechanical transformation to a regular program._ Why have programs in CPS? Loosely, it's easier to write algorithms transforming them in useful ways, i.e. optimizations.

**Theofanis:** I see. So `call/cc` is to continuations as Java `@FunctionInterface` is to first-class functions. Or as C++ templates[^ad-hoc-poly] is to parametric polymorphism.

**Asimoula:** You got it exactly right, my friend.

As some bonus chatter, an naive `call/cc` machinery by stack copying can be quite slow. It would literally copy the entire execution stack, megabytes of data, to the heap, and replace it when reentering the continuation.
Non-naive implementations like [Chicken](https://www.more-magic.net/posts/internals-gc.html) exists, by rewriting the entire program to be CPS. This way, `call/cc` is free because literally everything is already a continuation. But this comes at the tradeoff that everything, even the code that does not _explicitly use_ continuations, are just a little bit slower.


An naive `call/cc` machinery can be quite slow, by literally writing megabytes of data to replace the stack. Non-naive implementations like [Chicken](https://www.more-magic.net/posts/internals-gc.html) exists, but with tradeoffs like making everything, even the code that does not _explicitly use_ continuations just a little bit slower.



[^refs]: If you, the reader, is really coming here confused, continuing may cause you to get more confused.
The author is in fact aware of the [burrito monads](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/) fallacy, and has no intention to stop. The author just found writing this amusing.
Read on at your own risk (or benefit).

[^demonic-breathe]: What I mean by this is `setjmp`/`longjmp` traditionally have a bad reputation among programmers, that it allows non-local control flow ("goto considered harmful"). 
Also this metaphor isn't all that helpful to showing what continuations are *supposed to* enable; C programs really don't use `setjmp`/`longjmp` similarly to continuations at all. I guess a little bit?

[^calling-convention]: These are all made up for the convenience for the demonstration. I don't remember if any real calling convetions work in this exact way, but even if they do, things like parameters, register spillage, and stack pointer handling are omitted here. Don't take it too seriously.

[^stack-dir]: Usually, the callstack is said to grow downwards (lower address means deeper callstack), based on that most calling conventions do it this way. I am going to call it "upwards" because it probably makes more sense to more people.

[^ad-hoc-poly]: Ignoring the template specializations part. I know that's ad-hoc polymorphism, but that's not the point.
