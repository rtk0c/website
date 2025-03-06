---
title: "Tips and tricks: CS 166 Information Security taught by Mark Stamp"
date: 2025-03-04T12:14:54-08:00
tags: ["SJSU"]
---

# Chapter 5
## Problem 4 - hash collision
The expected answer is the square root approximation that’s much glossed over in the textbook, and seen here:
https://en.m.wikipedia.org/wiki/Birthday_problem#Square_approximation

But, for the sake for information, I did end up in quite a rabbit hole trying to find the exact solution. These maybe interesting reads:
- https://math.stackexchange.com/questions/407307/second-pair-of-matching-birthdays
- https://math.stackexchange.com/questions/2313215/birthday-paradox-2-pairs (same thing, just less general and less verbose)
- https://math.stackexchange.com/questions/1539271/probability-of-exactly-two-pairs-share-a-birthday-and-each-pair-shares-differen

## Problem 24 - MD5 collision
### I'm getting different hashes
The messages are supposed to be binary files, but the textbook gave them in hex codes.
You probably need something like `xxd -r -p`, or [powershell](https://stackoverflow.com/a/64927815), or your hex editor of choice, to turn it into a binary message.

Something like these will work:
```sh
$ cut -d ' ' -f 2- <<'EOF' | xxd -r -p > msg1.bin
00000000 d1 31 dd 02 c5 e6 ee c4  69 3d 9a 06 98 af f9 5c
00000010 2f ca b5 87 12 46 7e ab  40 04 58 3e b8 fb 7f 89
00000020 55 ad 34 06 09 f4 b3 02  83 e4 88 83 25 71 41 5a
00000030 08 51 25 e8 f7 cd c9 9f  d9 1d bd f2 80 37 3c 5b
00000040 96 0b 1d d1 dc 41 7b 9c  e4 d8 97 f4 5a 65 55 d5
00000050 35 73 9a c7 f0 eb fd 0c  30 29 f1 66 d1 09 b1 8f
00000060 75 27 7f 79 30 d5 5c eb  22 e8 ad ba 79 cc 15 5c
00000070 ed 74 cb dd 5f c5 d3 6d  b1 9b 0a d8 35 cc a7 e3
EOF
$ cut -d ' ' -f 2- <<'EOF' | xxd -r -p > msg2.bin
00000000 d1 31 dd 02 c5 e6 ee c4  69 3d 9a 06 98 af f9 5c
00000010 2f ca b5 07 12 46 7e ab  40 04 58 3e b8 fb 7f 89
00000020 55 ad 34 06 09 f4 b3 02  83 e4 88 83 25 f1 41 5a
00000030 08 51 25 e8 f7 cd c9 9f  d9 1d bd 72 80 37 3c 5b
00000040 96 0b 1d d1 dc 41 7b 9c  e4 d8 97 f4 5a 65 55 d5
00000050 35 73 9a 47 f0 eb fd 0c  30 29 f1 66 d1 09 b1 8f
00000060 75 27 7f 79 30 d5 5c eb  22 e8 ad ba 79 4c 15 5c
00000070 ed 74 cb dd 5f c5 d3 6d  b1 9b 0a 58 35 cc a7 e3
EOF
$ md5sum msg1.bin msg2.bin
```

FYI, `cut` is used to strip the address column from the string.

## Problem 39 - stenography
### Getting a blank PDF on windows
The given `stegoRead.c` and `stego.c` is using `fopen(2)` in text mode, and CRT on Windows _may_ expand byte sequence 0A (\n) to 0D 0A (\r\n). I'm honestly not sure when it decides to do that.

Add `b` to the mode of all instances of `fopen(...)`. For example, change `fopen(outfname, "w")` into `fopen(outfname, "wb")`
