---
title: "Tips and tricks: CS 166 Information Security taught by Mark Stamp"
date: 2025-05-13T15:27:00-07:00
tags: ["SJSU"]
---

A collection of troubleshooting notes, general tips and tricks, or personal thoughts on the CS 166 Information Security class taught by Mark Stamp at SJSU.

Part of this, dealing with specific homework problems, is written with the intention of being a last-resort rescue manual. I only include information you need to get out of potential deep water. No solutions to hard problems. No hand holding, especially no "here is how you solve this problem".

The other part, the tips and thoughts, is just that. I will not stop myself blabbering on for forever.

---

# Recommended Tools

## Hex editor
I'm only listing two of my favorites here.

HxD <https://mh-nexus.de/en/hxd/>

A very light, Windows-only hex editor. I use it on my laptop for quick things. It definitely will suffice for this class.

ImHex <https://imhex.werwolv.net/>

Much more powerful, but heavier. It has builtin support for pattern matching, processing, disassembly, etc. It also just looks _really really nice_, 200% eye candy factor.

## C compiler
Some assignments supply C source code that you'll have to compile. In general, they are not compatible with MSVC (Visual Studio), so special care needs taken on Windows.

- macOS: install Xcode Command Line Tools, which contains an Apple-flavored clang. Alternatively, install either clang or gcc from [Homebrew](https://brew.sh/).
- Liunx: your distro's gcc will do. Or clang if you like that. Honestly if you use Linux why are you even reading this section, go away. :P
- Windows: install some flavor of gcc
  - I highly recommend <https://nuwen.net/mingw.html>
    It’s tiny, just a zip file. Unzip it, you get a `open_distro_window.bat`, which when opened gives you a terminal with everything setup. There is _zero_ room for `PATH` to go wrong.
  - Otherwise, https://www.msys2.org/

{{< details summary="What's MinGW and what does it have to do with msys2 and cygwin" >}}
In short, gcc is to MinGW as Linux is to distros.

gcc is a whole bunch of code that can turn C source code, among other things, into an executable. It's designed to run on various \*nix platforms.

MinGW is a bunch of extra code _on top of gcc_ to make it (1) run on Windows, and (2) produce Windows ("PECOFF") executable.

Both projects are just code, they don't provide downloads ("builds"). For gcc, various Linux distros compile them and ship it as a package.
For MinGW, these projects do the same job:

- https://winlibs.com/
- https://github.com/niXman/mingw-builds-binaries
- https://cygwin.com/ \
  This one tries to emulate the *nix environment on Windows. Comes with quite a few extra programs like bash. _See google._
- https://www.msys2.org/ \
  This one builds a whole bunch of softwares in addition to MinGW: bash, make, etc. It's quite complicated, due to those software requring compatibility layers like cygwin to function. I won't explain here. _See google._
- etc. 

Regardless of which one you download, you get a copy of gcc (and MinGW). The difference is the default configs, and the extra software they ship with gcc.

In fact, you'll hear people call these things "MinGW distros".
The MinGW project has a [list of notable distros](https://www.mingw-w64.org/downloads/).

I would have linked something instead of writing this myself, but I literally can't find anything of good quality on the internet...
{{< /details >}}

## x86 Static Analysis

Some assignments ask you to disassemble and understand what a program is doing. For this—as is explained in Chapter 12 of the 3rd ed. textbook—you will need a disassembler that turns the bytes in the executable into assembly code.

I am talking specifically about the offline disassembly and analysis functions. [They also do debugging](#x86-dynamic-analysis).

I recommend **Ghidra** because that's what I use.
Realistically, for the things you'll do in this class, either choice will work just fine.
Similarly, comments below are targeted to use for this class. They're are shallow on purpose.

- [Ghidra](https://ghidra-sre.org). Completely free and open-source. Looks kind of ugly but once you get over that and interaction logic, it's good.

- [ ] [IDA Pro](https://hex-rays.com/ida-pro). Slightly closer eye candy. The built-in pattern matching works slightly better. Lots of people like it, so it (must also be) good. _This costs money_, there is a [free version](https://hex-rays.com/ida-free) but it doesn't come with a decompiler... not good.

  According to legend, alfs who have passed the great challenge will be rewarded a completely legitimate, legal way of using IDA Pro. Let the brave thus sail forth.

- [Binary Ninja](https://binary.ninja). Apparently this is a thing so I'm including it here for completeness, never used, never heard until today, no remarks.

None of these are intuitive, so  please consult the respective _manuals_ and youtube _tutorial videos_ copiously. I shall not provide any guidance here because this blog will turn into the thickness of _Critique of Pure Reason_ or something.

## x86 Dynamic Analysis

Do note that all the tools above can also do debugging. Perfectly capable for the job for this class.

I really like [x64dbg](https://x64dbg.com) (which does both x86\_32 and x86\_64). For reference, the 3rd edition of the textbook recommends [OllyDbg](https://www.ollydbg.de) (32-bit only), but it doesn't receive updates anymore, so it won't be as much of a transferable skill. It'll still work great though.

## Java Decompiler

As of the writing of this blog, I shall claim FernFlower is the best Java decompiler available._<sub>runs<sub><sub>don't slap me don't slap me</sub></sub></sub>_

Admittedly, I'm biased because I worked on Minecraft modding for a while, and Fernflower is what the whole community settled on... anyways!

It's bundled in **IntelliJ IDEA**. You can just open any .class file and it will decompile.

It's also available as a **CLI tool**. If you're going this route, consider using one of the forks that grew from the Minecraft modding efforts, such as [Vineflower](https://vineflower.org). It's not going to matter for whatever you'll be doing in this class, but support their efforts!

# Chapter 5
## Problem 4 - hash collision
Hint: the expected answer _is not_ the exact solution. That's way too complicated

I did end up in quite a rabbit hole trying to find the exact solution. These are not relevant to the textbook at all, but they're still interesting reads:
- https://math.stackexchange.com/questions/407307/second-pair-of-matching-birthdays
- https://math.stackexchange.com/questions/2313215/birthday-paradox-2-pairs (same thing, just less general and less verbose)
- https://math.stackexchange.com/questions/1539271/probability-of-exactly-two-pairs-share-a-birthday-and-each-pair-shares-differen

{{< details summary="More hint" >}}
It's the square root approximation that’s implied, but much glossed over in the textbook. Fuller explaination here:
https://en.m.wikipedia.org/wiki/Birthday_problem#Square_approximation
{{< /details >}}

## Problem 24 - MD5 collision
### I'm getting different hashes
The messages are supposed to be binary files, but the textbook gave them in hex codes.
You probably need something like `xxd -r -p`, or [powershell](https://stackoverflow.com/a/64927815), or your hex editor of choice, to turn it into a binary message.

{{< details >}}
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
{{< /details >}}

## Problem 39 - stenography
### Getting a blank PDF on windows
The given `stegoRead.c` and `stego.c` is using `fopen(2)` in text mode, and CRT on Windows _may_ expand byte sequence 0A (\n) to 0D 0A (\r\n). I'm honestly not sure when it decides to do that.

Add `b` to the mode of all instances of `fopen(...)`. For example, change `fopen(outfname, "w")` into `fopen(outfname, "wb")`

# Chapter 12
Remember to use your favorite search engine to learn. The internet exists for a reason.

## Your OS
All the programs provided here are Windows PECOFF executables.

If you have macOS/Linux, they should all work just fine in Wine (or variants it thereof). You may also grab a Windows VM to run them if Wine doesn't work somehow. Do consider using an under version, e.g. XP or 7 just so it's lighter on the resource usage. (Remember to disconnect internet to the VM if you are using an old Windows!)

## x86 assembly
You'll need a basic understanding of x86 32-bit assembly for this chapter. Very little is required, so whatever you know above another assembly should be transferable.

General tip 0: x86 assembly has 2 syntax flavors, AT&T and Intel. Internet resources may use either, just be ware. GCC and whatnot by default produces AT&T syntax (🤮); if you see lots of % everywhere, or things like `movq` it's this. The textbook and all the reverse engineering tools use Intel syntax (💖 as they should); if you see square brackets `[rip+32h]` or bare `mov`'s, it's this.

General tip 1: `je`/`jz` and `jne`/`jnz` are the same instructions, just different mnemonics. You can always replace either with an unconditional `jmp` in place, they have the same encoding length.

General tip 2: `test reg1,reg2` means taking a bitwise AND, and set zero/carry flags accordingly. `xor reg1,reg1` is a convenient, 2 byte instruction that zeros any register.

General tip 3: almost all the string literals are contained in the `.rdata` section.

General tip 4: `rip` is the instruction pointer. Your debugger probably has a "set `rip` here" function to jump around.

## The program just exits immediately after I type something
Basically, when you double click to open a .exe that's a [Console program](https://stackoverflow.com/questions/574911), Windows only keeps the terminal open for as long as the program is running. Since the program exits right after it prints the last thing, it'll "exit immediately after I type something".

The proper way to do this is open a Cmd or Powershell window, run the .exe from inside like `path/to/my/program.exe`.
This is exactly the same thing as running a command-line program on macOS or Linux: you open Terminal.app, Konsole, Gnome Terminal or whatever, and type `/path/to/my/program`

To save you some head scratching: Note that in Powershell supports `cd D:/path/to/my/folder` directly, but Cmd you have to **type `D:` on it's own to switch drive**, followed by a separate command `cd D:/path/to/my/folder` to change directory in that drive.
