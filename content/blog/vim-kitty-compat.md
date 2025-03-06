---
title: "Vim really does not like Kitty"
date: 2025-03-05T20:30:01-08:00
ShowToc: false
---

**TL;DR**: Kitty uses a custom terminfo `xterm-kitty`. Vim doesn't like it.
If you're in a pinch, commit a [crime](#i-just-want-it-work-right-now) and hopefully it works fine.
If you're not, switch to another terminal for vim, or switch to neovim, or [attempt to configure vim](#teach-vim-to-speak-kitty).

---

If you use Vim in Kitty, local machine or going through SSH, and (at least) one of these is happening:

1. Paste <kbd>Ctrl+Shift+V</kbd> from system clipboard is egregiously slow. Like two lines per second slow[^arch].
2. Paste is glitchy. All the whitespaces get eaten, nowhere to be seen. Lines get jumbled together, parts of the clipboard overwrite another, etc[^debian].
3. Kitty tells you your clipboard contains terminal escape sequences. Except it absolutely does not. Pasting elsewhere, still in Kitty, like into `bash` or `nvim` works completely fine[^debian].

[^arch]: happened on a local ArchLinux machine
[^debian]: happened on SSH to an Debian Bookworm arm64 supplied by AWS EC2

you have just discovered that Vim isn't very compatible with Kitty as a terminal emulator. Instead of trying to poorly summarize _why_, you can instead read the problem being extensively discussed _con fuoco_ in the Vim issue tracker, <https://github.com/vim/vim/issues/11729>.

This problem is actually documented. On the Vim side, it's here without any mention of how to make it work: <https://vimhelp.org/term.txt.html#xterm-kitty>.

# Teach Vim to speak Kitty
On the Kitty side, the FAQ mentions it, and also supplies some stuff to put in your `.vimrc` to make Vim happy: <https://sw.kovidgoyal.net/kitty/faq/#using-a-color-theme-with-a-background-color-does-not-work-well-in-vim>.
I have not personally tried any of these, and don't understand a single thing about what they're doing. Just for your reference.

# I just want it work. Right now.
Try `TERM=xterm-256color vim /my/file.txt`.

It seems like by setting `TERM` to `xterm-256color`, Vim will understand at least the paste part mostly fine. I don't know if bracketed paste is happening.
**This is highly discouraged** per Kitty's [documentation](https://sw.kovidgoyal.net/kitty/faq/#i-get-errors-about-the-terminal-being-unknown-or-opening-the-terminal-failing-or-functional-keys-like-arrow-keys-don-t-work) and [opinion](https://github.com/kovidgoyal/kitty/issues/2192).
Don't use it long term, or you'll find other inexplicable weirdness (which I had, but it's all a blurry mess so I can't recite to you the war story. alas).

# Other solutions
As far as I know, all other terminals pretend to be xterm well enough that Vim plays happily long. So you use them instead.

Neovim doesn't seem to have any issue with Kitty.
I prefer Neovim, and got in a habit to go out of my way to replace the default Vim on Debian, Fedora, etc., in part because of this paste weirdness.
Recently I got lazy and just went with Vim, and this issue resurfaced, and I finally decided to research it in depth.
I couldn't find any internet echos regarding this issue apart from the ones I linked here. Maybe my searching skills have deteriorated. Maybe search engines are not as good anymore. In any case, hopefully writing this adds another data point.

# `WARNING: terminal is not fully functional`
All of the above, especially the Neovim part, is assuming the `/usr/share/terminfo/x/xterm-kitty` file is present.
Installing Kitty the terminal automatically installs it.
But that's probably not the case for any machine you SSH into.
You can get it separately, probably packaged as `kitty-terminfo` in your distro.

This is only marginally relevant to the Vim and Kitty story, but since it seems vaguely related, I'm leaving it here, just in case.
