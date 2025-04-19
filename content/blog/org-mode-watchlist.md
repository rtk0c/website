---
title: "Watchlist × Emacs org-mode"
date: 2025-04-19T12:29:00-07:00
---

Watch progress websites exist for [anime](https://myanimelist.net) and [film](https://letterboxd.com). They work great. Socialization is great.

But they don't record the _exact time_ at which I finished each episode. I find such statistics amusing to dig through in some kind of a year-end review. I also found it tremendously helpful to know which episodes were most recently watched, and in what order. Helps with recollecting the context of each show, especially when chasing more than a couple of shows at the same time.
Depending on your judgment, using Letterboxd and MyAnimeList may also constitute giving private information to 3rd parties.

So I ended with an organization system that's, effectively, top layer of bullet points for the show, and inner layer for the episodes. Something like:

```org
* STRT SHIROBAKO
** [2025-04-01 Tue 11:11] E01
** [2025-04-16 Wed 22:22] E02
* TODO Do It Yourself!!
* DONE ゆるキャンプ
** [2025-03-20 Thu 20:21] E10
** [2025-03-21 Fri 20:59] E11
** [2025-04-14 Mon 21:36] E12
* STRT mono
** [2025-04-13 Sun 16:56] E01
```

I want to be able to run a command, and get it as a linear timeline:

```org
* [2025-03-20 Thu 20:21] ゆるキャンプ E10
* [2025-03-21 Fri 20:59] ゆるキャンプ E11
* [2025-04-01 Tue 11:11] SHIROBAKO E01
* [2025-04-13 Sun 16:56] mono E01
* [2025-04-14 Mon 21:36] ゆるキャンプ E12
* [2025-04-16 Wed 22:22] SHIROBAKO E02
```

Which turns out to be a pretty easy simple regex-based forward parsing algorithm. I thought about using `org-element-map`, but it seemed like a bit of an overkill?

To use, I do <kbd>g g M-x org-category->timeline</kbd> (evidently, I use evil-mode), bringing me to a new buffer `*Category Timeline*` filled out appropriately.

```emacs-lisp
(defun org-category->timeline ()
  "Collect category-timestamp tree into a single, chronologically
ordered timestamp list. Starting at point."
  (interactive)
  (let ((pt (point))
        (curr-category nil)
        (curr-pt (search-forward "\n* " nil t))
        (items '()))
    ;; Collect items
    (while curr-pt
      (goto-char curr-pt)
      ;; Skip org-todo marker
      (forward-word) (forward-char)
      (setq curr-category (buffer-substring-no-properties (point) (line-end-position))
            curr-pt (save-excursion (search-forward "\n* " nil t)))
      (while-let ((i (re-search-forward (rx "\n** ["
                                            (group (* (not "]")))
                                            "] ")
                                        curr-pt t)))
        (let ((time (match-string-no-properties 1))
              (comment (buffer-substring-no-properties (point) (line-end-position))))
          (push (list (encode-time (org-parse-time-string time))
                      time curr-category comment)
                items))))
    ;; Sort by time
    (sort items (lambda (a b) (time-less-p (car a) (car b))))
    ;; Restore to starting point in the source buffer
    (goto-char pt)
    ;; Print items into a new buffer
    (switch-to-buffer "*Category Timeline*")
    (erase-buffer)
    (dolist (i items)
      (let ((time (cadr i))
            (category (caddr i))
            (comment (cadddr i)))
        (insert "* [" time "] " category " " comment ?\n)))))
```
