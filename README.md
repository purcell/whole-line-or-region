[![Melpa Status](http://melpa.org/packages/whole-line-or-region-badge.svg)](http://melpa.org/#/whole-line-or-region)
[![Melpa Stable Status](http://stable.melpa.org/packages/whole-line-or-region-badge.svg)](http://stable.melpa.org/#/whole-line-or-region)
[![Build Status](https://github.com/purcell/whole-line-or-region/workflows/CI/badge.svg)](https://github.com/purcell/whole-line-or-region/actions)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# In Emacs, operate on the current line if no region is active

This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

The primary use for this is to kill (cut) the current line if no
region is defined, and kill-region is invoked.  It basically saves you
the effort of going to the begining of the line, selecting the text up
to the end of the line, and killing.  Similarly, when yanking, it's
smart enough to know that the string to be yanked was killed as a
whole line, and it should be yanked as one, too.  So you don't need to
position yourself at the start of the line before yanking.  If region
*is* defined, though, all functions act as normal.

This minor mode was originally written by Joe Casadonte in 2001, and
was maintained for recent Emacsen by Steve Purcell for several
years. In 2020 the internals were rewritten to use modern Emacs
features such as `filter-buffer-substring-function` and
`yank-handler`, and thereby behave more consistently.

## Installation

### Manual

Ensure `whole-line-or-region.el` is in a directory on your load-path, and add
the following to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(require 'whole-line-or-region)
```

### MELPA

If you're an Emacs 24 user or you have a recent version of
`package.el` you can install `whole-line-or-region` from the
[MELPA](http://melpa.org) repository. The version of
`whole-line-or-region` there will always be up-to-date.

## About

Author: Steve Purcell <steve at sanityinc dot com>

Homepage: https://github.com/purcell/whole-line-or-region

<hr>

[💝 Support this project and my other Open Source work](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](https://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)