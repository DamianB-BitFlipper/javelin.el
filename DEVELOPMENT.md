# Development Guide

This guide is for [Doom Emacs](https://github.com/doomemacs/doomemacs) users.

## Local Installation

Add the following to your `packages.el`:

```elisp
(package! javelin
  :recipe (:local-repo "/path/to/javelin.el"
           :build (:not compile)))
```

Then run `doom sync` to install.

## Reloading Changes

### If autoloads change

Run:

```elisp
(doom/reload)
```

### For other changes

Use one of:

```elisp
(load-library "javelin")  ; Reload the entire library
```

or

```elisp
(eval-buffer)  ; Evaluate the current buffer (when editing javelin.el)
```
