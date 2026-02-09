# Javelin.el

Quick file and buffer bookmarking for Emacs, inspired by [ThePrimeagen's Harpoon](https://github.com/ThePrimeagen/harpoon).

Pin bookmarks to numbered javelin positions (1-9) for instant access. Javelin positions are automatically separated by project and git branch.

<p align="center">

https://github.com/user-attachments/assets/e1d0fe82-1def-4ef4-af06-f98e3c6655b2

</p>

## Installation

### Doom Emacs

In `packages.el`:
```elisp
(package! javelin)
```

In `config.el`:
```elisp
(use-package! javelin
  :custom
  (javelin-update-bookmark t)
  :config
  (global-javelin-minor-mode 1))
```

### Vanilla Emacs

```elisp
(use-package javelin
  :ensure t
  :custom
  (javelin-update-bookmark t)
  :config
  (global-javelin-minor-mode 1))
```

## Keybindings

| Key | Action |
|-----|--------|
| `M-1` to `M-9` | Jump to buffer (or assign if empty) |
| `C-u M-1` ... | Assign current buffer to position |
| `M-0 1` to `M-0 9` | Delete position |
| `M--` | Quick menu to select from all javelined buffers |

## Configuration

| Variable | Description |
|----------|-------------|
| `javelin-update-bookmark` | When non-nil, automatically update cursor position when jumping between javelin positions. Default is `nil`. |

## Key Features

- Jump to any pinned buffer with a single keystroke (M-1 through M-9)
- Javelin positions remember your cursor position when pinning so that jumping restores you exactly right back there
- Javelin positions are automatically scoped to each project and git branch, keeping your bookmarks organized

## Commands

- `javelin-go-or-assign-to-<NUM>` - Go to position if occupied, otherwise assign (prefix arg to force assign)
- `javelin-delete-<NUM>` - Delete position `<NUM>`
- `javelin-toggle-quick-menu` - Select from javelined buffers
- `javelin-go-to-<NUM>` - Go to buffer at position `<NUM>`
- `javelin-assign-to-<NUM>` - Assign current buffer to position `<NUM>`
- `javelin-go-to-next` / `javelin-go-to-prev` - Cycle through javelined buffers
- `javelin-add-file` - Add current buffer to next available position
- `javelin-clear` - Clear all positions for current project/branch
- `javelin-clear-all` - Clear all positions across all projects

Where `<NUM>` is a number from 1-9.

---

<a href="https://buymeacoffee.com/thebitflipper" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" height="41" width="174"></a>

## Acknowledgments

Based on [harpoon.el](https://github.com/otavioschwanck/harpoon.el) by Otavio Schwanck.
