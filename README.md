# Javelin.el

Quick file bookmarks for Emacs, inspired by [ThePrimeagen's Harpoon](https://github.com/ThePrimeagen/harpoon).

Pin files to numbered positions (1-9) for instant access. Positions are automatically separated by project and git branch.

## Installation

### Doom Emacs

In `packages.el`:
```elisp
(package! javelin)
```

In `config.el`:
```elisp
(use-package! javelin
  :config
  (global-javelin-minor-mode 1))
```

### Vanilla Emacs

```elisp
(use-package javelin
  :ensure t
  :config
  (global-javelin-minor-mode 1))
```

## Keybindings

| Key | Action |
|-----|--------|
| `M-1` to `M-9` | Jump to file (or assign if empty) |
| `C-u M-1` ... | Assign current file to position |
| `M-0 1` to `M-0 9` | Delete position |
| `M--` | Quick menu to select from all javelined files |

## Commands

- `javelin-add-file` - Add current file to next available position
- `javelin-toggle-quick-menu` - Select from javelined files
- `javelin-go-to-next` / `javelin-go-to-prev` - Cycle through files
- `javelin-clear` - Clear all positions

## Acknowledgments

Based on [harpoon.el](https://github.com/otavioschwanck/harpoon.el) by Otavio Schwanck.

---

<a href="https://buymeacoffee.com/thebitflipper" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" height="41" width="174"></a>
