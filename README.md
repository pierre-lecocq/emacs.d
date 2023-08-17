# Emacs configuration

(based on Emacs 30)

## Features

- LSP support (with `eglot`)
- Completion (with `company`)
- Project support (with `projectile`)
- Syntax checker (with `flycheck`)
- Git modification markers (with `git-gutter+`)
- Terminal integration (with `vterm`)
- Themes cycle (with `ef-themes`)
- Simplified modeline (with `simple-modeline`)

## Added keybindings

| Keys               | Command                            |
|--------------------|------------------------------------|
| <kbd>f9</kbd>      | `ef-themes-toggle`                 |
| <kbd>f11</kbd>     | `global-display-line-numbers-mode` |
| <kbd>f12</kbd>     | `imenu-list-smart-toggle`          |
| <kbd>C-RET</kbd>   | `toggle-terminal`                  |
| <kbd>M-g</kbd>     | `goto-line`                        |
| <kbd>C-c r</kbd>   | `comment-dwim`                     |
| <kbd>C-0</kbd>     | `other-window`                     |
| <kbd>C-x C-b</kbd> | `ibuffer`                          |
| <kbd>C-c C-u</kbd> | `string-inflection-all-cycle`      |
| <kbd>C-c g n</kbd> | `git-gutter+-next-hunk`            |
| <kbd>C-c g p</kbd> | `git-gutter+-previous-hunk`        |
| <kbd>C-c g s</kbd> | `git-gutter+-show-hunk`            |
| <kbd>C-c C-e</kbd> | `markdown-export-and-preview`      |
| <kbd>M-.</kbd>     | `xref-find-definitions`            |
| <kbd>M-?</kbd>     | `xref-find-references`             |
