# Emacs configuration

- Version: _insert your big int here_
- Time-stamp: <2019-09-11 15:48:12>

## Features

- Host specific modules activation
- Completion with `company-mode`
- Git integration with `git-gutter` and `git-messenger`
- Project management with `projectile`
- Snippets integration with `yasnippet`
- Syntax verification and linting with `flycheck`
- File tree with `neotree`
- Visual helps toggles
- Supported languages: c, elisp, go, js, lisp, php, python, ruby

## Modules system

Create a `host.el` file to automatically load available [modules](./modules/) for this specific host.
This file is intentionally not versioned since it varies on the different machines used.

**Full example**:

```
;; File: host.el

;; Look and feel
(require 'init-theme)

;; Features
(require 'init-completion)
(require 'init-cursors)
(require 'init-git)
(require 'init-neotree)
(require 'init-project)
(require 'init-snippets)
(require 'init-syntax)

;; Programming languages
(require 'lang-c)
(require 'lang-elisp)
(require 'lang-go)
(require 'lang-http)
(require 'lang-js)
(require 'lang-lisp)
(require 'lang-makefile)
(require 'lang-php)
(require 'lang-python)
(require 'lang-ruby)
(require 'lang-text)
(require 'lang-web)
```

## Cheatsheet

| Key               | Function                           | Description                                              |
|-------------------|------------------------------------|----------------------------------------------------------|
| **Defaults**      |                                    |                                                          |
| `C-S-f`           | `imenu`                            |                                                          |
| `M-g`             | `goto-line`                        |                                                          |
| `C-c r`           | `comment-dwim`                     |                                                          |
| `M-/`             | `hippie-expand`                    |                                                          |
| `C-c C-b`         | `ibuffer`                          |                                                          |
| **Windows**       |                                    |                                                          |
| `C-o`             | `other-window`                     |                                                          |
| `C-x 2`           | `split-window-and-switch`          | Split window horizontally and switch to the new one      |
| `C-x 3`           | `split-window-and-switch`          | Split window vertically and switch to the new one        |
| **Edit**          |                                    |                                                          |
| `C-c C-u`         | `string-inflection-all-cycle`      | Switch between camel case, snake case, ...               |
| `C-S-c C-S-c`     | `mc/edit-lines`                    | Multicursors edit                                        |
| **Search**        |                                    |                                                          |
| `M-%`             | `anzu-query-replace`               |                                                          |
| `C-M-%`           | `anzu-query-replace-regexp`        |                                                          |
| **Syntax**        |                                    |                                                          |
| `C-c s e`         | `flycheck-list-errors`             |                                                          |
| **Git**           |                                    |                                                          |
| `C-c g m`         | `git-messenger:popup-message`      |                                                          |
| `C-c g v`         | `git-messenger:popup-show-verbose` |                                                          |
| **Files**         |                                    |                                                          |
| `C-c f t`         | `neotree-toggle`                   |                                                          |
| `C-c f p`         | `neotree-project-dir`              | Move to the project root directory (requires Projectile) |
| `C-c f h`         | `neotree-hidden-file-toggle`       |                                                          |
| **Projects**      |                                    |                                                          |
| `C-c p p`         | `projectile-switch-project`        |                                                          |
| `C-c p D`         | `projectile-dired`                 |                                                          |
| `C-c p f`         | `projectile-find-file`             |                                                          |
| `C-c p s g`       | `projectile-grep`                  |                                                          |
| `C-c p r`         | `projectile-replace`               |                                                          |
| `C-c p v`         | `projectile-vc`                    | Run `vc-dir` on the project root directory               |
| `C-c p R`         | `projectile-regenerate-tags`       |                                                          |
| `C-c p j`         | `projectile-find-tag`              |                                                          |
| **Visual helps**  |                                    |                                                          |
| `C-c v p`         | `toggle-show-paren-mode-style`     | Switch style from parenthesis to expression              |
| `C-c v i`         | `toggle-fill-column-indicator`     |                                                          |
| `C-c v f`         | `toggle-focus-mode`                |                                                          |
| `C-c v l`         | `toggle-linenum-mode`              |                                                          |
| `C-c v b`         | `toggle-rainbow-mode`              |                                                          |
| `C-c v c`         | `toggle-centered-window-mode`      |                                                          |
| `C-c v w`         | `toggle-whitespace-mode-style`     | Switch style from minimal to very verbose                |
| **Visual themes** |                                    |                                                          |
| `C-c v t`         | `switch-theme`                     | Cycle themes (dark and light)                            |
