# Emacs configuration

- Version: _insert your big int here_
- Time-stamp: <2019-12-29 13:59:48>

## Features

- Language server protocol support (`lsp-mode`)
- Completion (`company-mode`)
- Git integration (`magit`, `git-gutter`, `git-messenger`)
- Project management (`projectile`)
- Files tree (`treemacs`)
- Snippets (`yasnippet`)
- Syntax checking (`flycheck`)
- Buffer navigation (`imenu`, `imenu-list`)
- Persistent buffer (`persistent-scratch`)
- Visual helps toggles
- Supported languages: c, elisp, go, js, lisp, php, python, ruby

## Cheatsheet

| Key               | Function                           | Description                                 |
|-------------------|------------------------------------|---------------------------------------------|
| **Defaults**      |                                    |                                             |
| `M-g`             | `goto-line`                        |                                             |
| `C-c r`           | `comment-dwim`                     |                                             |
| `M-/`             | `hippie-expand`                    |                                             |
| `C-c C-b`         | `ibuffer`                          |                                             |
| **Windows**       |                                    |                                             |
| `C-o`             | `other-window`                     |                                             |
| `C-x 2`           |                                    | Adviced to switch to the new window         |
| `C-x 3`           |                                    | Adviced to switch to the new window         |
| **Edit**          |                                    |                                             |
| `C-c C-u`         | `string-inflection-all-cycle`      | Switch between camel case, snake case, ...  |
| `C-S-c C-S-c`     | `mc/edit-lines`                    | Multicursors edit                           |
| **Search**        |                                    |                                             |
| `M-%`             | `anzu-query-replace`               |                                             |
| `C-M-%`           | `anzu-query-replace-regexp`        |                                             |
| **Syntax**        |                                    |                                             |
| `C-c s e`         | `flycheck-list-errors`             |                                             |
| **Files tree**    |                                    |                                             |
| `C-c f t`         | `treemacs`                         |                                             |
| **Git**           |                                    |                                             |
| `C-c g m`         | `git-messenger:popup-message`      |                                             |
| `C-c g v`         | `git-messenger:popup-show-verbose` |                                             |
| `C-c g s`         | `magit-status`                     |                                             |
| **Imenu**         |                                    |                                             |
| `C-c i m`         | `imenu`                            |                                             |
| `C-c i l`         | `imenu-list`                       |                                             |
| **Projects**      |                                    |                                             |
| `C-c p p`         | `projectile-switch-project`        |                                             |
| `C-c p D`         | `projectile-dired`                 |                                             |
| `C-c p f`         | `projectile-find-file`             |                                             |
| `C-c p s g`       | `projectile-grep`                  |                                             |
| `C-c p r`         | `projectile-replace`               |                                             |
| `C-c p v`         | `projectile-vc`                    | Run `vc-dir` on the project root directory  |
| `C-c p R`         | `projectile-regenerate-tags`       |                                             |
| `C-c p j`         | `projectile-find-tag`              |                                             |
| **Visual helps**  |                                    |                                             |
| `C-c v p`         | `toggle-show-paren-mode-style`     | Switch style from parenthesis to expression |
| `C-c v i`         | `toggle-fill-column-indicator`     |                                             |
| `C-c v h`         | `toggle-highlight-indentation`     |                                             |
| `C-c v l`         | `toggle-linenum-mode`              |                                             |
| `C-c v w`         | `toggle-whitespace-mode-style`     | Switch style from minimal to very verbose   |
| **Visual themes** |                                    |                                             |
| `C-c v t`         | `switch-theme`                     | Cycle themes (`darkokai`, `modus-operandi`) |
