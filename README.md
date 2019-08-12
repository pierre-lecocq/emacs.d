# Emacs configuration

- Version: _insert your big int here_
- Time-stamp: <2019-08-12 18:00:18>

## Built-in packages

- `column-number-mode`
- `epa-file`
- `global-auto-revert-mode`
- `global-font-lock-mode`
- `global-hl-line-mode`
- `global-subword-mode`
- `line-number-mode`
- `show-paren-mode`
- `time-stamp`
- `which-func`
- `whitespace`

## Default packages

- `use-package`
- `aggressive-indent`
- `anzu`
- `autopair`
- `editorconfig`
- `exec-path-from-shell`
- `fill-column-indicator`
- `idle-highlight-mode`
- `ido`, `flx-ido`, `ido-hacks`, `ido-vertical-mode`
- `string-inflection`
- `which-key`

## Modules system

Create a `host.el` file to automatically load available [modules](./modules/) and set custom variables.

**Full example**:

```
;; File: host.el

(defvar host-frame-type    'fullscreen) ;; default, maximized, fullscreen
(defvar host-modeline-type 'simple)     ;; none, default, simple

(require 'look-theme)
(require 'look-modeline)

(require 'feat-completion)
(require 'feat-git)
(require 'feat-multicursors)
(require 'feat-neotree)
(require 'feat-project)
(require 'feat-snippets)
(require 'feat-syntax)

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


### Available modules packages

| File                                                   | Description           | Packages                                                                                                            |
|--------------------------------------------------------|-----------------------|---------------------------------------------------------------------------------------------------------------------|
| [feat-completion.el](./modules/feat-completion.el)     | Text completion       | `company-mode`                                                                                                      |
| [feat-git.el](./modules/feat-git.el)                   | Git support           | `git-gutter`, `git-messenger`                                                                                       |
| [feat-multicursors.el](./modules/feat-multicursors.el) | Multi cursors support | `multi-cursors`                                                                                                     |
| [feat-neotree.el](./modules/feat-neotree.el)           | File brower           | `neotree`                                                                                                           |
| [feat-project.el](./modules/feat-project.el)           | Project management    | `projectile`                                                                                                        |
| [feat-snippets.el](./modules/feat-snippets.el)         | Snippets              | `yasnippet`                                                                                                         |
| [feat-syntax.el](./modules/feat-syntax.el)             | Syntax checking       | `flycheck`                                                                                                          |
| [lang-c.el](./modules/lang-c.el)                       | C language family     | `c-mode`, `cc-mode`, `company-c-headers`                                                                            |
| [lang-elisp.el](./modules/lang-elisp.el)               | Emacs lisp language   | `emacs-lis-mode`, `eros-mode`, `eldoc-mode`                                                                         |
| [lang-go.el](./modules/lang-go.el)                     | Go language           | `go-mode`, `go-eldoc`, `company-go`                                                                                 |
| [lang-http.el](./modules/lang-http.el)                 | HTTP support          | `restclient`                                                                                                        |
| [lang-js.el](./modules/lang-js.el)                     | Javascript language   | `js2-mode`, `js2-refactor`, `xref-js2`, `company-tern`                                                              |
| [lang-lisp.el](./modules/lang-lisp.el)                 | Common Lisp language  | `lisp-mode`, `slime`, `slime-company`                                                                               |
| [lang-makefile.el](./modules/lang-makefile.el)         | Makefile support      | `makefile-mode`                                                                                                     |
| [lang-php.el](./modules/lang-php.el)                   | PHP language          | `php-mode`, `php-extras`                                                                                            |
| [lang-python.el](./modules/lang-python.el)             | Python language       | `python-mode`, `elpy`                                                                                               |
| [lang-ruby.el](./modules/lang-ruby.el)                 | Ruby language         | `ruby-mode`, `inf-ruby`, `robe`, `rubocop`, `ruby-tools`, `yard-mode`                                               |
| [lang-text.el](./modules/lang-text.el)                 | Text based languages  | `dockerfile-mode`, `dotenv-mode`, `json-mode`, `markdown-mode`, `flymd`, `terraform-mode`, `toml-mode`, `yaml-mode` |
| [lang-web.el](./modules/lang-web.el)                   | Web languages family  | `htmlize`, `scss-mode`, `web-mode`                                                                                  |
| [look-modeline.el](./modules/look-modeline.el)         | Modeline theme        |                                                                                                                     |
| [look-theme.el](./modules/look-theme.el)               | Theme                 | `all-the-icons`, `all-the-icons-dired`                                                                              |
