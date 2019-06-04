# Emacs configuration

- Version: _insert your big int here_
- Time-stamp: <2019-06-04 09:48:37>

## Modules system

Create a `modules.el` file to automatically load available [modules](./modules/).

<details>
    <summary>Sample content</summary>

```
(require 'feat-theme)

(require 'feat-completion)
(require 'feat-git)
(require 'feat-neotree)
(require 'feat-project)
(require 'feat-shell)
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
</details>
