# Emacs configuration

- Version: _insert your big int here_
- Time-stamp: <2019-06-03 15:10:45>

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
(require 'feat-snippets)
(require 'feat-syntax)
(require 'lang-elisp)
(require 'lang-lisp)
(require 'lang-makefile)
(require 'lang-ruby)
```
</details>
