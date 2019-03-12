# Emacs configuration

- Version: <insert your big int here>
- Time-stamp: <2019-03-12 15:46:01>

## Packages

### Common

| **Package**             | **Description**                                                                          | **Built-in** |
|-------------------------|------------------------------------------------------------------------------------------|--------------|
| `aggressive-indent`     | Emacs minor mode that keeps your code always indented                                    |              |
| `all-the-icons`         | A utility package to collect various Icon Fonts and propertize them within Emacs         |              |
| `anzu`                  | A minor mode which displays current match and total matches information in the mode-line |              |
| `autoinsert`            | Automagically inserts text into new buffers based on file extension or the major mode    | yes          |
| `autopair`              | Automagically pair braces and quotes in emacs                                            |              |
| `company`               | A text completion framework for Emacs                                                    |              |
| `darkokai-theme`        | Dark colour theme inspired by monokai                                                    |              |
| `diminish`              | Diminished modes are minor modes with no modeline display                                | yes          |
| `dumb-jump`             | An Emacs "jump to definition" package                                                    |              |
| `editorconfig`          | EditorConfig plugin for emacs                                                            |              |
| `epa-file`              | EasyPG is an all-in-one GnuPG interface for Emacs                                        | yes          |
| `etags-select`          | Select from multiple tags                                                                |              |
| `find-file-in-project`  | Quick access to project files in Emacs                                                   |              |
| `fill-column-indicator` | Indicate the location of the fill column by drawing a thin line                          |              |
| `flx-ido`               | Fuzzy matching for Emacs                                                                 |              |
| `flycheck`              | Syntax checking for GNU Emacs                                                            |              |
| `git-gutter`            | Display line's git status in the gutter                                                  |              |
| `grep`                  | Run the grep program                                                                     | yes          |
| `idle-highlight-mode`   | Highlights all occurences of the word under the point                                    |              |
| `ido`                   | Interactively DO things                                                                  | yes          |
| `ido-hacks`             | Misc collection of ido changes                                                           |              |
| `ido-vertical-mode`     | Makes ido-mode display vertically                                                        |              |
| `neotree`               | An Emacs tree plugin                                                                     |              |
| `rainbow-delimiters`    | Rainbow parentheses                                                                      |              |
| `rainbow-mode`          | Colorize color names in buffers                                                          | yes          |
| `time-stamp`            | Insert the current timestamp and keep it updated                                         | yes          |
| `which-func`            | Displays the current function name in the mode line                                      | yes          |
| `which-key`             | Displays available keybindings in popup                                                  |              |
| `whitespace-mode`       | A minor mode to visualize blanks                                                         | yes          |

### Language specific

| **Language**    | **Package**            | **Description** | **Built-in** |
|-----------------|------------------------|-----------------|--------------|
| **Emacs Lisp**  | `emacs-lisp-mode`      |                 | yes          |
|                 | `eros`                 |                 |              |
| **Common Lisp** | `lisp-mode`            |                 | yes          |
|                 | `slime`                |                 |              |
|                 | `slime-company`        |                 |              |
| **SH**          | `sh-mode`              |                 | yes          |
| **Makefile**    | `makefile-mode`        |                 | yes          |
| **C**           | `c-mode`               |                 | yes          |
|                 | `cc-mode`              |                 |              |
|                 | `company-c-headers`    |                 |              |
| **Go**          | `go-mode`              |                 |              |
|                 | `go-eldoc`             |                 |              |
|                 | `exec-path-from-shell` |                 |              |
| **Ruby**        | `ruby-mode`            |                 | yes          |
|                 | `ruby-tools`           |                 |              |
|                 | `rubocop`              |                 |              |
|                 | `robe`                 |                 |              |
|                 | `inf-ruby`             |                 |              |
| **Python**      | `python-mode`          |                 | yes          |
|                 | `elpy`                 |                 |              |
| **HTTP**        | `restclient`           |                 |              |
| **PHP**         | `php-mode`             |                 |              |
|                 | `php-extras`           |                 |              |
| **JS**          | `js2-mode`             |                 |              |
|                 | `js2-refactor`         |                 |              |
| **SQL**         | `sqlup-mode`           |                 |              |
|                 | `sql-indent`           |                 |              |
| **Web**         | `web-mode`             |                 |              |
|                 | `scss-mode`            |                 |              |
|                 | `htmlize`              |                 |              |
| **Text**        | `org`                  |                 |              |
|                 | `json-mode`            |                 |              |
|                 | `yaml-mode`            |                 |              |
|                 | `markdown-mode`        |                 |              |
|                 | `dockerfile-mode`      |                 |              |
|                 | `terraform-mode`       |                 |              |
|                 | `toml-mode`            |                 |              |

## Keybindings cheatsheet

### Common

| **Key**   | **Function**           | **Description**                            |
|-----------|------------------------|--------------------------------------------|
| `C-S-f`   | `imenu`                | Jump to a place in the buffer              |
| `C-x C-b` | `ibuffer`              | Edit a list of buffers                     |
| `M-g`     | `goto-line`            | Go to LINE                                 |
| `C-c r`   | `comment-dwim`         | Comment command you want                   |
| `M-/`     | `hippie-expand`        | Try to expand text before point            |
| `<f8>`    | `flycheck-list-errors` | Show the error list for the current buffer |

### Window

| **Key** | **Function**              | **Description**                                                           |
|---------|---------------------------|---------------------------------------------------------------------------|
| `C-x 2` | `split-window-and-switch` | Split window horizontally and switch to the new one                       |
| `C-x 3` | `split-window-and-switch` | Split window vertically and switch to the new one                         |
| `C-;`   | `other-window`            | Select another window in cyclic ordering of windows                       |
| `M-;`   | `other-frame`             | Select the ARGth different visible frame on current display, and raise it |

### Search

| **Key**       | **Function**           | **Description**                                                                   |
|---------------|------------------------|-----------------------------------------------------------------------------------|
| `C-S-x C-S-f` | `find-file-in-project` | Prompt with a completing list of all files in the project to find one             |
| `C-c s g`     | `vc-git-grep`          | Run git grep, searching for REGEXP in FILES in directory DIR                      |
| `C-c s r`     | `rgrep`                | Recursively grep for REGEXP in FILES in directory tree rooted at DIR              |
| `C-c s o`     | `occur`                | Show all lines in the current buffer containing a match for REGEXP                |
|               |                        | Use `n` for next occurence, `p` for previous occurrence and `o` to show occurence |

### Quick jump

| **Key**   | **Function**             | **Description**                                                             |
|-----------|--------------------------|-----------------------------------------------------------------------------|
| `C-c q l` | `dumb-jump-quick-look`   | Show a tooltip of where it would jump instead of jumping                    |
| `C-c q g` | `dumb-jump-go`           | Go to the function/variable declaration for thing at point                  |
| `C-c q b` | `dumb-jump-back`         | Jump back to where the last jump was done                                   |
| `C-c q o` | `dumb-jump-other-window` | Like ’dumb-jump-go’ but use ’find-file-other-window’ instead of ’find-file’ |
| `C-c q p` | `dumb-jump-go-prompt`    | Like dumb-jump-go but prompts for function instead of using under point     |

### Tags

| **Key**   | **Function**            | **Description**                              |
|-----------|-------------------------|----------------------------------------------|
| `C-c t r` | `refresh-tags`          | Refresh tags table of the current project    |
| `C-c t s` | `etags-select-find-tag` | Do a find-tag, and display all exact matches |

### File tree

| **Key**   | **Function**                 | **Description**                                        |
|-----------|------------------------------|--------------------------------------------------------|
| `C-c t t` | `neotree-toggle`             | Toggle show the NeoTree window                         |
| `C-c t p` | `neotree-project-dir`        | Open NeoTree using the git root of the current project |
| `C-c t h` | `neotree-hidden-file-toggle` | Toggle show hidden files                               |
