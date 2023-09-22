# Emacs Cheat Sheet

**Table of Contents**

- [Install](#install)
- [Help](#help)
- [Movement](#movement)
- [Registers](#registers)
- [Isearch](#isearch)
- [Replace](#replace)
- [Occur](#occur)
- [Grep and find](#grep-and-find)
- [Dired](#dired)
- [Todo list](#todo-list)
- [SQL](#sql)

---

## Install

### Emacs

#### Mac OS X

```sh
brew tap d12frosted/emacs-plus
brew install --with-xwidgets --with-native-comp --with-elrumo2-icon emacs-plus@30
brew linkapps emacs-plus
```

### Dependencies

For Lisp

```sh
brew install sbcl

curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit

sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

For Go

```sh
go get -u golang.org/x/lint/golint
go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
```

For PHP and JS

```sh
npm i -g eslint typescript-language-server typescript intelephense
```

---

## Help

Emacs has the most powerful internal help system. Here are a few useful commands:

- `C-h a RET <pattern>` to show all commands matching a pattern
- `C-h k <key>` to show the command bound to `<key>`
- `C-h v <variable>` to show the documentation of `<variable>`
- `C-h f <function>` to show the documentation of `<function>`
- `C-h m` to show the documentation of the current major mode and all activated minor-modes
- `C-h b` to show all the key bindings
- `C-h e` to show the `*Messages*` buffer

---

## Movement

- `C-f`/`C-b` to move a character forward/backward (can be `C-u` prefixed)
- `M-f`/`M-b` to move a word forward/backward (can be `C-u` prefixed)

- `C-n`/`C-p` to move a line forward/backward (can be `C-u` prefixed)

- `C-a`/`C-e` to move to the begining/end of the line

- `M-a`/`M-e` to move to the begining/end of the paragraph

- `C-v`/`M-v` to jump one screen forward/backward
- `C-l` to center the screen at point

- `M-<`/`M->` to jump to the begining/end of the file

---

## Registers

Emacs has registers where anything can be saved and reused afterwards: text, position, window configurations, files, numbers, ... and even macros.

- `M-x list-registers` to list what is stored in the registers

- `C-x r s <letter>` to save the current region or killed text in the register `<letter>`
- `C-x r i <letter>` to insert the region or text from the register `<letter>`

- `C-x r SPACE <letter>` to store the current position in the register `<letter>`
- `C-x r j <letter>` to jump to the position in the register `<letter>`

### References

- https://tech.toryanderson.com/2020/07/03/emacs-tip-registers/
- https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html
- https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macro-Registers.html#Keyboard-Macro-Registers

---

## Isearch

Use `M-x isearch-forward` (or `C-s`) and `M-x isearch-forward-regexp` (or `C-M-s`) to search for patterns in a buffer.

### In the search loop

- Navigate through occurrences with `C-s` or `C-r`
- Hit `M-e` to edit the current search pattern and then `RET` to activate the new search loop
- In the minibuffer, use `M-n` and `M-p` to cycle through previously searched patterns
- Hit `M-s o` to switch the results of the search to `occurr`
- Use `M-r` to switch to regexp search
- Hit `M-c` to toggle the case sensitivity
- Hit `RET` to stop searching

### Outside the search loop

- Use `C-u C-SPACE` to go back to the position before searching
- Use `C-s C-s` to search for the last pattern searched
- Hit `C-s C-w` to search the word at point
  - Multiple `C-w` after that will extend the selection
- Hit `M-s .` to search the wider word at point

### References

- https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html#Search
- https://bzg.fr/en/tutorial-introduction-searching-emacs.html/

---

## Replace

Use `M-x anzu-query-replace` (or `M-%`) and `M-x anzu-query-replace-regexp` (or `C-M-%`) to replace patterns in a buffer.

### In the replace loop

- Hit `!` to replace all occurrences
- Hit `y` to replace one occurrence
- Hit `n` to skip one occurrence
- In the minibuffer, use `M-n` and `M-p` to cycle through previously replaced patterns
- Hit `M-s o` to switch the results of the search to `occurr`

### Outside the replace loop

- Use `C-u C-SPACE` to go back to the position before replacing

---

## Occur

Use `M-x occur` (or `M-s o`) to find occurrences of a string or regexp in a buffer.

### Common usage

It opens a read-only buffer with the results.

- Navigate through occurrences with the standard `M-n` or `M-p`
  - Activate the follow mode with `C-c C-f` so when `M-n` or `M-p` are hit, the cursor move to the location
- Rename the buffer accordingly to the file by typing `r` (useful if several search buffers are opened)
- Update the buffer by re-running the search by typing `g` (useful if the target buffer has changed)
- With a prefix (`C-u 5 M-s o`), it will include extra lines around the result (here, 5 lines)

### Edit results

The results buffer can be edited by hittinh `e` to enter the edit mode.

- Perform a query/replace or a macro on the results
- Hit `C-c C-c` to apply changes to the target buffer

### References

- https://www.youtube.com/watch?v=zxS3zXwV0PU
- https://www.masteringemacs.org/article/searching-buffers-occur-mode

---

## Grep and find

- Use `M-x vc-git-grep RET <search pattern> RET <file name pattern> RET <root dir>` to search a pattern through a git repository
  - Use `M-x rgrep RET <search pattern> RET <file name pattern> RET <root dir>` to search a pattern from a given directory
- Use `M-x find-name-dired RET <root dir> RET <file name pattern>` to search files from a given directory. This produces a `dired` buffer, so any `dired` actions are possible

---

## Dired

Use `M-x dired` (or `C-x d`) to open a file system listing buffer.

### Navigation

- `n` and `p` to navigate through files
  - `C-u 5 n` will move to the 5th next file
- `^` to move to the parent directory
- `RET` to open a directory in another dired buffer

### Listing

- `C-u i` allows to switch the `ls` flags (i.e add the `-R` flag in case of a directory)
- On a directory, `i` will display its content in the same buffer
  - Any action can now be performed directly on the sub-files listed below
  - `C-_` to remove the display the content of the directory

### Finding

- Use `M-x find-name-dired RET <root dir> RET <file name pattern>` to search files from a given directory
- Use `M-x image-dired RET <root dir>` to display images from a `dired` buffer
  - `RET` on a thumbnail to display the image full size

### Actions

- `g` to refresh the listing
- `+` to create a new directory
- `C` to copy the file at point
  - Use `M-n` to insert the original name (it can be useful to re-use a part of that name)
- `R` to rename the file at point
  - Use `M-n` to insert the original name (it can be useful to re-use a part of that name)
- `d` mark the file at point for deletion
- `D` to delete the file at point
- `=` to diff the file at point with another one. This will produce a `diff-mode` buffer

### Marks

- `m` to mark the file at point
- `%m` to mark multiple files from a regexp
- `u` to unmark the file at point
- `U` to unmark all the files

Then

- `x` to confirm actions on marked files

### Edit

- Use `C-x C-q` to deactivate the read only mode and thus make the buffer editable.
- Edit the buffer by performing changes manually, or with a query-replace, or a macro, ... etc
- Press `C-c C-c` to validate the changes or `C-c ESC` to cancel them

### References

- https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
- https://www.youtube.com/watch?v=5dlydii7tAU
- https://www.youtube.com/watch?v=2lF3z7H8oaI

---

## Todo list

`M-x todo-show` runs mode to manage todo list. Files are saved in the emacs root directory.

To create a new file, run `M-x todo-show` again. One file can contain several categories

### Navigation

- Use `n` or `p` to navigate through items
- Use `f` or `b` to jump to the first or last item
- Use `S` to search for items

### Actions on items

- `i` to insert a new item
- `e` to edit an item
  - `m` to add multiline comment to the item, and then `C-x C-q` to validate
- `k` to delete an item
- `d` to mark an item as done
  - `v` to toggle display of items marked as done with the todo items
  - `V` to toggle display of only items marked as done
- `u` to unmark an item as done

### Actions on multiple items

- `*` to mark one item
- `C-*` to mark all items
- `C-u` to unmark items

Then, use `k`, `d`, `m` ... actions on items

### Move items

- `r` or `l` to relocate an item (raise or lower)
- `#` to relocate an item to a given index
- `m` to move an item to a category

### Categories

- `j` to jump to another category or create a new one
- `F c` to list categories with statistics
- `r` or `l` to relocate a category (raise or lower)
- `#` to relocate a category to a given index

### References

- https://www.gnu.org/software/emacs/manual/html_mono/todo-mode.html

---

## SQL

### Connect to a server

`M-x sql-postgres` prompts for database connection details and connects to the server.
The result is just a prompt like the `psql` client in a terminal, which is convenient but not a big step forward.

### Sending SQL commands

The nice addition to that is that from any other buffer some commands allow to send queries to the open connection:

- `C-c C-b` in SQL mode, or `M-x sql-send-buffer` in other modes
- `C-c C-r` in SQL mode, or `M-x sql-send-region` in other modes
- `C-c C-s` in SQL mode, or `M-x sql-send-string` in other modes
- `C-c C-c` in SQL mode, or `M-x sql-send-paragraph` in other modes
- `C-c C-n` in SQL mode, or `M-x sql-send-line-and-next` in other modes

### References

- https://arjanvandergaag.nl/blog/using-emacs-as-a-database-client.html
