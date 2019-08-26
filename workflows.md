# Emacs Workflows Cheat Sheet

**Table of Contents**

- [Isearch](#isearch)
- [Replace](#replace)
- [Occur](#occur)
- [Grep and find](#grep-and-find)
- [Dired](#dired)

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
