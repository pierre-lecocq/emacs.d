;;; 03-autoinsert.el --- Emacs Config - AutoInsert

;;; Commentary:
;; Time-stamp: <2015-02-25 23:39:35 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

(add-hook 'find-file-hooks 'auto-insert)
(add-hook 'before-save-hook 'time-stamp)
(setq auto-insert-copyright (user-full-name))

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         "# Description: " _ "\n\n")

        ((lisp-mode . "Lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";;; Commentary:\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")

        ((emacs-lisp-mode . "Emacs lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";;; Commentary:\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")

        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         " * Description: " _ "\n"
         " */\n\n"
         "#include <stdlib.h>\n"
         "#include <stdio.h>\n"
         "#include <unistd.h>\n\n"
         "int main(int argc, char **argv)\n"
         "{\n"
         "    return(0);\n"
         "}\n")

        ((shell-mode . "Shell script") nil
         "#!/bin/bash\n\n"
         " # File: " (file-name-nondirectory buffer-file-name) "\n"
         " # Time-stamp: <>\n"
         " # Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         " # Description: " _ "\n\n")

        ((php-mode . "PHP script") nil
         "<?php\n\n"
         "/**\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         " * Description: " _ "\n"
         " */\n\n")))

;;; 03-autoinsert.el ends here
