;;; lang-sh.el --- Shell language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:29:13>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((sh-mode . "Shell script") nil
               "#!/usr/bin/env bash\n"
               "# -*- mode: sh; -*-\n\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               "# Description: " _ "\n\n"
               "set -o errexit\n\n"
               "[ -z $BASH ] && (echo \"Not in a BASH sub process\"; exit 1)\n"
               "BASE_DIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)\n\n"))

;;; lang-sh.el ends here
