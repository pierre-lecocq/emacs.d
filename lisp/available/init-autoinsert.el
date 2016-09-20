;;; init-autoinsert.el --- Autoinsert

;; Time-stamp: <2016-07-11 12:07:18>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'autoinsert)

(auto-insert-mode 1)
(auto-insert)

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n"
         "# -*- mode: ruby; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((python-mode . "Python program") nil
         "#!/usr/bin/env python\n"
         "# -*- mode: python; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((lisp-mode . "Lisp program") nil
         ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
        ((emacs-lisp-mode . "Emacs lisp program") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         " * Description: " _ "\n"
         " */\n\n")
        ((sh-mode . "Shell script") nil
         "#!/usr/bin/env bash\n"
         "# -*- mode: sh; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n"
         "# set -o xtrace\n"
         "set -o nounset\n"
         "set -o errexit\n"
         "set -o pipefail\n\n"
         "__dir=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n"
         "__file=\"${__dir}/$(basename \"${BASH_SOURCE[0]}\")\"\n"
         "__base=\"$(basename ${__file} .sh)\"\n\n")
        ((org-mode . "Org mode") nil
         "#+AUTHOR: " (user-full-name) "\n"
         "#+DATE: " (current-time-string) "\n"
         "#+STARTUP: showall\n\n")))

(provide 'init-autoinsert)

;;; init-autoinsert.el ends here
