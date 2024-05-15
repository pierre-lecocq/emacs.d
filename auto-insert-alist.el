;;; auto-insert-alist.el --- Auto insert configuration -*- lexical-binding: t; -*-

;;; Local Variables:
;;; eval: (remove-hook 'before-save-hook 'time-stamp t)
;;; End:

;;; Commentary:

;;; Code:

(setq auto-insert-alist
      `(
        (("\\.el$" . "Elisp files")
         "Description: "
         ";;; " (file-name-nondirectory buffer-file-name) " --- " str " -*- lexical-binding: t; -*-\n\n"
         ";; Creation: " (current-time-string) "\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         _
         "\n\n;;; " (file-name-nondirectory buffer-file-name) " ends here.\n")

        (("\\.js$" . "JavaScript files") nil
         "// File: " (file-name-nondirectory buffer-file-name) "\n"
         "// Creation: " (current-time-string) "\n"
         "// Time-stamp: <>\n"
         "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")

        (("\\.php$" . "PHP files") nil
         "<?php\n\n"
         "// File: " (file-name-nondirectory buffer-file-name) "\n"
         "// Creation: " (current-time-string) "\n"
         "// Time-stamp: <>\n"
         "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")

        (("\\.html$" . "HTML files")
         "Title: "
         "<!DOCTYPE html>\n"
         "<html lang=\"en\">\n"
         "  <head>\n"
         "    <meta charset=\"utf-8\" />\n"
         "    <title>" str "</title>\n"
         "    <link href=\"style.css\" rel=\"stylesheet\" />\n"
         "  </head>\n"
         "  <body>\n"
         _
         "\n  </body>\n"
         "</html>\n")

        (("\\.go$" . "GoLang files") nil
         "// File: " (file-name-nondirectory buffer-file-name) "\n"
         "// Creation: " (current-time-string) "\n"
         "// Time-stamp: <>\n"
         "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")

        (("\\.py$" . "Python files") nil
         "#!/usr/bin/env python3\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Creation: " (current-time-string) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         "def main():\n"
         _
         "\nif __name__ == '__main__':\n"
         "    main()\n")

        (("\\.sh$" . "Shell files") nil
         "#!/usr/bin/env bash\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Creation: " (current-time-string) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         "set -ex\n\n")

        (("\\.http$" . "HTTP client files") nil
         "# -*- restclient -*-\n#\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Creation: " (current-time-string) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
        ))

;;; auto-insert-alist.el ends here.
