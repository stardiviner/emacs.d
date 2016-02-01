;;; init-my-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Search Functions ]

(use-package ag
  :ensure t
  :config
  (defun ag-org (string directory)
    "Search using ag in a given DIRECTORY for a given search STRING,
with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag."
    (interactive
     (list
      (ag/read-from-minibuffer "Search string")
      (read-directory-name "Directory: " (expand-file-name "~/Org"))
      ))
    ;; (ag/search string directory :regexp nil :file-type 'org)
    (ag/search string directory :regexp nil :file-regex ".*\.org")
    )

  (define-key my-org-prefix (kbd "s") 'ag-org)
  )



(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
