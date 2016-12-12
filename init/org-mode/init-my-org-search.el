;;; init-my-org-search.el --- init for Org search.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-occur-case-fold-search 'smart)

;;; [ Agenda dispatcher ]

;; (org-agenda-dispatch)

;;; [ org-searching ] -- Searching Org-mode files in directory.

(use-package org-searching
  :ensure t
  :commands (org-searching-string org-searching-regexp org-searching-headlines)
  :config
  (define-key my-org-prefix (kbd "s") 'org-searching-headlines)
  (define-key my-org-prefix (kbd "S") 'org-searching-string)
  (define-key my-org-prefix (kbd "M-s") 'org-searching-regexp)
  )

;;; [ Search Functions ]

(use-package ag
  :ensure t
  :config
  (autoload 'ag/read-from-minibuffer "ag")
  
  (defun ag-org-search (string directory)
    "Full context searching STRING using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag."
    (interactive
     (list
      (ag/read-from-minibuffer "Search string in Org:")
      (read-directory-name "Directory: " (expand-file-name "~/Org"))
      ))
    ;; (ag/search string directory :regexp nil :file-type 'org)
    (ag/search string directory :regexp nil :file-regex ".*\.org")
    )

  (defun ag-org-search-headline (string directory)
    "Search STRING in Org files headlines using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag."
    (interactive
     (list
      (ag/read-from-minibuffer "Search headlines in Org:")
      (read-directory-name "Directory: "
                           (expand-file-name
                            (if current-prefix-arg "~/Org" ".")))
      ))
    (ag/search (concat "^(\\*)+\ .*" string) directory :regexp t :file-regex ".*\.org")
    
    ;; the real shell command and regexp result
    ;; ag --file-search-regex .\*.org --group --line-number --column --color --color-match 30\;43 --color-path 1\;32 --smart-case --stats -- \^\(\\\*\)\+.\*time .
    )

  (define-key my-org-prefix (kbd "s") 'ag-org-search-headline)
  (define-key my-org-prefix (kbd "S") 'ag-org-search)
  )



(provide 'init-my-org-search)

;;; init-my-org-search.el ends here
