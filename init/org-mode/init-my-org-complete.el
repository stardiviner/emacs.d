;;; init-my-org-complete.el --- init for Org Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; add Org-mode's `capf' default to `pcomplete' for `company-mode'.

(defun my-org-mode-completion-setting ()
  "My basic settings for org-mode completion."
  (interactive)
  (setq-local company-minimum-prefix-length 2)

  (add-hook 'completion-at-point-functions
            'pcomplete-completions-at-point nil 'local)

  (make-local-variable 'company-auto-complete-chars)
  (setq company-auto-complete-chars '(?\( ?. ?#))
  
  (make-local-variable 'company-backends)
  (setq company-backends
        `(company-capf
          company-yasnippet
          company-dabbrev-code company-abbrev))
  )

(add-hook 'org-mode-hook #'my-org-mode-completion-setting)


(provide 'init-my-org-complete)

;;; init-my-org-complete.el ends here
