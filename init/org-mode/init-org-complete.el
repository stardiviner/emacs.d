;;; init-org-complete.el --- init for Org Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; add Org-mode's `capf' default to `pcomplete' for `company-mode'.

(defun my/org-mode-completion-setting ()
  "My basic settings for org-mode completion."
  (interactive)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil 'local)

  (setq-local company-minimum-prefix-length 3)
  (setq-local company-idle-delay 0.3)
  (setq-local company-tooltip-idle-delay 0.1)

  (make-local-variable 'company-auto-complete-chars)
  (setq company-auto-complete-chars '(?\( ?. ?#))
  
  (make-local-variable 'company-backends)
  (setq company-backends
        '(company-files
          (company-capf :with company-yasnippet
                        :separate company-tempo
                        :separate company-dabbrev-code)
          (company-keywords :with company-abbrev)
          company-ispell)))

(add-hook 'org-mode-hook #'my/org-mode-completion-setting)


(provide 'init-org-complete)

;;; init-org-complete.el ends here
