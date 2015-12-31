;;; init-my-org-complete.el --- init for Org Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(setq org-completion-use-ido t)

;; (if (featurep 'helm)
;;     (setq org-completion-fallback-command 'helm)
;;   ;; `completing-read', `ido', `ivy-read', `hippie-expand',
;;   (setq org-completion-fallback-command 'completing-read)
;;   )


;;; add Org-mode's `capf' default to `pcomplete' for `company-mode'.

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        '(pcomplete-completions-at-point t))

            ;; (my-company-add-backends-to-mode '(company-ispell))
            ))


(provide 'init-my-org-complete)

;;; init-my-org-complete.el ends here
