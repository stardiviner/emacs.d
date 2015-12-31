;;; init-my-org-complete.el --- init for Org Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(if (featurep 'helm)
    (setq org-completion-fallback-command 'helm)
  ;; (setq org-completion-fallback-command 'hippie-expand)
  (setq org-completion-fallback-command 'completing-read)
  )

;; (if (featurep 'ido-vertical-mode)
;;     (setq org-completion-use-ido t)
;;   (setq org-completion-use-ido nil)
;;   (setq org-completion-use-iswitchb nil)
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
