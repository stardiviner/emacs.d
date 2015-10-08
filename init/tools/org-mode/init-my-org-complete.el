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


;;; add Org-mode's default `pcomplete' to `company-mode'.

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions 'pcomplete-completions-at-point)

            (setq company-backends (copy-tree company-backends))
            (setf (car company-backends)
                  (append (car company-backends)
                          '(company-ispell)))
            ))


(provide 'init-my-org-complete)

;;; init-my-org-complete.el ends here
