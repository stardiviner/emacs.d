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
            (setq-local completion-at-point-functions 'pcomplete-completions-at-point)

            ;; FIXME: improve this code.
            (make-local-variable 'company-backends)
            (setq company-backends (copy-tree company-backends))
            (setf (car company-backends)
                  (let ((company-backends-first-group (car company-backends)))
                    (setq temp company-backends-first-group)
                    (add-to-list 'temp 'company-ispell t)))
            ))


(provide 'init-my-org-complete)

;;; init-my-org-complete.el ends here
