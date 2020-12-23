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
  (setq-local company-tooltip-idle-delay 0)
  
  (make-local-variable 'company-backends)
  (setq-local company-backends
              '((company-capf :with company-yasnippet
                              :separate company-tempo)
                company-keywords
                company-files
                ;; company-ispell
                )))

(add-hook 'org-mode-hook #'my/org-mode-completion-setting)


;;; [ helm-org ] -- Helm for org headlines and keywords completion.

(use-package helm-org
  :ensure t
  :after org
  :commands (helm-org-agenda-files-headings
             helm-org-in-buffer-headings
             helm-org-parent-headings
             helm-org-capture-templates)
  :bind (([remap org-goto] . helm-org-in-buffer-headings)) ; [C-c C-j]
  :custom (helm-org-headings-fontify t)
  :config
  (with-eval-after-load 'helm-mode
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-set-tags . helm-org-completing-read-tags))))


(provide 'init-org-complete)

;;; init-org-complete.el ends here
