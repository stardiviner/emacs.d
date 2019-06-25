;;; init-emacs-search-wgrep.el --- init for wgrep.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; writable grep or other search engine result buffer which can apply changes to files.

;;; Code:

;;; [ wgrep ] -- writable grep buffer and apply the changes to files.

(use-package wgrep
  :ensure t
  :defer t
  :init (setq wgrep-enable-key (kbd "C-c C-p")
              wgrep-auto-save-buffer nil)
  :config (advice-add 'wgrep-finish-edit :after #'wgrep-save-all-buffers))

;;; [ wgrep-helm ] -- Writable helm-grep-mode buffer and apply the changes to files.

;; (use-package wgrep-helm
;;   :ensure t
;;   :defer t
;;   ;; :config
;;   ;; support wgrep for ripgrep.
;;   ;; [M-x `helm-do-grep-ag'] `ripgrep' is fully supported with `wgrep' when used as backend for `helm'.
;;   ;; You have first [C-x C-s] to save your helm grep buffer before being able to edit it with wgrep.
;;   ;; (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
;;   )


(provide 'init-emacs-search-wgrep)

;;; init-emacs-search-wgrep.el ends here
