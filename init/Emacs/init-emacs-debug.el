;;; init-emacs-debug.el --- init for Emacs debug

;;; Commentary:


;;; Code:

;;; [ debug ] -- Emacs built-in debugger.

;; for Emacs startup freeze debug.
;; (setq debug-on-quit t)
;; (add-hook 'after-init-hook #'(lambda () (setq debug-on-quit nil)))
;; (setq debug-on-error t)
;; (add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

(add-to-list 'display-buffer-alist
             '("^\\*Warnings\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Backtrace\\*" (display-buffer-below-selected)))

;;; [ Edebug ] -- Edebug is a source level debugger.

(use-package edebug
  ;; :bind (:map emacs-lisp-mode-map ("C-c d e" . edebug-mode))
  :init (add-to-list 'display-buffer-alist
                     '("^\\*edebug-trace\\*" (display-buffer-below-selected)))
  :config
  ;; show full edebug value result instead of truncated.
  (setq edebug-print-length 500)
  (setq edebug-print-level  500))

;;; [ edebug-x ] -- Extensions for Edebug.

(use-package edebug-x
  :ensure t
  :defer t
  :commands (edebug-x-mode)
  ;; :init (setq edebug-x-stop-point-overlay t)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*Instrumented Functions\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Edebug Breakpoints\\*" (display-buffer-below-selected))))

;;; [ edebug-inline-result ] -- Show Edebug result inline.

(use-package edebug-inline-result
  :quelpa (edebug-inline-result :fetcher github :repo "stardiviner/edebug-inline-result")
  :init (setq edebug-inline-result-backend 'posframe)
  :config (edebug-inline-result-mode))

;;; [ bug-hunter ] -- Hunt down errors in elisp files.

(use-package bug-hunter
  :ensure t
  :defer t
  :commands (bug-hunter-file bug-hunter-init-file))


(provide 'init-emacs-debug)

;;; init-emacs-debug.el ends here
