;;; init-emacs-debug.el --- init for Emacs debug

;;; Commentary:


;;; Code:

;;; [ debug ] -- Emacs built-in debugger.

(add-to-list 'display-buffer-alist '("^\\*Warnings\\*" . (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist '("^\\*Backtrace\\*" . (display-buffer-below-selected)))

;;; [ Edebug ] -- Edebug is a source level debugger.

(use-package edebug
  ;; show full edebug value result instead of truncated.
  :custom ((edebug-print-length 500)
           (edebug-print-level  500))
  ;; :bind (:map emacs-lisp-mode-map ("C-c d e" . edebug-mode))
  :init (add-to-list 'display-buffer-alist '("^\\*edebug-trace\\*" . (display-buffer-below-selected))))

;;; [ edebug-x ] -- Extensions for Edebug.

(use-package edebug-x
  :ensure t
  :defer t
  :commands (edebug-x-mode)
  ;; :custom (edebug-x-stop-point-overlay t)
  :init
  (add-to-list 'display-buffer-alist '("^\\*Instrumented Functions\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^\\*Edebug Breakpoints\\*" . (display-buffer-below-selected))))

;;; [ edebug-inline-result ] -- Show Edebug result inline.

(use-package edebug-inline-result
  :ensure t
  :defer t
  :custom (edebug-inline-result-backend 'posframe)
  :hook (edebug-mode . edebug-inline-result-mode))

;;; [ bug-hunter ] -- Hunt down errors in elisp files.

(use-package bug-hunter
  :ensure t
  :defer t
  :commands (bug-hunter-file bug-hunter-init-file))

;;; print and insert Emacs version info
(defun emacs-version-commit-insert ()
  "Insert version of Emacs and 7 characters of the commit hash."
  (interactive)
  (insert
   (format "GNU Emacs %s (commit %s)"
	       emacs-version (substring emacs-repository-version 0 7))))


(provide 'init-emacs-debug)

;;; init-emacs-debug.el ends here
