;;; init-eshell.el --- init for Eshell

;;; Commentary:



;;; Code:

;;; [ Eshell ] (Emacs Shell)

(use-package eshell
  :ensure t
  :defer t
  :commands (eshell)
  :bind ("C-x !" . eshell)
  :preface (setenv "PAGER" "cat") ; change PAGER from `less' to `cat'.
  :custom ((eshell-visual-subcommands '(("git" "log" "diff" "show")))))

;;; [ eshell-toggle ] -- Show/hide eshell at the bottom of active window with directory of its buffer.

(use-package eshell-toggle
  :ensure t
  :defer t
  :commands (eshell-toggle)
  :bind ("C-x !" . eshell-toggle)
  :custom (eshell-toggle-use-projectile-root nil))

;;; [ eshell-bookmark ] -- Integrate bookmarks with EShell.

(use-package eshell-bookmark
  :ensure t
  :defer t
  :after eshell
  :custom (counsel-bookmark-avoid-dired nil)
  ;; NOTE avoid `eshell-bookmark' open docker-tramp eshell buffer by `counsel'
  ;; with `counsel-find-file' instead of corresponding eshell bookmark buffer.
  :hook (eshell-mode . eshell-bookmark-setup))

;;; [ ob-eshell ]

;; (use-package ob-eshell
;;   :load-path (lambda () (expand-file-name "init/extensions/ob-eshell.el" user-emacs-directory))
;;   :defer t
;;   :commands (org-babel-execute:eshell)
;;   :config
;;   (add-to-list 'org-babel-load-languages '(eshell . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (add-to-list 'org-babel-tangle-lang-exts '("eshell" . "sh")))

;;; [ Aweshell ] -- An enhanced Eshell layer like on-my-zsh with many features.

;; (use-package aweshell
;;   :quelpa (aweshell :fetcher github :repo "manateelazycat/aweshell")
;;   :commands (aweshell-new aweshell-toggle aweshell-sudo-toggle)
;;   :bind ("C-x !" . aweshell-new) ; TODO: ("C-x !" . aweshell-toggle)
;;   :init (setq esh-autosuggest-use-company-map nil))

;;; [ eshell-syntax-highlighting ] -- Syntax highlighting for Eshell.

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :init (eshell-syntax-highlighting-global-mode 1))


(provide 'init-eshell)

;;; init-eshell.el ends here
