;;; init-my-prog-sidebar.el --- init Emacs sidebar for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ECB ]

;; (use-package ecb
;;   :ensure t
;;   :init
;;   (unless (boundp 'ECB-prefix)
;;     (define-prefix-command 'ECB-prefix))
;;   (global-set-key (kbd "C-x C-;") 'ECB-prefix)
;;   :bind (:map ECB-prefix
;;               ;; activate and deactivate ECB
;;               (";" . ecb-activate)
;;               ("'" . ecb-deactivate)
;;               ;; show/hide ECB window
;;               ("C-;" . ecb-show-ecb-windows)
;;               ("C-'" . ecb-hide-ecb-windows)
;;               ;; quick navigation between ECB windows
;;               ;; ("" . ecb-goto-window-edit1)
;;               ;; ("" . ecb-goto-window-directories)
;;               ;; ("" . ecb-goto-window-sources)
;;               ;; ("" . ecb-goto-window-methods)
;;               ;; ("" . ecb-goto-window-compilation)
;;               )
;;   :config
;;   (setq ecb-show-sources-in-directories-buffer 'always
;;         ecb-compile-window-height 12)
;;   )

;;; [ speedbar ]

;; (use-package speedbar
;;   :ensure t
;;   :config
;;   (speedbar-mode 1))

;;; [ Sr-Speedbar ] -- SrSpeedbar is mode make SpeedBar show in Current Frame

(use-package sr-speedbar
  :ensure t
  :config
  (setq sr-speedbar-width 30
        sr-speedbar-max-width 30
        sr-speedbar-delete-windows nil
        sr-speedbar-skip-other-window-p nil
        sr-speedbar-auto-refresh t
        sr-speedbar-right-side nil
        )
  
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (linum-mode -1)))

  (defun my-sr-speedbar-toggle-and-switch ()
    "Toggle sr-speedbar or switch to sr-speedbar window if already opened."
    (interactive)
    (if (sr-speedbar-exist-p)
        (if (equal (buffer-name (current-buffer)) sr-speedbar-buffer-name) ; if currently in the sr-speedbar window.
            (sr-speedbar-close)
          (sr-speedbar-select-window))
      (sr-speedbar-open)
      ;; (other-window 1) ; switch back to previous window.
      ))
  )

;;; [ emacs-neotree ]

(use-package neotree
  :ensure t
  ;; :init
  ;; (add-hook 'after-init-hook 'neotree-show)
  :config
  (setq neo-window-position 'left
        neo-window-width 25
        neo-window-fixed-size t
        neo-smart-open t
        neo-auto-indent-point t
        neo-vc-integration nil
        )
  (setq neo-theme (if (and (featurep 'all-the-icons) (display-graphic-p)) 'icons 'classic))
  )

;;; [ project-explorer ] -- A project explorer sidebar.

;; (use-package project-explorer
;;   :ensure t
;;   :config
;;   (setq pe/side 'left
;;         pe/width 30
;;         ;; pe/project-root-function
;;         ;; pe/directory-tree-function
;;         ;; pe/cache-enabled
;;         ;; pe/omit-regexp
;;         )
;;   )



;; (global-set-key [f8] 'sr-speedbar-toggle)
;; (global-set-key [f8] 'my-sr-speedbar-toggle-and-switch)
;; (global-set-key [f8] 'project-explorer-toggle)
(global-set-key [f8] 'neotree-toggle)

;;; [ treemacs ] -- a file and project explorer in the same vein as NeoTree or Vim’s NerdTree.

(use-package treemacs
  :ensure t
  :bind ("[f8]" . treemacs-toggle)
  :config
  (setq treemacs-header-function            #'treemacs--create-header-projectile
        treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil)

  ;; (treemacs-follow-mode t)
  ;; (treemacs-filewatch-mode t)
  )



(provide 'init-my-prog-sidebar)

;;; init-my-prog-sidebar.el ends here
