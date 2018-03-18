;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ivy ]

(use-package ivy
  :ensure t
  ;; [ ivy-hydra ] -- [C-o], [M-o]
  :ensure ivy-hydra
  :defer t
  :init
  (ivy-mode 1)
  (define-key read-expression-map (kbd "C-r") #'counsel-minibuffer-history) ; in [M-:]
  :config
  (setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
        ivy-virtual-abbreviate 'full ; use 'full to fix bookmarks uniquify issue.
        ivy-height 5
        ivy-fixed-height-minibuffer t
        ivy-display-style 'fancy
        ;; ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil ; remove initial ^ input.
        ;; ivy-extra-directories '() ; remove . and .. directory.
        )
  )

;;; [ ivy-rich ] -- More friendly display transformer for ivy.

(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;;; [ counsel ]

(use-package counsel
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x) ; [M-x]
         ([remap describe-variable] . counsel-describe-variable) ; [C-h v]
         ([remap describe-function] . counsel-describe-function) ; [C-h f]
         ([remap describe-bindings] . counsel-descbinds) ; [C-h b]
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol) ; [C-h S]
         ([remap menu-bar-open] . counsel-tmm) ; [F10] text menu access
         ([remap apropos] . counsel-apropos)
         ;; ("C-s" . counsel-grep-or-swiper)
         ("C-x c p" . counsel-list-processes) ; [C-x c p]
         ("C-x c t" . cancel-function-timers) ; [C-x c t]
         ("C-x c c" . counsel-colors-emacs)
         ("C-x c C" . counsel-colors-web)
         ("C-x c f" . counsel-faces)
         ("C-x RET v" . counsel-set-variable) ; [C-x RET v]
         ("C-x RET u" . counsel-unicode-char) ; [C-x RET u]
         ([remap switch-to-buffer] . ivy-switch-buffer) ; [C-x b]
         ([remap find-file] . counsel-find-file) ; [C-x C-f]
         ("M-`" . counsel-mark-ring) ; [M-`]
         ("M-t" . counsel-git) ; [M-t]
         ("C-c v g g" . counsel-git-grep)
         ([remap grep] . counsel-grep) ; [C-s g]
         ("C-c s C-r" . counsel-rg) ; [ C-c s C-r]
         ([remap yank-pop] . counsel-yank-pop) ; [M-y]
         ;; ("" . counsel-switch-to-shell-buffer) ; switch to a shell buffer, or create one
         ([remap imenu] . counsel-imenu) ; [C-x j]
         ([remap org-goto] . counsel-org-goto) ; [C-c C-j] completion for Org headings
         ;; ( . counsel-org-goto-all) ; completion for Org headings in all open buffers
         ;; ([remap org-set-tags-command] . counsel-org-tag) ; [C-c C-q]
         ;; ([remap org-agenda-set-tags] . counsel-org-tag-agenda) ; [:]
         ;; ([remap org-capture] . counsel-org-capture)
         ;; ("" . counsel-org-file) ; browse all attachments for the current Org file
         ([remap load-library] . counsel-load-library)
         ([remap load-theme] . counsel-load-theme)
         ([remap locate] . counsel-locate)
         ("C-x c #" . counsel-linux-app)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line)
         )
  :init
  (unless (boundp 'search-prefix)
    (define-prefix-command 'search-prefix))
  (define-key search-prefix (kbd "g") 'counsel-grep)
  ;; [C-u] prompt for dir support
  (define-key search-prefix (kbd "M-r") 'counsel-rg)
  ;; (define-key search-prefix (kbd "M-a") 'counsel-ag)
  (define-key search-prefix (kbd "F") 'counsel-fzf)
  :config
  (setq ivy-use-selectable-prompt nil)
  )


(provide 'init-ivy)

;;; init-ivy.el ends here
