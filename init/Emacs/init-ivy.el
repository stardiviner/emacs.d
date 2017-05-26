;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ivy ]

(use-package ivy
  :ensure t
  :defer t
  :bind (
         ;; ("C-c C-r" . ivy-resume) ; [C-c C-r] keybinding conflict with `rtags' prefix.
         ;; ("C-i" . complete-symbol)
         ([remap bookmark-jump] . ivy-bookmark-goto)
         )
  :config
  ;; ivy-mode
  (setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
        ivy-virtual-abbreviate 'full ; use 'full to fix bookmarks uniquify issue.
        ivy-height 5
        ivy-fixed-height-minibuffer nil
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil ; remove initial ^ input.
        ivy-extra-directories '("./" "../") ; remove . and .. directory.
        ivy-wrap nil
        )

  ;; use fuzzy search as default matcher
  ;; (setq ivy-re-builders-alist
  ;;       '((ivy-switch-buffer . ivy--regex-plus)
  ;;         (t . ivy--regexp-fuzzy)))

  (set-face-attribute 'ivy-confirm-face nil
                      :weight 'bold)
  
  ;; [ ivy-hydra ] -- [C-o], [M-o]
  (use-package ivy-hydra
    :ensure t)
  
  ;; {Bookmarks}
  (defun ivy-bookmark-goto ()
    "Open ANY bookmark"
    (interactive)
    (let (bookmarks filename)
      ;; load bookmarks
      (unless (featurep 'bookmark)
        (require 'bookmark))
      (bookmark-maybe-load-default-file)
      (setq bookmarks (and (boundp 'bookmark-alist) bookmark-alist))

      ;; do the real thing
      (ivy-read "bookmarks:"
                (delq nil (mapcar (lambda (bookmark)
                                    (let (key)
                                      (setq key (car bookmark))
                                      (cons key bookmark)))
                                  bookmarks))
                :action (lambda (bookmark)
                          (bookmark-jump bookmark)))
      ))
  
  (ivy-mode 1)
  )

;;; [ ivy-rich ] -- More friendly display transformer for ivy.

(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  )

;;; [ all-the-icons-ivy ] -- Ivy/Counsel integration with all-the-icons.el

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :config
;;   (all-the-icons-ivy-setup)
;;   )

;;; [ counsel ]

(use-package counsel
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x) ; [M-x]
         ([remap describe-variable] . counsel-describe-variable) ; [C-h v]
         ([remap describe-function] . counsel-describe-function) ; [C-h f]
         ([remap describe-bindings] . counsel-descbinds) ; [C-h b]
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol) ; [C-h S]
         ([remap menu-bar-open] . counsel-tmm) ; [F10] text menu access
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
         ([remap imenu] . counsel-imenu)
         ("C-x j" . counsel-imenu)
         ;; ([remap org-set-tags-command] . counsel-org-tag) ; [C-c C-q]
         ;; ([remap org-agenda-set-tags] . counsel-org-tag-agenda) ; [:]
         ([remap load-library] . counsel-load-library)
         ([remap load-theme] . counsel-load-theme)
         ([remap locate] . counsel-locate)
         ("C-x c #" . counsel-linux-app)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line)
         )

  :init
  (unless (boundp 'my-search-prefix)
    (define-prefix-command 'my-search-prefix))
  (global-set-key (kbd "C-c s") 'my-search-prefix)
  (define-key my-search-prefix (kbd "g") 'counsel-grep)
  (define-key my-search-prefix (kbd "G") 'counsel-ag) ; [C-u] prompt for dir support
  :config
  ;; (setq ivy-switch-buffer-show-info '("%s" "buffer-name"))
  (setq counsel-yank-pop-truncate t)
  )


;;; [ counsel-projectile ] -- Ivy integration for Projectile.

;; (use-package counsel-projectile
;;   :ensure t
;;   :defer t
;;   :commands counsel-projectile
;;   :bind (:map projectile-command-map
;;               ("p". counsel-projectile))
;;   )


(provide 'init-ivy)

;;; init-ivy.el ends here
