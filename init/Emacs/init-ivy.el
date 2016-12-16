;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ivy ]

(use-package ivy
  :ensure t
  :defer t
  :bind (("C-c C-r" . ivy-resume)
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
                      :inherit nil
                      :foreground "black" :background "khaki"
                      :weight 'bold)
  (set-face-attribute 'ivy-current-match nil
                      :inherit nil
                      ;; :foreground "white" :background "#004A5D"
                      ;; :weight 'normal :box nil
                      ;; Sci-Fi style
                      :foreground "white" :background "#004A5D"
                      :box '(:color "cyan" :line-width -1)
                      )
  (set-face-attribute 'ivy-match-required-face nil
                      :inherit 'minibuffer-prompt
                      :foreground "dark red" :background nil
                      :weight 'bold :box nil
                      )
  ;; the string between matches
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                      :inherit nil
                      :foreground nil :background nil
                      :weight 'normal :box nil
                      )
  ;; first match part.
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "green" :background nil
                      :weight 'normal :box nil :underline t
                      )
  ;; second match part.
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "orange" :background nil
                      :weight 'normal :box nil :underline t
                      )
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "dodger blue" :background nil
                      :weight 'normal :box nil :underline t
                      )

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

;;; [ swiper ]

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper)
         ("C-c u" . swiper-all))
  )


;;; [ counsel ]

(use-package counsel
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper)
         ([remap execute-extended-command] . counsel-M-x) ; [M-x]
         ([remap describe-variable] . counsel-describe-variable) ; [C-h v]
         ([remap describe-function] . counsel-describe-function) ; [C-h f]
         ([remap describe-bindings] . counsel-descbinds) ; [C-h b]
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol) ; [C-h S]
         ([remap menu-bar-open] . counsel-tmm) ; [F10] text menu access
         ("C-x c p" . counsel-list-processes) ; [C-x c p]
         ("C-x c t" . cancel-function-timers)
         ("C-x RET v" . counsel-set-variable) ; [C-x RET v]
         ("C-x RET u" . counsel-unicode-char) ; [C-x RET u]
         ([remap switch-to-buffer] . ivy-switch-buffer) ; [C-x b]
         ([remap find-file] . counsel-find-file) ; [C-x C-f]
         ("M-t" . counsel-git) ; [M-t]
         ("C-c v g g" . counsel-git-grep)
         ([remap grep] . counsel-grep) ; [C-s g]
         ([remap yank-pop] . counsel-yank-pop) ; [M-y]
         ([remap imenu] . counsel-imenu)
         ("C-x j" . counsel-imenu)
         ([remap org-set-tags-command] . counsel-org-tag) ; [C-c C-q]
         ([remap org-agenda-set-tags] . counsel-org-tag-agenda) ; [:]
         ([remap load-library] . counsel-load-library)
         ([remap load-theme] . counsel-load-theme)
         ([remap locate] . counsel-locate)
         ("C-c #" . counsel-linux-app)
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
