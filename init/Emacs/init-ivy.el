;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ivy ]

(use-package ivy
  :ensure t
  :defer t
  :config
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
  ;;         (swiper . ivy--regexp-fuzzy)
  ;;         (t . ivy--regexp-fuzzy)))
  ;; or more programmatically:
  ;; (with-eval-after-load 'ivy
  ;;   (push (cons t #'ivy--regex-plus) ivy-re-builders-alist)
  ;;   (push (cons #'swiper (cdr (assq t ivy-re-builders-alist))) ivy-re-builders-alist))

  (define-key read-expression-map (kbd "C-r") #'counsel-expression-history) ; in [M-:]
  
  ;; [ ivy-hydra ] -- [C-o], [M-o]
  (use-package ivy-hydra
    :ensure t)

  ;; some actions
  (ivy-set-actions
   'counsel-find-file
   '(("d" (lambda (x) (delete-file (expand-file-name x)))
      "delete")
     ))
  (ivy-set-actions
   'ivy-switch-buffer
   '(("k" (lambda (x) (kill-buffer x) (ivy--reset-state ivy-last))
      "kill")
     ("j" ivy--switch-buffer-other-window-action
      "other window")
     ))
  
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

  (global-set-key [remap bookmark-jump] 'ivy-bookmark-goto)
  )

;;; [ ivy-rich ] -- More friendly display transformer for ivy.

(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

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
         ([remap imenu] . counsel-imenu)
         ("C-x j" . counsel-imenu)
         ;; ("" . counsel-switch-to-shell-buffer) ; switch to a shell buffer, or create one
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
  (define-key search-prefix (kbd "a") 'counsel-ag) ; [C-u] prompt for dir support
  (define-key search-prefix (kbd "F") 'counsel-fzf)
  :config
  (setq ivy-use-selectable-prompt t)
  ;; (setq ivy-switch-buffer-show-info '("%s" "buffer-name"))
  (setq counsel-yank-pop-truncate t)
  )


;;; [ Swpier ] -- gives you an overview as you search for a regex.

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))


(provide 'init-ivy)

;;; init-ivy.el ends here
