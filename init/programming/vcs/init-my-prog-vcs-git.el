;;; init-my-prog-vcs-git.el --- init Git for Emacs
;;
;;; Commentary:

;;; Code:


(unless (boundp 'my-prog-vcs-git-map)
  (define-prefix-command 'my-prog-vcs-git-map))
(define-key 'my-prog-vcs-map (kbd "g") 'my-prog-vcs-git-map)


;; [ git-modes ] -- front end wrapper for vc-git.

(use-package gitconfig-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode)))
(use-package gitattributes-mode
  :ensure t
  :defer t)
(use-package gitignore-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode)))
(use-package git-commit
  :ensure t
  :config
  (set-face-attribute 'git-commit-summary nil
                      :foreground "black"
                      )
  )


;;; [ Magit ]

(use-package magit
  :ensure t
  :bind (:map my-prog-vcs-map
              ("v" . magit-status))
  :config
  ;; let magit status buffer display in current window.
  (setq magit-display-buffer-function 'display-buffer)

  (add-to-list 'display-buffer-alist
               '("\\`\\*magit:.*\\'"
                 (display-buffer-reuse-window
                  display-buffer-same-window)
                 ))

  ;; Magit Faces

  ;; file
  (set-face-attribute 'magit-diff-file-heading nil
                      :weight 'normal)
  (set-face-attribute 'magit-diff-file-heading-highlight nil ; current diff file headings.
                      :weight 'normal)
  (set-face-attribute 'magit-diff-file-heading-selection nil ; current select region
                      :weight 'normal)

  ;; section
  (set-face-attribute 'magit-section-heading nil
                      :foreground "sky blue" :background "#222222"
                      :box '(:color "cyan" :line-width 1)
                      :weight 'bold)
  (set-face-attribute 'magit-section-highlight nil ; current section (current selected thing)
                      :background (color-darken-name (face-background 'default) 3)
                      ;; :box '(:color "black" :line-width -1)
                      )

  ;; branch
  (set-face-attribute 'magit-branch-local nil
                      :foreground "#8BEEFF" :background "#1B8194"
                      :box '(:color "#8BEEFF" :line-width -1)
                      :weight 'normal)
  (set-face-attribute 'magit-branch-remote nil
                      :foreground "#95CA2A" :background "#4F6827"
                      :box '(:color "#95CA2A" :line-width -1)
                      :weight 'normal)

  ;; hash
  (set-face-attribute 'magit-hash nil
                      :foreground "orange")

  ;; signature
  (set-face-attribute 'magit-signature-untrusted nil
                      :foreground "dark green")

  ;; diff colors
  (set-face-attribute 'magit-diff-added nil
                      :background "#ddffdd"
                      :foreground "#22aa22")
  (set-face-attribute 'magit-diff-removed nil
                      :background "#ffdddd"
                      :foreground "#aa2222")
  (set-face-attribute 'magit-diff-our nil
                      :background "#ffffcc"
                      :foreground "#aaaa11")
  (set-face-attribute 'magit-diff-base nil
                      :background "#ffffcc"
                      :foreground "#aaaa11")
  (set-face-attribute 'magit-diff-their nil
                      :background "#ffffcc"
                      :foreground "#aaaa11")

  ;; (set-face-attribute 'magit-diff-removed-highlight nil
  ;;                     )
  ;; (set-face-attribute 'magit-diff-added-highlight nil
  ;;                     )
  ;; (set-face-attribute 'magit-diff-our-highlight nil
  ;;                     )
  ;; (set-face-attribute 'magit-diff-base-highlight nil
  ;;                     )
  ;; (set-face-attribute 'magit-diff-their-highlight nil
  ;;                     )

  ;; keybindings
  (define-key my-prog-vcs-git-map (kbd "F") 'magit-log-buffer-file)
  (define-key my-prog-vcs-git-map (kbd "b") 'magit-blame-popup)
  (define-key my-prog-vcs-git-map (kbd "v") 'magit-status)
  (define-key my-prog-vcs-git-map (kbd "s") 'magit-stage)
  (define-key my-prog-vcs-git-map (kbd "c") 'magit-commit)
  (define-key my-prog-vcs-git-map (kbd "C") 'magit-commit-amend)
  (define-key my-prog-vcs-git-map (kbd "d") 'magit-diff)
  (define-key my-prog-vcs-git-map (kbd "l") 'magit-log)
  (define-key my-prog-vcs-git-map (kbd "L") 'magit-log-buffer-file) ; show log for current buffer visiting file.
  (define-key my-prog-vcs-git-map (kbd "o") 'magit-checkout)
  (define-key my-prog-vcs-git-map (kbd "B") 'magit-bisect)
  (define-key my-prog-vcs-git-map (kbd "b") 'magit-blame)
  (define-key my-prog-vcs-git-map (kbd "f") 'magit-file-popup)
  
  ;; enable ispell words complete in commit message buffer.
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-ispell)))
  )


;;; [ with-editor ]

(use-package with-editor
  :ensure t
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-mode-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)

  ;; Some variants of this function exist, these two forms are
  ;; equivalent:

  ;; (add-hook 'shell-mode-hook
  ;;           (apply-partially 'with-editor-export-editor "GIT_EDITOR"))
  ;; (add-hook 'shell-mode-hook 'with-editor-export-git-editor)
  )


;;; [ magit-find-file ]

;; (use-package magit-find-file
;;   :ensure t
;;   :bind ("M-t" . magit-find-file)
;;   )


;;; [ magit-gitflow ] -- Git Flow plugin for magit

(use-package magit-gitflow
  :ensure t
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

;;; [ magit-p4 ] -- Magit plugin integrating git-p4 add-on.

(use-package magit-p4
  :ensure t)


;;; [ git-messenger ] -- popup commit message at current line.

(use-package git-messenger
  :ensure t
  :bind (:map my-prog-vcs-map
              ("m m" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message)
              ("c" . git-messenger:copy-message)
              )
  :config
  (setq git-messenger:show-detail t ; always show detail message.
        ;; git-messenger:handled-backends '(git svn)
        )
  
  ;; enable `magit-commit-mode' after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )

;;; [ git-timemachine ] -- time-machine of Git revisions.

(use-package git-timemachine
  :ensure t)

;;; [ projectile-git-autofetch ] -- Automatically fetch git repositories known to projectile.

;; (use-package projectile-git-autofetch
;;   :ensure t
;;   :config
;;   ;; (projectile-git-autofetch-mode 1)
;;   )


(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
