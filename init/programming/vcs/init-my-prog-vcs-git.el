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
  :ensure t)


;;; [ Magit ]

(use-package magit
  :ensure t
  :bind (:map my-prog-vcs-map
              ("v" . magit-status)
              ("l" . magit-list-repositories))
  :config
  ;; Git WIP (work in progress) in Magit
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; Performance
  ;; (setq magit-refresh-status-buffer nil)
  ;; (setq auto-revert-buffer-list-filter
  ;;       'magit-auto-revert-repository-buffers-p)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  
  (setq magit-repository-directories
        `((,user-emacs-directory . 0)
          ("~/Code/" . 2)
          ;; ("~/Code/Emacs/" . 2)
          ;; ("~/Code/Clojure/" . 2)
          ))
  
  ;; let magit status buffer display in current window.
  (setq magit-display-buffer-function 'display-buffer)

  ;; show gravatar in Magit revision.
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  (add-to-list 'display-buffer-alist
               '("\\`\\*magit:.*\\'"
                 (display-buffer-reuse-window
                  display-buffer-same-window)
                 ))

  ;; keybindings
  (define-key my-prog-vcs-git-map (kbd "F") 'magit-log-buffer-file)
  (define-key my-prog-vcs-git-map (kbd "b") 'magit-blame-popup)
  (define-key my-prog-vcs-git-map (kbd "v") 'magit-status)
  (define-key my-prog-vcs-git-map (kbd "s") 'magit-stage)
  (define-key my-prog-vcs-git-map (kbd "c") 'magit-commit)
  (define-key my-prog-vcs-git-map (kbd "C") 'magit-commit-amend)
  (define-key my-prog-vcs-git-map (kbd "d") 'magit-diff)
  (define-key my-prog-vcs-git-map (kbd "l") 'magit-log)
  (defalias 'magit-log-region 'magit-log-buffer-file)
  (define-key my-prog-vcs-git-map (kbd "L") 'magit-log-region)
  (define-key my-prog-vcs-git-map (kbd "o") 'magit-checkout)
  (define-key my-prog-vcs-git-map (kbd "M-b") 'magit-bisect)
  (define-key my-prog-vcs-git-map (kbd "B") 'magit-blame)
  (define-key my-prog-vcs-git-map (kbd "f") 'magit-file-popup)
  
  ;; enable ispell words complete in commit message buffer.
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends '(company-flyspell :with company-ispell))
              (add-to-list 'company-backends 'company-emoji)))
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
        git-messenger:use-magit-popup t
        )
  
  ;; enable `magit-commit-mode' after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  )

;;; [ git-timemachine ] -- time-machine of Git revisions.

(use-package git-timemachine
  :ensure t)

;;; [ magit-tbdiff ] -- topic branch interdiff

(use-package magit-tbdiff
  :ensure t)

;;; [ gited ] -- operate on Git branches like Dired.

(use-package gited
  :ensure t
  :bind (:map my-prog-vcs-git-map
              ("b" . gited-list-branches)
              :map dired-mode-map
              ("C-x C-g" . gited-list-branches))
  )

;;; [ magit-lfs ] -- Magit support for GLFS: Git Large File System

(use-package magit-lfs
  :ensure t)

;;; [ magit-stgit ] -- StGit extension for Magit

;; (use-package magit-stgit
;;   :ensure t
;;   :config
;;   (add-hook 'magit-mode-hook 'magit-stgit-mode)
;;   )

;;; [ magit-topgit ] -- TopGit extension for Magit.

;; (use-package magit-topgit
;;   :ensure t
;;   :config
;;   (add-hook 'magit-mode-hook 'magit-topgit-mode)
;;   )

;;; [ projectile-git-autofetch ] -- Automatically fetch git repositories known to projectile.

;; (use-package projectile-git-autofetch
;;   :ensure t
;;   :config
;;   ;; (projectile-git-autofetch-mode 1)
;;   )



(provide 'init-my-prog-vcs-git)

;;; init-my-prog-vcs-git.el ends here
