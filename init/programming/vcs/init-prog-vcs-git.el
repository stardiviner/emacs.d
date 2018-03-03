;;; init-prog-vcs-git.el --- init Git for Emacs
;;
;;; Commentary:

;;; Code:


(unless (boundp 'prog-vcs-git-prefix)
  (define-prefix-command 'prog-vcs-git-prefix))
(define-key 'prog-vcs-prefix (kbd "g") 'prog-vcs-git-prefix)


;; [ git-modes ] -- front end wrapper for vc-git.

;; (use-package gitconfig-mode
;;   :ensure t
;;   :mode ("\\.gitconfig\\'" . gitconfig-mode))
;; (use-package gitattributes-mode
;;   :ensure t)
;; (use-package gitignore-mode
;;   :ensure t
;;   :mode ("\\.gitignore\\'" . gitignore-mode))
;; (use-package git-commit
;;   :ensure t)

;;; `company-dabbrev' in git commit buffer.
;; https://github.com/company-mode/company-mode/issues/704
(defun my--company-dabbrev-ignore-except-magit-diff (buffer)
  (let ((name (buffer-name)))
    (and (string-match-p "\\`[ *]" name)
         (not (string-match-p "\\*magit-diff:" name)))))

(defun my--git-commit-setup-hook ()
  (setq-local fill-column 72)
  (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
  (setq-local company-dabbrev-ignore-buffers
              #'my--company-dabbrev-ignore-except-magit-diff)
  (setq company-dabbrev-code-other-buffers 'all))

(add-hook 'git-commit-setup-hook #'my--git-commit-setup-hook)

;;; [ Magit ]

(use-package magit
  :ensure t
  :defer t
  :ensure-system-package git
  :bind (:map prog-vcs-prefix
              ("v" . magit-status)
              ("l" . magit-list-repositories))
  :config
  ;; Git WIP (work in progress) in Magit
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; Performance
  ;; (setq magit-refresh-status-buffer nil)
  ;; (setq auto-revert-buffer-list-filter
  ;;       'magit-auto-revert-repository-buffers-p)
  
  (setq magit-repository-directories
        `((,user-emacs-directory . 0)
          ("~/Code/" . 2)
          ;; ("~/Code/Emacs/" . 2)
          ;; ("~/Code/Clojure/" . 2)
          ))
  
  ;; let magit status buffer display in current window.
  (setq magit-display-buffer-function 'display-buffer)

  ;; show gravatar in Magit revision.
  ;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; keybindings
  (define-key prog-vcs-git-prefix (kbd "F") 'magit-log-buffer-file)
  (define-key prog-vcs-git-prefix (kbd "b") 'magit-blame-popup)
  (define-key prog-vcs-git-prefix (kbd "v") 'magit-status)
  (define-key prog-vcs-git-prefix (kbd "s") 'magit-stage)
  (define-key prog-vcs-git-prefix (kbd "c") 'magit-commit)
  (define-key prog-vcs-git-prefix (kbd "C") 'magit-commit-amend)
  (define-key prog-vcs-git-prefix (kbd "d") 'magit-diff)
  (define-key prog-vcs-git-prefix (kbd "l") 'magit-log)
  (defalias 'magit-log-region 'magit-log-buffer-file)
  (define-key prog-vcs-git-prefix (kbd "r") 'magit-log-region)
  (define-key prog-vcs-git-prefix (kbd "o") 'magit-checkout)
  (define-key prog-vcs-git-prefix (kbd "M-b") 'magit-bisect)
  (define-key prog-vcs-git-prefix (kbd "B") 'magit-blame)
  (define-key prog-vcs-git-prefix (kbd "f") 'magit-file-popup)
  
  ;; enable ispell words complete in commit message buffer.
  (add-hook 'git-commit-setup-hook
            (lambda ()
              ;; company-flyspell + company-ispell
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-ispell-dict)
              (if (featurep 'company-emoji)
                  (add-to-list 'company-backends 'company-emoji))
              ;; company-abbrev
              (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
              (setq-local company-dabbrev-code-other-buffers 'all)
              (setq-local company-dabbrev-ignore-buffers #'my--company-dabbrev-ignore-except-magit-diff)
              ))

  ;; manage popup buffers.
  (add-to-list 'display-buffer-alist
               '("\\`magit:.*\\'" (display-buffer-reuse-window display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("^magit-diff.*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^magit-revision.*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^magit-log.*" (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("^magit-process.*" (display-buffer-same-window)))
  )

;;; [ magit-find-file ]

;; (use-package magit-find-file
;;   :ensure t
;;   :bind ("M-t" . magit-find-file)
;;   )


;;; [ magit-gitflow ] -- Git Flow plugin for magit

(use-package magit-gitflow
  :ensure t
  :after magit
  :defer t
  :init
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
  :config
  ;; TODO: check out the original Issue on GitHub.
  (magit-define-popup-switch 'magit-gitflow-release-finish-popup ?p
    "Push after finish" "--push" t)
  )

;;; [ magit-p4 ] -- Magit plugin integrating git-p4 add-on.

;; (use-package magit-p4
;;   :ensure t)


;;; [ git-messenger ] -- popup commit message at current line.

(use-package git-messenger
  :ensure t
  :defer t
  :bind (:map prog-vcs-prefix
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

;; (use-package git-timemachine
;;   :ensure t)

;;; [ magit-tbdiff ] -- topic branch interdiff

;; (use-package magit-tbdiff
;;   :ensure t)

;;; [ magit-lfs ] -- Magit support for GLFS: Git Large File System

;; (use-package magit-lfs
;;   :ensure t)

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

;;; [ pcmpl-git ] -- Complete both git commands and their options and arguments.

;; (use-package pcmpl-git
;;   :ensure t)



(provide 'init-prog-vcs-git)

;;; init-prog-vcs-git.el ends here
