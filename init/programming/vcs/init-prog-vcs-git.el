;;; init-prog-vcs-git.el --- init Git for Emacs
;;
;;; Commentary:

;;; Code:


(unless (boundp 'prog-vcs-git-prefix)
  (define-prefix-command 'prog-vcs-git-prefix))
(define-key 'prog-vcs-prefix (kbd "g") 'prog-vcs-git-prefix)


;; [ Git modes ] -- front end wrapper for vc-git.

(use-package gitconfig-mode
  :ensure t
  :defer t
  :mode ("\\.gitconfig\\'" . gitconfig-mode))
(use-package gitattributes-mode
  :ensure t
  :defer t)
(use-package gitignore-mode
  :ensure t
  :defer t
  :mode ("\\.gitignore\\'" . gitignore-mode))

;;; [ gitignore-templates ] -- Access GitHub .gitignore templates.

(use-package gitignore-templates
  :ensure t
  :defer t
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :preface
  (unless (boundp 'gitignore-template-prefix)
    (define-prefix-command 'gitignore-template-prefix))
  (define-key prog-vcs-prefix (kbd "t") 'gitignore-template-prefix)
  :bind (:map gitignore-template-prefix
              ("t" . gitignore-templates-insert)
              ("n" . gitignore-templates-new-file)))

(use-package git-commit ; edit Git commit messages.
  :ensure t
  :defer t
  :config
  ;; `company-dabbrev' in git commit buffer.
  ;; https://github.com/company-mode/company-mode/issues/704
  (defun my/company-dabbrev-ignore-except-magit-diff (buffer)
    (let ((name (buffer-name)))
      (and (string-match-p "\\`[ *]" name)
           (not (string-match-p "\\*magit-diff:" name)))))
  (defun my:git-commit-setup-hook ()
    (setq-local fill-column 72)
    (auto-fill-mode t)
    (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
    (setq-local company-dabbrev-ignore-buffers
                #'my/company-dabbrev-ignore-except-magit-diff)
    (setq company-dabbrev-code-other-buffers 'all)
    (flyspell-mode 1)
    (setq-local company-backends '(company-dabbrev-code
                                   ;; :with company-abbrev                  
                                   :separate company-ispell)))
  (add-hook 'git-commit-setup-hook #'my:git-commit-setup-hook))

;;; [ Magit ]

(use-package magit
  :ensure t
  :defer t
  :preface (defalias 'magit-log-region 'magit-log-buffer-file)
  :commands (magit-status)
  :bind (:map prog-vcs-prefix
              ("v" . magit-status)
              ("l" . magit-list-repositories)
              
              :map prog-vcs-git-prefix
              ("F" . magit-log-buffer-file)
              ("r" . magit-log-region)
              ("b" . magit-blame-popup)
              ("v" . magit-status)
              ("s" . magit-stage)
              ("c" . magit-commit-create)
              ("C" . magit-commit-amend)
              ("d" . magit-diff)
              ("l" . magit-log)
              ("o" . magit-checkout)
              ("M-b" . magit-bisect)
              ("B" . magit-blame)
              ("f" . magit-file-popup))
  :custom ((magit-clone-default-directory (expand-file-name "~/Code"))
           (magit-repository-directories `((,user-emacs-directory . 0)
                                           ("~/Code/" . 3)
                                           ("~/Org/Website" . 1)))
           ;; let magit status buffer display in current window.
           (magit-display-buffer-function 'display-buffer)
           ;; use toggled arguments. For example, "-c" (--color) for log.
           (magit-prefix-use-buffer-arguments 'always)
           (magit-diff-refine-hunk t)   ; `magit-diff-toggle-refine-hunk'
           )
  :init
  ;; manage popup buffers.
  (add-to-list 'display-buffer-alist '("\\`magit:.*\\'" . (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist '("^magit-diff.*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^magit-revision.*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("^magit-log.*" . (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist '("^magit-process.*" . (display-buffer-same-window))))

;;; [ magit-pretty-graph ] -- A prettier graph for Magit drawn in Emacs.

(use-package magit-pretty-graph
  :quelpa (magit-pretty-graph :fetcher github :repo "georgek/magit-pretty-graph")
  :defer t
  :commands (magit-pg-repo)
  :init (add-to-list 'display-buffer-alist '("^\\*magit-prettier-graph\\*" . (display-buffer-below-selected))))

;;; [ magit-delta ] -- Use Delta when displaying diffs in Magit.

;; (use-package magit-delta
;;   :ensure t
;;   :init (magit-delta-mode 1))

;;; [ magit-gitflow ] -- Git Flow plugin for magit

(use-package magit-gitflow
  :ensure t
  :defer t
  :after magit
  :hook (magit-status-mode . turn-on-magit-gitflow))

;;; [ magit-p4 ] -- Magit plugin integrating git-p4 add-on.

;; (use-package magit-p4
;;   :ensure t)

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
;;   :defer t
;;   :init (add-hook 'magit-mode-hook 'magit-stgit-mode))

;;; [ magit-topgit ] -- TopGit extension for Magit.

;; (use-package magit-topgit
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'magit-mode-hook 'magit-topgit-mode))

;;; [ pcmpl-git ] -- Complete both git commands and their options and arguments.

;; (use-package pcmpl-git
;;   :ensure t)

;;; [ magit-todos ] -- Show source file TODOs in Magit.

;; (use-package magit-todos
;;   :ensure t
;;   :defer t
;;   :init (magit-todos-mode 1))

;;; [ magit-org-todos ] -- Display file "todo.org" (in project root path) to your Magit status section.

(use-package magit-org-todos
  :ensure t
  :ensure projectile
  :defer t
  :hook (magit-status-mode . magit-org-todos-autoinsert)
  :commands (magit-org-todos--magit-visit-org-todo)
  :init
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "C-o")  'magit-org-todos--magit-visit-org-todo)))

;;; [ forge ] -- Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.

(use-package forge
  :ensure t
  :defer t
  :after magit)

;;; [ magit-reviewboard ] -- integrate the ReviewBoard review software into magit.

;; (use-package magit-reviewboard
;;   :ensure t
;;   :commands (magit-reviewboard-mode magit-reviewboard-list)
;;   :custom
;;   (magit-reviewboard-base-uri
;;    "https://reviews.reviewboard.org/api"
;;    "Set Reviewboard api to point to official reviewboard server"))

;;; [ flower ] -- Emacs task tracker client. Integration with Github, Gitlab, Atlassian Jira and Slack etc.

;; (use-package flower
;;   :ensure t
;;   :defer t
;;   :hook (org-mode . flower-mode)
;;   :commands (flower-open
;;              flower-org-show-task-info
;;              flower-list-tasks flower-show-task-info)
;;   :custom (flower-tracker-queries [("https://github.com/stardiviner/arduino-mode" nil nil)
;;                                    ("https://github.com/stardiviner/kiwix.el" nil nil)])
;;   :init (add-to-list 'display-buffer-alist '("^\\*flower\\*" . (display-buffer-below-selected))))

;;==============================================================================

(unless (boundp 'git-quick-prefix)
  (define-prefix-command 'git-quick-prefix))
(global-set-key (kbd "M-g g") 'git-quick-prefix)

;; [ git-gutter+ ] -- Manage Git hunks straight from the buffer.

(use-package git-gutter+
  :ensure t
  :defer t
  :delight git-gutter+-mode
  :preface (setq git-gutter+-disabled-modes '(asm-mode image-mode))
  :commands (global-git-gutter+-mode git-gutter+-turn-on)
  :hook (prog-mode . git-gutter+-turn-on)
  :bind (:map git-quick-prefix
              ("t" . git-gutter+-mode) ; Turn on/off in the current buffer
              ("T" . global-git-gutter+-mode) ; Turn on/off globally
              ;; jump between hunks
              ("n" . git-gutter+-next-hunk)
              ("p" . git-gutter+-previous-hunk)
              ;; actions on hunks
              ("d" . git-gutter+-show-hunk-inline-at-point)
              ("D" . git-gutter+-show-hunk) ; diff
              ("=" . git-gutter+-popup-hunk)
              ("r" . git-gutter+-revert-hunk)
              ;; stage hunk at point
              ;; if region is active, stage all hunk lines within the region.
              ("s" . git-gutter+-stage-hunks)
              ("c" . magit-commit-create)
              ("C" . git-gutter+-stage-and-commit)
              ("u" . git-gutter:update-all-windows)
              
              :map prog-vcs-prefix
              ("m t" . git-gutter+-mode) ; Turn on/off in the current buffer
              ("m T" . global-git-gutter+-mode) ; Turn on/off globally
              ;; jump between hunks
              ("m n" . git-gutter+-next-hunk)
              ("m p" . git-gutter+-previous-hunk)
              ;; actions on hunks
              ("m d" . git-gutter+-show-hunk-inline-at-point)
              ("m =" . git-gutter+-show-hunk) ; diff
              ("m D" . git-gutter+-show-hunk) ; diff
              ("m r" . git-gutter+-revert-hunk)
              ;; stage hunk at point
              ;; if region is active, stage all hunk lines within the region.
              ("m s" . git-gutter+-stage-hunks)
              ("m c" . git-gutter+-commit)
              ("m C" . git-gutter+-stage-and-commit)
              ("m u" . git-gutter:update-all-windows))
  :init (add-to-list 'display-buffer-alist '("\\*git-gutter+-diff\\*" . (display-buffer-below-selected)))
  :config
  (defun git-gutter+-turn-on--around-advice (orig-func &rest args)
    (unless (and (buffer-file-name)
                 (file-remote-p (buffer-file-name)))
      (apply orig-func args)))
  (advice-add 'git-gutter+-turn-on :around #'git-gutter+-turn-on--around-advice))

;;; [ diff-hl ] -- highlighting uncommitted changes with continuous fringe vertical block.

;; (use-package diff-hl
;;   :ensure t
;;   :defer t
;;   :commands (diff-hl-next-hunk diff-hl-previous-hunk diff-hl-diff-goto-hunk)
;;   :init
;;   (add-hook 'prog-mode-hook 'diff-hl-mode)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   :bind (:map git-quick-prefix
;;               ("M-n" . diff-hl-next-hunk)
;;               ("M-p" . diff-hl-previous-hunk)
;;               ("M-=" . diff-hl-diff-goto-hunk))
;;   :config
;;   (setq vc-git-diff-switches '("--histogram"))
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; [ vc-msg ] -- Show commit message of current line in Emacs.

(use-package  vc-msg
  :ensure t
  :defer t
  :commands (vc-msg-show)
  :bind (:map git-quick-prefix ("b" . vc-msg-show))
  :init (setq vc-msg-git-show-commit-function 'magit-show-commit))

;;; [ git-messenger ] -- popup commit message at current line.

(use-package git-messenger
  :ensure t
  :defer t
  :bind (:map prog-vcs-prefix
              ("m m" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message)
              ("c" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t ; always show detail message.
              ;; git-messenger:handled-backends '(git svn)
              git-messenger:use-magit-popup t))



(provide 'init-prog-vcs-git)

;;; init-prog-vcs-git.el ends here
