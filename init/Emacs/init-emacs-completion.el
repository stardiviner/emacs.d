;;; init-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ completion ] -- *Completion* buffer

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    add a completion command into it with mode locally.
;;    (add-hook 'completion-at-point-functions 'completion-function nil t)

(setq-default completion-styles '(basic partial-completion)
              completion-show-inline-help nil
              ;; completion-cycle-threshold t
              completion-ignore-case t)

(setq tab-always-indent 'complete) ; first try indent, if already, then try to complete.

(setq history-delete-duplicates t)

;;; [ selectrum ] -- Easily select item from list.

;; Selectrum is a better solution for incremental narrowing in Emacs, replacing
;; Helm, Ivy, and IDO. Its design philosophy is based on choosing the right
;; abstractions and prioritizing consistency and predictability over
;; special-cased improvements for particular cases. As such, Selectrum follows
;; existing Emacs conventions where they exist and are reasonable, and it
;; declines to implement features which have marginal benefit compared to the
;; additional complexity of a new interface.

;; (use-package selectrum
;;   :ensure t
;;   :custom ((selectrum-num-candidates-displayed 7))
;;   :hook (after-init . selectrum-mode))
;;
;; (use-package selectrum-prescient
;;   :ensure t
;;   :hook (selectrum-mode . selectrum-prescient-mode))
;;
;; (use-package prescient
;;   :ensure t
;;   :config (prescient-persist-mode 1))
;;
;; (use-package orderless
;;   :ensure t
;;   :custom (completion-styles '(basic partial-completion emacs22 orderless)))
;;
;; (use-package consult
;;   :ensure t
;;   :bind (([remap switch-to-buffer] . consult-buffer)
;;          ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
;;          ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
;;          ([remap yank-pop] . consult-yank)
;;          ([remap apropos] . consult-apropos)
;;          ("C-c ." . imenu)
;;          ([remap imenu] . consult-imenu)
;;          ([remap grep] . consult-grep)
;;          ([remap vc-git-grep] . consult-git-grep)
;;          ([remap locate] . consult-locate)
;;          ([remap jump-to-mark] . consult-mark)
;;          ([remap pop-global-mark] . consult-global-mark)
;;          ([remap register-list] . consult-register)
;;          ([remap bookmark-jump] . consult-bookmark)
;;          ([remap kmacro-call-macro] . consult-kmacro)
;;          ([remap ripgrep] . consult-ripgrep)
;;          ;; ([remap goto-line] . consult-line)
;;          ([remap goto-line] . consult-goto-line)
;;          ([remap man] . consult-man))
;;   :config
;;   (if (fboundp 'projectile-project-root)
;;       (setq-default consult-project-root-function 'projectile-project-root)))
;;
;; (use-package embark
;;   :ensure t
;;   :after selectrum
;;   :bind (:map selectrum-minibuffer-map
;;               ("C-c C-o" . embark-export)
;;               ("C-c C-c" . embark-act)))
;;
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult))
;;
;; (use-package marginalia
;;   :ensure t
;;   :hook (after-init . marginalia-mode)
;;   :config (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

;;; [ Ivy ]

(require 'init-ivy)

;;; [ Helm ]

;; (require 'init-helm)

;; (use-package snails ; A modern, easy-to-expand fuzzy search framework.
;;   :require t
;;   :quelpa (snails :fetcher github :repo "manateelazycat/snails"))



(provide 'init-emacs-completion)

;;; init-emacs-completion.el ends here
