;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ completion ] -- *Completion* buffer

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    add a completion command into it with mode locally.
;;    (add-hook 'completion-at-point-functions 'completion-function nil t)

(use-package completion
  :config
  (setq enable-completion t
        completion-ignore-case t
        completion-cdabbrev-prompt-flag nil
        completion-on-separator-character nil
        ;; completion-prompt-speed-threshold 4800 ; default
        ;; completion-search-distance 15000
        completion-show-help t
        completion-show-inline-help t
        ;; completions-merging-modes '(lisp c)
        save-completions-flag t
        save-completions-file-name "~/.emacs.d/completion"
        ;; save-completions-retention-time 336
        )

  (setq-default completion-at-point-functions nil)

  ;; (setq-default completion-at-point-functions
  ;;               '(pcomplete-completions-at-point
  ;;                 ;; semantic-completion-at-point-function
  ;;                 ;; tags-completion-at-point-function
  ;;                 ))

  ;; (setq tab-always-indent 'complete)
  ;; (add-to-list 'completion-styles 'initials t)
  ;; (setq completion-cycle-threshold 5)

  ;; (completion-mode)
  ;; (dynamic-completion-mode 1)
  )

;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(use-package pcomplete
  :ensure t
  :config
  (setq pcomplete-ignore-case t)
  )


;; (require 'init-ido)
(require 'init-ivy)
(require 'init-helm)

(if (bound-and-true-p helm-mode)
    (ivy-mode -1)
  (helm-mode 1))



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
