;;; init-my-prog-complete.el --- init for Programming Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ completion ] -- *Completion* buffer

(require 'completion)

(setq enable-completion t
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

;; (completion-mode)
;; (dynamic-completion-mode 1)



;; (require 'init-auto-complete)
(require 'init-company-mode)

;;; make auto-complete work with company-mode
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (company-mode -1)
;;               (auto-complete-mode 1)
;;               )))

;; (require 'init-my-prog-complete-ycmd)


(provide 'init-my-prog-complete)

;;; init-my-prog-complete.el ends here
