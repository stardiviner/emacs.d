;;; init-my-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

;;; [ FlyCheck ] --- modern on-the-fly syntax checking

;;; Features:
;;
;; - Automatic on-the-fly syntax checking while editing
;; - Fully automatic selection of the syntax checker
;; - Optional manual selection of a syntax checker with flycheck-select-checker at C-c ! s
;; - Built-in syntax checkers for:
;; -  Nice error indication and highlighting
;; - Easy customization
;; - Syntax checker configuration with project-specific configuration files and options
;; - Error navigation with next-error and previous-error
;; - Error list with flycheck-list-errors at C-c ! l
;; - Easy declarative API to define new syntax checkers
;; - Error parsers for structured markup formats (e.g. Checkdoc XML)
;;
;; - [C-c ! s] -- manual select a syntax checker.
;; - [C-c ! ?] -- see what a checker needs.
;; - [C-c ! l] -- list error list.
;; - error navigation with 'next-error' and 'previous-error'.
;; - [M-x flycheck-buffer] -- check buffer.
;;
;; Error navigation:
;; - [M-g n] -- next-error.
;; - [M-g p] -- previous-error.
;; - [C-c ! n] -- flycheck-next-error
;; - [C-c ! p] -- flycheck-previous-error
;;
;; Mode line:
;; - FlyC     -- There are no errors in the current buffer.
;; - FlyC*    -- A syntax check is being performed currently.
;; - FlyC:3/4 -- There are three errors and four warnings in the current buffer.
;; - FlyC-    -- Automatic syntax checker selection did not find a suitable syntax checker. See Selection, for more information.
;; - FlyC!    -- The syntax check failed. Inspect the *Messages* buffer for details.

;; - FlyC?    -- The syntax check had a dubious result. The definition of the syntax checker
;;               may be flawed. Inspect the *Messages* buffer for details. This indicator
;;              should never be displayed for built-in syntax checkers. If it is, please
;;              report this issue to the Flycheck developers.


(require 'flycheck)

;; Enable flycheck in all buffers.
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'prog-mode-hook #'flycheck-mode)

;; (save idle-change new-line mode-enabled)
(setq flycheck-check-syntax-automatically '(save new-line)
      flycheck-idle-change-delay 5.0
      flycheck-display-errors-delay 0.9
      flycheck-highlighting-mode 'symbols
      flycheck-indication-mode 'left-fringe
      ;; 'flycheck-fringe-bitmap-double-arrow
      flycheck-standard-error-navigation t ; [M-g n/p]
      flycheck-deferred-syntax-check nil
      ;; flycheck-mode-line '(:eval (flycheck-mode-line-status-text))
      flycheck-completion-system nil ; 'ido, 'grizzl, nil
      )

;; For Languages

;; {emacs-lisp}
;; To make Flycheck use the current `load-path'.
;; Don't error about "free variable" without (require ??).
(setq flycheck-emacs-lisp-initialize-packages t
      flycheck-emacs-lisp-load-path 'inherit
      flycheck-emacs-lisp-package-user-dir nil
      )

;; {Ruby}
(setq flycheck-ruby-executable "rubocop"
      ;; flycheck-rubocop-lint-only t
      )

;; {clang}
;; flycheck-clang-definitions
;; flycheck-clang-include-path
;; flycheck-clang-includes
;; flycheck-clang-language-standard
;; flycheck-clang-no-rtti
;; flycheck-clang-standard-library
;; flycheck-clang-warnings
;; flycheck-cppcheck-checks

(set-face-attribute 'flycheck-info nil
                    :underline '(:color "forest green" :style wave))
(set-face-attribute 'flycheck-fringe-info nil
                    :foreground "forest green")
(set-face-attribute 'flycheck-warning nil
                    :underline '(:color "orange" :style wave)
                    )
(set-face-attribute 'flycheck-fringe-warning nil
                    :foreground "orange"
                    :weight 'normal)
(set-face-attribute 'flycheck-error nil
                    :background "dark red"
                    ;; :underline '(:color "dark red" :style wave)
                    ;; :box '(:color "dark red" :line-width -1)
                    )
(set-face-attribute 'flycheck-fringe-error nil
                    :foreground "dark red"
                    :weight 'normal)


;;; list errors only when has lint errors
(defun flycheck-list-errors-only-when-errors ()
  "List errors only when has lint errors."
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))
;; TODO: only show when has error:
;; (add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)

;;; add Django-mode
;; (with-eval-after-load 'flycheck
;;   (dolist (checker '(python-pylint python-flake8 python-pycompile))
;;     (flycheck-add-mode checker 'django-mode)))


;;; [ flycheck-tip ] -- show you error by popup-tip.

;; (define-key YOUR-PROG-MODE (kbd "C-c C-n") 'flycheck-tip-cycle)

;;; If you want to show current line errors by popup instead of flycheck's echo
;;; area function, then configure like this:
;; (flycheck-tip-use-timer 'verbose)

;;; note: This program avoid flycheck-show-error-at-point function to avoid
;;; duplicated error message(i.e., minibuffer and popup-tip). But if you want to
;;; regain this behavior, set following configuration to your .emacs:
;; (setq flycheck-tip-avoid-show-func nil)

;; (define-key flycheck-mode-map (kbd "C-c ! c") 'flycheck-tip-cycle)


;;; [ flycheck-pos-tip ] -- display errors under point using popup.el.

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))


;;; [ helm-c-flycheck ]

(eval-after-load 'flycheck
  (lambda ()
    (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
    (define-key flycheck-mode-map (kbd "C-c ! !") 'helm-flycheck)))


(provide 'init-my-prog-lint)

;;; init-my-prog-lint.el ends here
