;;; init-my-prog-lint.el --- init Programming Lint

;;; Commentary:



;;; Code:

;;; [ FlyCheck ] --- init FlyCheck
;;; Commentary:
;; Modern on-the-fly syntax checking for GNU Emacs 24 (aka “Flymake done right”)
;; Flycheck is a modern alternative to Flymake that supports many programming languages out of the box.

;; http://flycheck.github.io/

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
;; - FlyC?    -- The syntax check had a dubious result. The definition of the syntax checker may be flawed. Inspect the *Messages* buffer for details. This indicator should never be displayed for built-in syntax checkers. If it is, please report this issue to the Flycheck developers.


(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode) ; enable flycheck in all buffers.

(setq flycheck-check-syntax-automatically
      '(save idle-change new-line mode-enabled)
      )

(setq flycheck-idle-change-delay 0.5 ; delay seconds
      ;; flycheck-temporaries nil
      flycheck-standard-error-navigation t
      ;;; {clang}
      ;; flycheck-clang-definitions
      ;; flycheck-clang-include-path
      ;; flycheck-clang-includes
      ;; flycheck-clang-language-standard
      ;; flycheck-clang-no-rtti
      ;; flycheck-clang-standard-library
      ;; flycheck-clang-warnings
      ;; flycheck-cppcheck-checks
      ;;; {emacs-lisp}
      ;; flycheck-emacs-lisp-initialize-packages 'auto
      ;; flycheck-emacs-lisp-load-path
      ;; flycheck-emacs-lisp-package-user-dir
      )

;;; < Error reporting >

;;; Faces
;; - flycheck-error
;; - flycheck-warning
;; - flycheck-fringe-error
;; - flycheck-fringe-warning
(set-face-attribute 'flycheck-info nil
                    :background nil
                    :underline '(:color "green" :style wave))
(set-face-attribute 'flycheck-fringe-info nil
                    :foreground "sky blue")
(set-face-attribute 'flycheck-warning nil
                    :background nil :foreground nil
                    :underline '(:color "dark red" :style wave))
(set-face-attribute 'flycheck-fringe-warning nil
                    :background " " :foreground "orange"
                    :weight 'normal)
(set-face-attribute 'flycheck-error nil
                    :background nil :foreground nil
                    :underline '(:color "red" :style wave))
(set-face-attribute 'flycheck-fringe-error nil
                    :background " " :foreground "dark red"
                    :weight 'normal)

;; determine how to highlight errors:
(setq flycheck-highlighting-mode 'symbols ; 'symbols, 'columns, 'sexps, 'lines, nil
      flycheck-indication-mode 'left-fringe ; 'left-fringe, 'right-fringe, nil
      flycheck-display-errors-delay 0.9
      ;; flycheck-mode-line
      ;; flycheck-mode-line-lighter
      flycheck-completion-system nil ; 'ido, 'grizzl, nil
      ;; flycheck-display-error-at-point-timer
      ;; flycheck-deferred-syntax-check
      flycheck-error-list-highlight-overlays t
      ;; flycheck-error-list-mode-line-map
      )


;;; list errors only when has lint errors
(defun flycheck-list-errors-only-when-errors ()
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))

;; (add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)




;;; [ helm-c-flycheck ]

(require 'helm-flycheck)                ; not necessary if using ELPA package
(eval-after-load 'flycheck
  (lambda ()
    (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
    (define-key flycheck-mode-map (kbd "C-c ! !") 'helm-flycheck)))


(provide 'init-my-prog-lint)

;;; init-my-prog-lint.el ends here
