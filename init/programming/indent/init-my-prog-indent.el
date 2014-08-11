;;; init-my-prog-indent.el --- indent init for programming

;;; Commentary:


;;; Code:

;;; electric-indent-mode

;; (electric-indent-mode t)



;;; [ auto-indent-mode ]
;; (require 'auto-indent-mode)

;; (setq
;;  ;; auto-indent-indent-style 'moderate     ; 'aggressive, 'moderate, 'conservative.
;;  auto-indent-fix-org-return t ; allow newline and indent behavior in source code blocks in org-mode.
;;  auto-indent-fix-org-backspace t
;;  auto-indent-fix-org-auto-fill t
;;  auto-indent-fix-org-yank t ; allow org-mode yanks to be indented in source code blocks of org-mode.
;;  auto-indent-fix-org-move-beginning-of-line t
;;  ;; auto-indent-block-close
;;  ;; auto-indent-engine
;;  )

;; (auto-indent-global-mode 1)

;;; [ highlight-indentation ]
(require 'highlight-indentation)

(setq highlight-indentation-offset 4) ; default offset if have not found offset in major mode.
;; (highlight-indentation-set-offset) ; set offset buffer locally.

(set-face-attribute 'highlight-indentation-face nil
                    :background "#00232C" :foreground nil
                    :inherit nil)
(set-face-attribute 'highlight-indentation-current-column-face nil
                    :background "slate blue" :foreground nil
                    :inherit nil)

(dolist (hook '(ruby-mode-hook
                python-mode-hook
                shell-mode-hook
                html-mode-hook
                css-mode-hook
                c-mode-hook
                c++-mode-hook
                go-mode-hook
                lua-mode-hook
                php-mode-hook
                ))
  (add-hook hook '(lambda ()
                    ;; displays guidelines indentation (space indentation only)
                    (highlight-indentation-mode t)
                    ;; displays guidelines for the current-point indentation (space indentation only)
                    (highlight-indentation-current-column-mode)
                    )))

;; TODO: ignore first 1, (or and 2) level indent guide line?

;;; [ indent-hints ] -- Get some hints about whether your buffer is space- or tab-loving
;; Usage:
;; 1.
;; Just check out your mode-line to see whether the buffer you're visiting is space-loving or tab-loving. It also shows the ratio of space-to-tab (or tab-to-space, whichever your buffer loves) loving-ness that your current buffer exudes. Here's a "screenshot":
;;            test.el Top -- (Emacs-Lisp \t:0.53 yas pair)--etc. etc.--
;; The file being visited in the "screenshot" has more tabs than spaces (53% of the lines that start with some sort of indentation start with tabs, to be exact).
;; 2.
;; You can also add your own custom functions to the hooks indent-hints-mode-tab-loving-hook and indent-hints-mode-space-loving-hook which run after a buffer is detected to be tab-loving or space-loving, respectively.
;; 3. Functions:
;; - (ih/activate-space-loving-profile) --> Activate the space-loving profile.
;; - (ih/activate-tab-loving-profile)   --> Activate the tab-loving profile.
;; - (indent-hints-global-activate)     --> Sets up the minor-mode-alist and buffer-local variable for indentation hints.
;; - (indent-hints-mode-on)             --> Turns on indent-hints-mode, if appropriate. This function is intended to be used with define-globalized-minor-mode.

;; (require 'indent-hints)

(setq indent-hints-tab-loving-modeline-indicator " \\t")
;; You can set up some "whitespace profiles" that get selected automatically
;; when a buffer is detected to be tab-loving or space-loving.
(setq indent-hints-profile-switching-enabled t)

;; (indent-hints-global-mode t)


;;; [ indent-hint.el ]
;; TODO (load "~/.emacs.d/my-init/extensions/indent-hint.el")
;; TODO 1. diff with indent/indent-hint.el file.
;;    (load "~/.emacs.d/extensions/indent/indent-hint-files/indent-hint-lisp.el")
;; TODO change indent line style and colors.

;; (dolist (hook '(;; emacs-lisp-mode-hook
;;                 lisp-mode-hook
;;                 ;; lisp-interaction-mode-hook
;;                 ))
;;   (add-hook hook 'indent-hint-lisp))



(provide 'init-my-prog-indent)
