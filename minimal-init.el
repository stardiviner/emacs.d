;;; minimal-init.el --- minimal init file for testing.

;;; Commentary:



;;; Code:

(package-initialize)

;;; [ Debug ]
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;;; [ profiler ]
;; (profiler-start 'cpu+mem)

;;; benchmark
;; (require 'init-my-emacs-benchmark)


;;; add my init files directory

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; recursively load init files.
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(setq load-prefer-newer t)

;;; [ package.el ]
(load "~/.emacs.d/init/init-package.el")
(require 'init-package)



(use-package pinentry
  :ensure t
  :ensure-system-package pinentry
  :config
  (pinentry-start))

(require 'color)



;;; my custom functions
(require 'init-my-library)
(require 'init-my-functions)

;;; add your customizations from here

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x C-j") 'ace-window))

(use-package which-key
  :ensure
  :config
  (which-key-mode 1))

(require 'init-ivy)
(require 'init-company-mode)



;; (require 'init-my-org-mode)

;;; Org-mode Babel
(setq org-confirm-babel-evaluate nil)
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;; babel src block editing
(setq org-src-fontify-natively t
      ;; nil: preserve org indent, t: preserve export indent.
      org-src-preserve-indentation nil
      ;; 0: fix `diff' babel syntax highlighting invalid issue.
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively nil ; make [Tab] work native as in major mode.
      org-src-window-setup 'current-window ; 'reorganize-frame, 'current-window
      org-src-ask-before-returning-to-edit-buffer nil
      org-edit-src-auto-save-idle-delay 0 ; 0: don't auto save.
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (shell . t)                          ; Shell
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   (C . t)                              ; C
   (lisp . t)                           ; Lisp
   (clojure . t)                        ; Clojure
   (js . t)                             ; JavaScript
   (haskell . t)       			; Haskell
   (latex . t)                          ; LaTeX
   ))





(provide 'minimal-init)

;;; minimal-init.el ends here
