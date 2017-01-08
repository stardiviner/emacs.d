;;; init-my-emacs-shell.el --- init Shell in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; variables

;; "$SHELL", "/bin/sh", "/bin/bash", "usr/bin/zsh"
;; (setq shell-file-name (getenv "SHELL"))


;;; [ Shell ]

;; M-x shell is a nice shell interface to use, let's make it colorful. If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq shell-command-completion-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [ Eshell ] (Emacs Shell)

(require 'eshell)

;; load eshell's sudo which use Tramp's su/sudo methods.
(require 'em-tramp)
;; Switch to eshell’s sudo
;; by prefering built-in commands
(setq eshell-prefer-lisp-functions t
      eshell-prefer-lisp-variables t)
(setq password-cache t)

(setq eshell-save-histroy-on-exit t
      eshell-history-size 500
      eshell-hist-ignoredups t
      eshell-compl-ignore-case t
      eshell-cp-interactive-query t
      eshell-lv-interactive-query t
      eshell-mv-interactive-query t
      eshell-rm-interactive-query t
      eshell-mv-overwrite-files nil
      eshell-highlight-prompt t
      )

;; visual commands
;; (setq eshell-destroy-buffer-when-process-dies nil)

;; Eshell-banner
(setq eshell-banner-message (format "%s %s\nwith Emacs %s on Linux: %s"
                                    (propertize
                                     "Eshell session started on"
                                     'face '((:foreground "dim gray")))
                                    (propertize
                                     (format-time-string "%c")
                                     'face '((:foreground "gray")))
                                    (propertize emacs-version
                                                'face '((:foreground "yellow")))
                                    (propertize
                                     (with-temp-buffer
                                       (call-process "uname" nil t nil "-r")
                                       (buffer-string))
                                     'face '((:foreground "orange")))))


;; EShell Prompt info:
;; Git
;; branch name
(defun my-eshell-git-branch-name (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output
           (shell-command-to-string
            (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]")
      )))

;; Eshell prompt
(setq eshell-prompt-function
      #'(lambda ()
          ;; (concat
          ;;  (abbreviate-file-name (eshell/pwd))
          ;;  ;; (newline)
          ;;  (getenv "USER")
          ;;  "@"
          ;;  (system-name)
          ;;  ":"
          ;;  (if (= (user-uid) 0) " # " " $ "))

          (format "{ %s } [ %s ]\n%s %s "
                  (propertize
                   (getenv "USER")
                   'face '((:foreground "sky blue")))
                  (propertize
                   (abbreviate-file-name (eshell/pwd))
                   'face '((:foreground "gray")))
                  ;; (propertize
                  ;;  (or (my-eshell-git-branch-name (eshell/pwd)) " ")
                  ;;  'face '((:foreground "green yellow"))
                  ;;  )
                  (propertize ; $ ➜ ⇨ </>
                   (if (= (user-uid) 0) "#" "⇨")
                   'face '((:foreground "deep pink")))
                  ;; set following cmd face
                  (propertize
                   "·"
                   'face '((:foreground "light gray")))
                  )))

;; (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")
;; (setq eshell-prompt-regexp "^[^#$\n]*[#⇨]* ")
(setq eshell-prompt-regexp "^[^#$\n]*[#⇨] · ")

;; Eshell modules
;; (add-to-list 'eshell-modules-list 'eshell-rebind)

;; Eshell completion
;;
;; - `eshell-cmpl-load-hook'

(setq eshell-show-lisp-completions t
      ;; eshell-command-completion-function #'function
      ;; eshell-cmpl-command-name-function #'function
      ;; eshell-default-completion-function #'function
      eshell-cmpl-use-paring t
      )

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (company-mode 1)
            (define-key company-active-map [return] 'company-complete-selection)
            (define-key company-active-map "\r" 'company-complete-selection)
            ))

(defun my-smart-eshell (&optional arg)
  "Smart set directory path."
  (interactive "P")
  (if arg
      (let ((default-directory (getenv "HOME")))
        (command-execute 'eshell))
    (command-execute 'eshell))
  )

(global-set-key (kbd "C-x !") 'my-smart-eshell)


;;; Eshell smart display

(require 'em-smart)

(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)


;; count how much time the command used.

;; (add-hook 'eshell-load-hook
;;           (lambda () (setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-pre-command-hook
;;           (lambda () (setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda ()
;;             (message "spend %g seconds"
;;                      (- (time-to-seconds) last-command-start-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [ eshell-fixed-prompt ] -- minor mode to restrict eshell to a single fixed prompt.

(use-package eshell-fixed-prompt
  :ensure t
  :config)


(provide 'init-my-emacs-shell)

;;; init-my-emacs-shell.el ends here
