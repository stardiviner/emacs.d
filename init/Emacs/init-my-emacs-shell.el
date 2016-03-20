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
          '(lambda ()
             (eshell-cmpl-initialize)
             (company-mode 1)
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


;;; [ shelldoc ] -- Improve edit shell command in minibuffer.

;;; Usage:
;;
;; Now you can see man page when read-shell-command is invoked. e.g. M-x
;; shell-command C-v / M-v to scroll the man page window. C-c C-s / C-c C-r to
;; search the page.
;;
;; You can complete - (hyphen) option at point. Try to type C-i after insert -.
;;
;; - You may install new man page after shelldoc:
;;     M-x shelldoc-clear-cache
;; - shelldoc is working as a minor mode if you desire.
;;   - eshell
;;     (add-hook 'eshell-mode-hook 'shelldoc-minor-mode-on)
;;   - sh-mode (editing shell script)
;;     (add-hook 'sh-mode-hook 'shelldoc-minor-mode-on)
;;   - M-x shell
;;     (add-hook 'shell-mode-hook 'shelldoc-minor-mode-on)
;; - To toggle shelldoc feature.
;;   M-x shelldoc

(use-package shelldoc
  ;; :ensure t
  :config
  (setq shelldoc-keep-man-locale nil ; To show original man page initially. (probably english)
        shelldoc-idle-delay 0.2
        shelldoc-fuzzy-match-requires 2
        )

  ;; minor mode for eshell
  (add-hook 'eshell-mode-hook 'shelldoc-minor-mode-on)
  ;; minor mode for sh-mode (editing shell script)
  (add-hook 'sh-mode-hook 'shelldoc-minor-mode-on)
  ;; M-x shell -> shell-mode (inferior)
  (add-hook 'shell-mode-hook 'shelldoc-minor-mode-on)
  )



(provide 'init-my-emacs-shell)

;;; init-my-emacs-shell.el ends here
