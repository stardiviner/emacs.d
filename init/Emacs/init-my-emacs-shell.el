;;; init-my-emacs-shell.el --- init Shell in Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; variables
(setq shell-file-name (getenv "SHELL"))      ; "$SHELL", "/bin/sh", "/bin/bash", "usr/bin/zsh"


;;; [ Cominit ]

(setq comint-prompt-read-only t ; prompt read-only.
      )



;;; [ Shell ]

;; M-x shell is a nice shell interface to use, let's make it colorful. If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq shell-command-completion-mode t)


;;; [ Eshell ] (Emacs Shell)

(require 'eshell)

(setq eshell-save-histroy-on-exit t
      eshell-history-size 128
      eshell-hist-ignoredups t
      eshell-compl-ignore-case t
      eshell-cp-interactive-query t
      eshell-lv-interactive-query t
      eshell-mv-interactive-query t
      eshell-rm-interactive-query t
      eshell-mv-overwrite-files nil
      eshell-highlight-prompt t
      ;; eshell-prompt-regexp "^[^#$\n]* [#>]+"
      ;; eshell-prompt-function (lambda nil
      ;;                          (concat
      ;;                           (abbreviate-file-name
      ;;                            (eshell/pwd))
      ;;                           (if
      ;;                               (= (user-uid) 0)
      ;;                               " # " " > ")))
      )

;;; count how much time the command used.
;; (add-hook 'eshell-load-hook
;;           (lambda()(setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-pre-command-hook
;;           (lambda()(setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda()
;;             (message "spend %g seconds"
;;                      (- (time-to-seconds) last-command-start-time))))

;; ;;; auto-complete settings
;; (autoload 'auto-complete "auto-complete" t)
;; (defvar ac-source-eshell-pcomplete
;;   '((candidates . (pcomplete-completions))))
;; (defun ac-complete-eshell-pcomplete ()
;;   (interactive)
;;   (auto-complete '(ac-source-eshell-pcomplete)))
;;
;; ;;; auto enable ac-mode
;; (global-auto-complete-mode 1)
;; (add-to-list 'ac-modes 'eshell-mode)
;; (setq ac-sources '(ac-source-eshell-pcomplete
;;                    ;; ac-source-files-in-current-dir
;;                    ac-source-filename
;;                    ;; ac-source-abbrev
;;                    ;; ac-source-words-in-buffer
;;                    ;; ac-source-imenu
;;                    ))

(defun my-eshell-start-or-switch ()
  "Start Emacs Shell or switch to its buffer if it already exist."
  (interactive)
  (if (get-buffer "*eshell*") ; eshell already active?
      (switch-to-buffer "*eshell*")
    (let ((default-directory (getenv "HOME")))
      (command-execute 'eshell)
      (bury-buffer))
    ))

;;; start Eshell at Emacs startup, and put in end of buffer list:
(add-hook 'emacs-startup-hook 'my-eshell-start-or-switch)

(global-set-key (kbd "C-x !") 'my-eshell-start-or-switch)

(define-key my-prog-inferior-map (kbd "s") 'my-eshell-start-or-switch)


;;; TODO: use this extension
;; readline-complete.el (with auto-complete.el)

;;; Usage:
;;
;; - [M-x shell] :: start.

(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)

;; (setq rlc-timeout 0.03
;;       rlc-attempts 30
;;       ;; ac-rlc-prompts
;;       )

;;; for auto-complete
(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)

;;; for company-mode backend.
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook
          (lambda () (company-mode -1)))



(provide 'init-my-emacs-shell)

;;; init-my-emacs-shell.el ends here
