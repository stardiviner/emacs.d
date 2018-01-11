;;; init-eshell.el --- init for Eshell

;;; Commentary:



;;; Code:

;;; [ Eshell ] (Emacs Shell)

(require 'eshell)

;;; change PAGER from `less' to `cat'.
(setenv "PAGER" "cat")

;;; TERM
(require 'em-term)

;;; Eshell sudo
;; load eshell's sudo which use Tramp's su/sudo methods.
(require 'em-tramp)
;; Switch to eshell’s sudo
;; by prefering built-in commands
(setq eshell-prefer-lisp-functions t
      eshell-prefer-lisp-variables t)
(setq password-cache t
      ;; password-cache-expiry 3600 ; 1 hour
      )

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
(setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                               "ncftp" "pine" "tin" "trn" "elm" "vim"
                               "nmtui" "alsamixer" "htop" "el" "elinks"
                               ))
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

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
(require 'em-prompt)
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
                   (if (= (user-uid) 0) " #" " $")
                   'face '((:foreground "deep pink")))
                  ;; set following cmd face
                  (propertize
                   " ☯ "
                   'face '((:foreground "dark")))
                  )))

(setq eshell-prompt-regexp "^[^#$\n]* [#$] ☯ ")

;; Eshell modules
;; (add-to-list 'eshell-modules-list 'eshell-rebind)

;; Eshell completion
;;
;; - `eshell-cmpl-load-hook'
(require 'em-cmpl)
(setq eshell-show-lisp-completions t
      ;; eshell-command-completion-function #'function
      ;; eshell-cmpl-command-name-function #'function
      ;; eshell-default-completion-function #'function
      eshell-cmpl-use-paring t
      )

(defun my-eshell-completing-setup ()
  "Setup my Eshell completing."
  (interactive)
  (eshell-cmpl-initialize)
  ;; disable company-mode auto complete to speed up Eshell typing command, use [Tab] manually.
  (company-mode 1)
  ;; (local-set-key (kbd "<tab>") 'company-complete)
  (setq-local company-minimum-prefix-length 3)
  (setq-local company-idle-delay 0.4)
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map "\r" 'company-complete-selection)
  )

(add-hook 'eshell-mode-hook 'my-eshell-completing-setup)

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

;;; Bookmarking directories in Eshell with oh-my-zsh plugin "jump".
;; http://mbork.pl/2017-03-04_Bookmarking_directories_in_Eshell
;; http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
;; Usage:
;; - mark, jump, unmark, marks.

(defvar eshell-jump-bookmark-dir "~/.masks/")

(or (f-directory-p eshell-jump-bookmark-dir)
    (make-directory eshell-jump-bookmark-dir))

(defun eshell/jump (mark)
  "Jump to a directory symlinked to by a file called ~/.emacs.d/.marks/MARK."
  (eshell/cd (file-symlink-p (concat eshell-jump-bookmark-dir mark))))

(defun pcomplete/jump ()
  "Auto-complete a command that wants a name of a file in ~/.emacs.d/.marks."
  (pcomplete-here* (directory-files eshell-jump-bookmark-dir)))


;;; [ eshell-bookmark ] -- Integrate bookmarks with eshell.

(use-package eshell-bookmark
  :ensure t
  :config
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))


(provide 'init-eshell)

;;; init-eshell.el ends here
