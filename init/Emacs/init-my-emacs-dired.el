;;; init-my-emacs-dired.el --- init Dired for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Dired ] (Directory editing mode)
;; Usage:
;; - [C-x d] -- dired (cond 'helm-mode (helm-mode-dired))
;; - [C-x C-d] -- simple entry list.
;; - [C-u C-x C-d] -- detail entry list.

(require 'dired)
(require 'dired-x)
(require 'wdired)


;; `dired-do-shell-command' does not know (by default) how to handle some
;; filetypes.
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" (if (exec-installed-p "okular")
                         "okular"
                       "evince"))       ; PDF
        ("\\.mm\\'" (if (exec-installed-p "freeplane")
                        "freeplane"
                      "freemind"))      ; Mind Maps
        ("\\.tex\\'" "pdflatex")        ; TeX
        ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice") ; Office
        ))

;; Another thing that did annoy me was the fact that when I traverse the
;; directory hierarchy, I leave a trail of open Dired buffers with all the
;; directories I go through. This is also easy to change: just enable the
;; `dired-find-alternate-file' function (bound to a in Dired) and use it to
;; visit a file or directory in place (IOW, open it instead of the current Dired
;; buffer – this also works for files I want to visit!).
;;
(put 'dired-find-alternate-file 'disabled nil) ; key [a] in Dired.


;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top) ; 'top means ask once

;; Dired tries to guess a default target directory.
;;
;; This means: if there is a Dired buffer displayed in the next window, use its
;; current directory, instead of this Dired buffer's current directory.
;;
(setq dired-dwim-target t)

;; How to make dired use the same buffer for viewing directory?
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; TODO: modify this function to my version.
(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

Version 2015-01-26
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ξfile-list))))))


;;; [ Dired+ ]

(use-package dired+
  :ensure t
  :config
  ;; The effect is that when you hit [RET] (or click the mouse) on a directory in
  ;; Dired, find-alternate-file is used, so the original Dired buffer is replaced
  ;; (deleted) by the new one.
  (diredp-toggle-find-file-reuse-dir 1)
  )


;;; [ direx ] --- direx.el is a simple directory explorer. It also works as a generic tree explore library.

;;; Usage:
;;
;; - [C-c C-j]
;; - [TAB], [Enter]

(use-package direx
  :ensure t
  :config
  ;; direx-project -- (bundled with direx.el) -- project tree explorer.
  (require 'direx-project)


  (defun my-direx:jump-to-directory ()
    (interactive)
    (if (projectile-project-root)
        ;; (direx-project:jump-to-project-root-other-window)
        (direx-project:jump-to-project-root)
      (direx:jump-to-directory-other-window)
      ))

  (global-set-key (kbd "C-c C-j") 'my-direx:jump-to-directory)

  ;; put direx under popwin management.
  ;; (push '(direx:direx-mode :position left :width 35 :dedicated t)
  ;;       popwin:special-display-config)
  )


;;; [ emacs-dired-k ] -- highlights dired buffer like "k".

;; Usage:
;;
;; - `dired-k'
;;
;; Highlight dired buffer by following parameters.
;;
;;  - File size
;;  - Modified time
;;  - Git status(if here is in git repository)

(use-package dired-k
  :ensure t
  :config
  (setq dired-k-style 'k.zsh) ; nil, 'k.zsh, 'git
  (setq dired-k-human-readable t)

  (define-key dired-mode-map (kbd "K") 'dired-k)
  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)

  (define-key direx:direx-mode-map (kbd "K") 'direx-k)
  )


;;; [ dired-efap ] -- Edit Filename At Point in an Emacs' dired buffer

;;; Usage:
;;
;; - [F2] / double clicking => rename => [RET] :: edit filename.
;; - [C-g] :: abort

(use-package dired-efap
  :ensure t
  :config
  (setq dired-efap-use-mouse t)

  ;; (setq dired-efap-initial-filename-selection 'no-extension)

  (set-face-attribute 'dired-efap-face nil
                      :box '(:color "orange" :line-width 2))

  (define-key dired-mode-map [f2] 'dired-efap)
  (define-key dired-mode-map [down-mouse-1] 'dired-efap-click)
  )


;;; [ peep-dired ] -- A convienent way to look up file contents in other window while browsing directory in dired

;;; Usage:
;;
;; - [C-n/p] :: scroll, and auto display file content.
;; - [SPC], [C-SPC] / [backspace] :: scroll file content down/up.

(use-package peep-dired
  :ensure t
  :config
  ;; When disabling the mode you can choose to kill the buffers that were opened
  ;; while browsing the directories.
  (setq peep-dired-cleanup-on-disable t)
  ;; Or you can choose to kill the buffer just after you move to another entry in
  ;; the dired buffer.
  (setq peep-dired-cleanup-eagerly t)
  ;; If you want the dired buffers that were peeped to have the mode enabled set
  ;; it to true.
  (setq peep-dired-enable-on-directories nil)

  ;; Ignoring Certain File Extensions
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))

  ;; Evil integration
  ;;
  ;; Adjust the state name depending on an evil state you open dired in:
  ;;
  ;; (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
  ;;                  (kbd "C-<SPC>") 'peep-dired-scroll-page-up
  ;;                  (kbd "<backspace>") 'peep-dired-scroll-page-up
  ;;                  (kbd "j") 'peep-dired-next-file
  ;;                  (kbd "k") 'peep-dired-prev-file)
  ;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)

  ;; FIXME:
  ;; (add-hook 'dired-mode-hook 'peep-dired)
  )


;;; [ diredful ]

(use-package diredful
  :ensure t
  :config
  (diredful-mode 1))



;;; The function above will open the current directory in sudo mode. I decided
;;; to bind it to !, since the default & seems strictly better than !. The
;;; function will ask you for the password once. Afterwards, you can open other
;;; directories without having to enter the password.

(defun sudired ()
  "The sudo privilege to change the owner of a file owned by root."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

(define-key dired-mode-map "!" 'sudired)


;;; [ image in Dired ]

(setq diredp-image-preview-in-tooltip 100)

(add-hook 'dired-mode-hook 'tooltip-mode)



(provide 'init-my-emacs-dired)

;;; init-my-emacs-dired.el ends here
