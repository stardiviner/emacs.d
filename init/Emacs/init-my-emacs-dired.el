;;; init-my-emacs-dired.el --- init Dired for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Dired ] (Directory editing mode)

(require 'dired)

(use-package dired-x
  ;; :ensure t
  :init
  ;; don't bind [C-x C-j] from `dired-x'. (conflict with `ace-window')
  (setq dired-bind-jump nil)
  )

;; (use-package wdired
;;   :ensure t)
(require 'wdired)

(define-key dired-mode-map (kbd "g") 'dired-do-redisplay)

;;; ignore specific files
;; toggle `dired-omit-mode' with [C-x M-o]
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))

;; `dired-do-shell-command' does not know (by default) how to handle some
;; filetypes.
(setq dired-guess-shell-alist-user
      '(
        ;; PDF
        ("\\.pdf\\'" (if (exec-installed-p "okular")
                         "okular"
                       "evince"))
        ;; Mind Maps
        ("\\.mm\\'" (if (exec-installed-p "freemind")
                        "freemind"
                      "freeplane"))
        ;; TeX
        ("\\.tex\\'" "pdflatex")
        ;; Office
        ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")
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



;;; [ Dired+ ] -- Extensions to Dired.

(use-package dired+
  :ensure t
  :config
  ;; The effect is that when you hit [RET] (or click the mouse) on a directory in
  ;; Dired, find-alternate-file is used, so the original Dired buffer is replaced
  ;; (deleted) by the new one.
  (diredp-toggle-find-file-reuse-dir 1)
  )


;;; [ direx ] --- A simple directory explorer. It also works as a generic tree explore library.

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


;;; [ dired-efap ] -- Edit Filename At Point in an Emacs' dired buffer

(use-package dired-efap
  :ensure t
  :config
  (setq dired-efap-use-mouse t)

  ;; (setq dired-efap-initial-filename-selection 'no-extension)

  (define-key dired-mode-map [f2] 'dired-efap)
  (define-key dired-mode-map [down-mouse-1] 'dired-efap-click)
  )


;;; [ peep-dired ] -- A convienent way to look up file contents in other window while browsing directory in dired

(use-package peep-dired
  :ensure t
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :config
  (setq peep-dired-cleanup-on-disable t
        peep-dired-cleanup-eagerly t
        peep-dired-enable-on-directories t
        peep-dired-ignored-extensions '("mkv" "iso" "mp4")
        )
  )


;;; [ diredful ] -- colorful file names in dired buffers.

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

(setq image-dired-thumb-size 100
      diredp-image-preview-in-tooltip 100)

(add-hook 'dired-mode-hook 'tooltip-mode)


;;; [ dired-narrow ]

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow))
  )


;;; [ call eww in Dired ]

;;; If you have files that you read in eww, here's a suggestion that might make
;;; your workflow a bit easier:

(define-key dired-mode-map (kbd "e")
  (lambda ()
    (interactive)
    (eww-open-file (dired-get-file-for-visit))))


;;; Run rsync in Dired.

(defun ora-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(define-key dired-mode-map "Y" 'ora-dired-rsync)


;;; [ dired-launch ] -- launch an external application from dired.

(use-package dired-launch
  :ensure t
  :config
  (dired-launch-enable)
  (define-key dired-launch-mode-map (kbd "J") nil)
  (define-key dired-launch-mode-map (kbd "K") nil)
  (define-key dired-launch-mode-map (kbd "C-c l") 'dired-launch-command)
  (define-key dired-launch-mode-map (kbd "C-c L") 'dired-launch-with-prompt-command)
  )



(provide 'init-my-emacs-dired)

;;; init-my-emacs-dired.el ends here
