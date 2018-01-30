;;; init-dired.el --- init Dired for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Dired ] (Directory editing mode)

(use-package dired
  :bind (("C-x d" . dired)
         :map dired-mode-map
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("g" . dired-do-redisplay)
         ("i" . ido-find-file)
         ("F" . find-name-dired)
         ;; ("e" . ediff-files)
         )
  :config
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  
  (defun dired-sudo ()
    "The sudo privilege to change the owner of a file owned by root."
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir)))))

  (define-key dired-mode-map "!" 'dired-sudo)

  ;; Run rsync in Dired.
  (defun dired-rsync (dest)
    (interactive (list
                  (expand-file-name
                   (read-file-name
                    "Rsync to:"
                    (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (tmtxt/rsync-command "rsync -arvz --progress "))
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

  (define-key dired-mode-map "Y" 'dired-rsync)

  ;; open file with eww.
  (define-key dired-mode-map (kbd "e")
    (lambda ()
      (interactive)
      (eww-open-file (dired-get-file-for-visit))))

  ;; Another thing that did annoy me was the fact that when I traverse the
  ;; directory hierarchy, I leave a trail of open Dired buffers with all the
  ;; directories I go through. This is also easy to change: just enable the
  ;; `dired-find-alternate-file' function (bound to a in Dired) and use it to
  ;; visit a file or directory in place (IOW, open it instead of the current Dired
  ;; buffer – this also works for files I want to visit!).
  ;;
  (put 'dired-find-alternate-file 'disabled nil) ; key [a] in Dired.

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

  ;; allow dired to be able to delete or copy a whole dir.
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top) ; 'top means ask once

  ;; Dired tries to guess a default target directory.
  ;; This means: if there is a Dired buffer displayed in the next window, use its
  ;; current directory, instead of this Dired buffer's current directory.
  (setq dired-dwim-target t)

  ;; [ dired+ (diredp) ]
  ;; (use-package dired+
  ;;   :ensure t
  ;;   :config
  ;;   ;; disable by default hide details
  ;;   (setq diredp-hide-details-initially-flag nil
  ;;         diredp-hide-details-propagate-flag nil)
  ;;   ;; The effect is that when you hit [RET] (or click the mouse) on a directory
  ;;   ;; in Dired, find-alternate-file is used, so the original Dired buffer is
  ;;   ;; replaced (deleted) by the new one.
  ;;   (diredp-toggle-find-file-reuse-dir 1)
  ;;   ;; image thumbnails tooltip
  ;;   (add-hook 'dired-mode-hook #'tooltip-mode))

  ;; [image-dired ] -- image in Dired
  (use-package image-dired+
    :ensure t
    :after image-dired
    :config
    (image-diredx-async-mode 1)
    (image-diredx-adjust-mode 1)
    (define-key image-dired-thumbnail-mode-map "g" 'revert-buffer)
    ;; delete confirmation prompt with thumbnails.
    (define-key image-dired-thumbnail-mode-map "x" 'image-diredx-flagged-delete))
  
  (use-package wdired ; Rename files editing their names in dired buffers.
    :ensure t
    :bind (:map dired-mode-map ("C-c C-p" . wdired-change-to-wdired-mode))
    :config
    (setq wdired-allow-to-change-permissions t))

  (use-package dired-x ; extra Dired functionality
    :preface
    ;; don't bind [C-x C-j] from `dired-x'. (conflict with `ace-window')
    (setq dired-bind-jump nil)
    :config
    ;; ignore specific files
    ;; toggle `dired-omit-mode' to hide hidden files with [C-x M-o]
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_STORE$\\|^.projectile$"
                  "\\(?:.*\\.\\(?:aux\\|log\\|synctex\\.gz\\|run\\.xml\\|bcf\\|am\\|in\\)\\'\\)\\|^\\.\\|-blx\\.bib"))
    )

  ;; colorful file names in dired buffers.
  (use-package diredful
    :ensure t
    :config
    (diredful-mode 1))

  ;; use `all-the-icons' icons to display for files.
  (use-package all-the-icons-dired
    :ensure t
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

  ;; Edit Filename At Point in an Emacs' dired buffer
  (use-package dired-efap
    :ensure t
    :bind (:map dired-mode-map
                ([f2] . dired-efap)
                ([down-mouse-1] . dired-efap-click))
    :config
    (setq dired-efap-use-mouse t)
    ;; (setq dired-efap-initial-filename-selection 'no-extension)
    )

  (use-package dired-aux ; less commonly used parts of dired.
    :init
    (use-package async
      :ensure t
      :load (dired-async)
      :config
      (dired-async-mode 1)))
  
  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))
  
  ;; A convienent way to look up file contents in other window while browsing
  ;; directory in Dired.
  (use-package peep-dired
    :ensure t
    :bind (:map dired-mode-map
                ("P" . peep-dired))
    :config
    (setq peep-dired-cleanup-on-disable t
          peep-dired-cleanup-eagerly t
          peep-dired-enable-on-directories t
          peep-dired-ignored-extensions '("mkv" "iso" "mp4")))

  ;; launch an external application from dired.
  (use-package dired-launch
    :ensure t
    :bind (:map dired-launch-mode-map
                ("J" . nil)
                ("K" . nil)
                ("C-c l" . dired-launch-command)
                ("C-c L" . dired-launch-with-prompt-command))
    :config
    (dired-launch-enable))
  
  ;; A simple directory explorer. It also works as a generic tree explore library.
  (use-package direx
    :ensure t
    ;; direx-project -- (bundled with direx.el) -- project tree explorer.
    :load (direx-project)
    :bind ("C-c C-j" . my-direx:jump-to-directory)
    :config
    (defun my-direx:jump-to-directory ()
      (interactive)
      (if (projectile-project-root)
          ;; (direx-project:jump-to-project-root-other-window)
          (direx-project:jump-to-project-root)
        (direx:jump-to-directory-other-window)
        )))
  (use-package dired-hacks-utils
    :ensure t)
  (use-package dired-collapse
    :ensure t
    :config
    ;; (dired-collapse-mode 1)
    )

  (use-package find-by-pinyin-dired ; Find file by first Pinyin characters of Chinese Hanzi.
    :ensure t)
  
  (use-package ivy-dired-history
    :ensure t
    :defer t
    :config
    (require 'savehist)
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
    (savehist-mode 1)
    
    (with-eval-after-load 'dired
      (require 'ivy-dired-history)
      ;; if you are using ido,you'd better disable ido for dired
      ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
      (define-key dired-mode-map "." 'dired))
    )
  
  ;; [ make-it-so ] -- Transform files with Makefile recipes.
  (use-package make-it-so
    :ensure t
    :config
    (mis-config-default)
    (setq mis-recipes-directory
          (concat user-emacs-directory "init/extensions/make-it-so-recipes/")))
  )

;;; [ exiftool ] -- an elisp wrapper around ExifTool.

(use-package exiftool
  :ensure t
  :defer t)


(provide 'init-dired)

;;; init-dired.el ends here
