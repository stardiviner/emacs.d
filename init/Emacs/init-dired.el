;;; init-dired.el --- init Dired for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Dired ] (Directory editing mode)

(use-package dired
  :ensure direx
  :defer t
  :bind (("C-x d" . dired)
         :map dired-mode-map
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("g" . dired-do-redisplay)
         ("F" . find-name-dired))
  :custom ((dired-auto-revert-buffer t) ; auto refresh dired when file changes
           (dired-listing-switches (concat dired-listing-switches "h")) ; human-readable size
           )
  :config
  (add-hook 'dired-mode-hook #'turn-on-auto-revert-mode)
  (add-hook 'dired-mode-hook #'toggle-truncate-lines) ; don't wrap long lines to break single line
  
  (setq dired-create-destination-dirs 'ask)

  ;; Another thing that did annoy me was the fact that when I traverse the
  ;; directory hierarchy, I leave a trail of open Dired buffers with all the
  ;; directories I go through. This is also easy to change: just enable the
  ;; `dired-find-alternate-file' function (bound to a in Dired) and use it to
  ;; visit a file or directory in place (IOW, open it instead of the current Dired
  ;; buffer – this also works for files I want to visit!).
  ;;
  (put 'dired-find-alternate-file 'disabled nil) ; key [a] in Dired.

  ;; open file with external program by pressing [W].
  (setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))

  ;; allow dired to be able to delete or copy a whole dir.
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top) ; 'top means ask once

  ;; Dired tries to guess a default target directory.
  ;; This means: if there is a Dired buffer displayed in the next window, use its
  ;; current directory, instead of this Dired buffer's current directory.
  (setq dired-dwim-target t)

  ;; reuse the current dired buffer to visit a directory.
  (use-package dired-single
    :ensure t
    :defer t
    :after dired
    :init
    (defun my:dired-single-enable ()
      (define-key dired-mode-map [return] 'dired-single-buffer)
      (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
      (define-key dired-mode-map "^"
        (function (lambda nil (interactive) (dired-single-buffer "..")))))
    (add-hook 'dired-mode-hook #'my:dired-single-enable))
  
  (use-package dired-toggle-sudo        ; browse directory with sudo privileges.
    :ensure t
    :defer t
    :commands (dired-toggle-sudo)
    :bind (:map dired-mode-map ("#" . dired-toggle-sudo)))

  ;; colorful Dired
  (use-package diredfl
    :ensure t
    :defer t
    :hook (dired-mode . diredfl-mode))

  ;; Show git info in Dired.
  (use-package dired-git-info
    :ensure t
    :defer t
    :commands (dired-git-info-mode)
    :bind (:map dired-mode-map (")" . dired-git-info-mode)))
  
  ;; allow rsync from dired buffers especially for large files.
  (use-package dired-rsync
    :ensure t
    :defer t
    :commands (dired-rsync))

  ;; open file with external program.
  (if (featurep 'openwith)
      (add-hook 'dired-mode-hook #'openwith-mode))
  
  ;; open file with `eww'.
  (define-key dired-mode-map (kbd "e")
    (lambda ()
      (interactive)
      (eww-open-file (dired-get-file-for-visit))))

  ;; [image-dired ] -- image in Dired
  (use-package image-dired+
    :ensure t
    :defer t
    :after image-dired
    :commands (image-dired image-dired-show-all-from-dir image-dired-slideshow-start)
    :bind (:map image-dired-minor-mode-map
                ("C-t d" . image-dired-show-all-from-dir)
                ("C-t D" . image-dired-display-thumbs)
                ("C-t s" . image-dired-slideshow-start)
                :map image-dired-thumbnail-mode-map
                ("C-n" . image-diredx-next-line)
                ("C-p" . image-diredx-previous-line)
                ("D" . image-diredx-flagged-delete)
                ("g" . revert-buffer))
    :init (setq image-dired-track-movement nil) ; suppress unknown cursor movements.
    (add-to-list 'display-buffer-alist
                 '("^\\*image-dired\\*" (display-buffer-same-window)))
    (add-to-list 'display-buffer-alist
                 '("^\\*image-dired-display-image\\*" (display-buffer-same-window)))
    :config (image-diredx-async-mode 1) (image-diredx-adjust-mode 1))

  ;; [ wdired ] -- rename files editing their names in dired buffers.
  (use-package wdired
    :defer t
    :commands (wdired-change-to-wdired-mode)
    :init
    (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
    (setq wdired-allow-to-change-permissions t))

  ;; [ diredc ] -- dired extensions and convenience features inspired from "midnight commander".
  ;; (use-package diredc
  ;;   :ensure t
  ;;   :defer t
  ;;   :commands (diredc)
  ;;   :bind (([remap dired-other-frame] . diredc)
  ;;          ;; ([remap dired] . diredc)
  ;;          ;; ("S-<f11>" . diredc)
  ;;          ))
  
  (use-package dired-x                  ; extra Dired functionality
    :defer t
    ;; don't bind [C-x C-j] from `dired-x'. (conflict with `ace-window')
    :preface (setq dired-bind-jump nil)
    :config
    ;; ignore specific files
    ;; toggle `dired-omit-mode' to hide hidden files with [C-x M-o]
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_STORE$\\|^.projectile$"
                  "\\(?:.*\\.\\(?:aux\\|log\\|synctex\\.gz\\|run\\.xml\\|bcf\\|am\\|in\\)\\'\\)\\|^\\.\\|-blx\\.bib")))

  ;; use `all-the-icons' icons to display for files.
  (use-package all-the-icons-dired
    :ensure t
    :defer t
    :commands (all-the-icons-dired-mode)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    ;; Don’t apply ‘all-the-icons-dired’ icons if too many files.
    (with-no-warnings
      (defun my/all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                    (when file
                      (let ((icon (if (file-directory-p file)
                                      (all-the-icons-icon-for-dir file
                                                                  :face 'all-the-icons-dired-dir-face
                                                                  :height 0.9
                                                                  :v-adjust all-the-icons-dired-v-adjust)
                                    (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                        (if (member file '("." ".."))
                            (all-the-icons-dired--add-overlay (point) "  \t")
                          (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
                (forward-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh :override #'my/all-the-icons-dired--refresh)))

  ;; Insert subdirectories in a tree-like fashion.
  (use-package dired-subtree
    :defer t
    :commands (dired-subtree-cycle)
    :bind (:map dired-mode-map ("TAB" . dired-subtree-cycle)))

  ;; Edit Filename At Point in an Emacs' dired buffer
  (use-package dired-efap
    :ensure t
    :defer t
    :commands (dired-efap dired-efap-click)
    :bind (:map dired-mode-map ([f2] . dired-efap) ([down-mouse-1] . dired-efap-click))
    :init (setq dired-efap-use-mouse t))

  ;; `dired-do-*' commands
  (use-package dired-aux
    :defer t
    :commands (dired-do-shell-command dired-do-async-shell-command))

  (use-package dired-narrow
    :ensure t
    :defer t
    :commands (dired-narrow)
    :bind (:map dired-mode-map ("/" . dired-narrow)))

  ;; [ make-it-so ] -- Transform files with Makefile recipes.
  ;; (use-package make-it-so
  ;;   :ensure t
  ;;   :defer t
  ;;   :init (mis-config-default)
  ;;   :config (setq mis-recipes-directory
  ;;                 (concat user-emacs-directory "init/extensions/make-it-so-recipes/")))

  ;; `dired-hide-details-mode'
  ;; (setq dired-hide-details-hide-information-lines t)

  ;; [ dired-git-info ] -- Show Git info in Dired.
  (use-package dired-git-info
    :ensure t
    :defer t
    :commands (dired-git-info-mode)
    :hook (dired . dired-git-info-mode))
  
  ;; [ dired-git ] -- Git integration for Dired.
  ;; (use-package dired-git
  ;;   :ensure t
  ;;   :hook (dired . dired-git-mode))
  )


(provide 'init-dired)

;;; init-dired.el ends here
