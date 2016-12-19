;;; init-my-org-attach.el --- init for Org attach
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-attach ] -- Manage file attachments to org-mode tasks.

;; - [C-c C-a] :: `org-attach'.
;; - drag & drop

(require 'org-attach)

(setq org-attach-directory "data/"
      org-attach-archive-delete 'query
      org-attach-allow-inheritance t
      org-attach-auto-tag nil
      )

;;; [ org-screenshot ] -- Take and manage screenshots in Org-mode files.

;;; Usage:
;;
;; - [M-x org-screenshot]
;; - `org-screenshot-take'

;; (setq org-screenshot-command-line "scrot -d 5 -s %f" ; "import %f",
;;       org-screenshot-relative-links t
;;       org-attach-directory "data/"
;;       org-screenshot-image-directory "./images/"
;;       org-screenshot-file-name-format "screenshot-%2.2d.png"
;;       )
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c o s") 'org-screenshot)))

;;; [ org-download ] -- drag and drop images to Emacs org-mode.

(use-package org-download
  :ensure t
  :config
  (setq org-download-screenshot-method "scrot -s %s"
        org-download-method 'attach ; 'attach, 'directory,
        ;; if you don't want the #+DOWNLOADED: annotation in your Org document
        org-download-annotate-function (lambda (_) "")
        org-download-backend t ; url-retrieve (t), wget, curl.
        ;; org-download-heading-lvl
        ;; org-download-timestamp "_%Y-%m-%d_%H:%M:%S"
        org-download-image-dir "data/images" ; nil: default to "."
        ;; org-download-image-width nil ; use #+attr_html: :width
        ;; org-download-img-regex-list '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
        )

  (org-download-enable)

  (unless (boundp 'my-org-download-map)
    (define-prefix-command 'my-org-download-map))
  (define-key my-org-prefix (kbd "d") 'my-org-download-map)

  (define-key my-org-download-map (kbd "i") 'org-download-image)
  (define-key my-org-download-map (kbd "s") 'org-download-screenshot)
  (define-key my-org-download-map (kbd "y") 'org-download-yank)
  (define-key my-org-download-map (kbd "d") 'org-download-delete)
  (define-key my-org-download-map (kbd "e") 'org-download-edit)

  (define-key org-mode-map (kbd "<drag-n-drop>") 'org-download-dnd)
  (define-key org-mode-map (kbd "<C-drag-n-drop>") 'org-download-dnd)
  (define-key org-mode-map (kbd "<M-drag-n-drop>") 'org-download-dnd)
  )

;;; [ org-board ] -- Org mode's web archiver.

(use-package org-board
  :ensure t
  :config
  (unless (boundp 'my-org-board-prefix)
    (define-prefix-command 'my-org-board-prefix))
  (define-key my-org-prefix (kbd "C-a") 'my-org-board-prefix)

  (define-key my-org-board-prefix (kbd "a") 'org-board-new)
  (define-key my-org-board-prefix (kbd "n") 'org-board-new)
  (define-key my-org-board-prefix (kbd "C-a") 'org-board-archive)
  (define-key my-org-board-prefix (kbd "r") 'org-board-archive-dry-run)
  (define-key my-org-board-prefix (kbd "o") 'org-board-open)
  (define-key my-org-board-prefix (kbd "d") 'org-board-diff)
  (define-key my-org-board-prefix (kbd "C-d") 'org-board-delete-all)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-org-attach)

;;; init-my-org-attach.el ends here
