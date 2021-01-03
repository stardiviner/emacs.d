;;; init-org-attach.el --- init for Org attach
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-attach ] -- Manage file attachments to org-mode tasks.

;; store link auto with `org-store-link' using `file:' link type or `attachment:' link type.
;; use `file:' to avoid `attachment:' problems especially on exporters.

(use-package org-attach
  :defer t
  :custom ((org-attach-store-link-p 'file)
           (org-attach-dir-relative t)  ; use relative path to attach dir
           (org-attach-auto-tag nil)    ; don't auto add tag "ATTACH"
           (org-attach-archive-delete 'query)
           (org-attach-preferred-new-method 'ask))
  :init (add-to-list 'org-default-properties "DIR"))

;;; [ org-screenshot ] -- Take and manage screenshots in Org-mode files.

;; (use-package org-screenshot
;;   :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
;;   :pin manual
;;   :defer t
;;   :commands (org-screenshot-take)
;;   :init (setq org-screenshot-image-directory "data/images")
;;   (add-hook 'org-mode-hook
;;             #'(lambda () (local-set-key (kbd "C-c o M-s") 'org-screenshot-take))))

;;; [ org-attach-screenshot ] -- screenshots integrated with org attachment dirs.

;; (use-package org-attach-screenshot
;;   :ensure t
;;   :commands (org-attach-screenshot)
;;   :bind (:map Org-prefix ("M-s" . org-attach-screenshot))
;;   :init
;;   (setq org-attach-screenshot-command-line "import %f") ; "scrot -c -d 5 -s %f"
;;   :config (setq org-attach-screenshot-relative-links t))

;;; [ org-attach-embedded-images ] --

;; This module provides command `org-attach-embedded-images-in-subtree' to save such
;; images as attachments and insert org links to them. Each image is named with its sha1
;; sum.
;;
;; (require 'org-attach-embedded-images)

;;; [ org-download ] -- drag and drop images to Emacs org-mode.

(use-package org-download
  :ensure t
  :defer t
  :preface
  (unless (boundp 'org-download-prefix)
    (define-prefix-command 'org-download-prefix))
  (define-key Org-prefix (kbd "d") 'org-download-prefix)
  :commands (org-download-image
             org-download-screenshot org-download-yank
             org-download-delete org-download-edit
             org-download-dnd)
  :bind (:map org-download-prefix
              ("i" . org-download-image)
              ("s" . org-download-screenshot)
              ("y" . org-download-yank)
              ("d" . org-download-delete)
              ("e" . org-download-edit)
              :map org-mode-map
              ("<drag-n-drop>" . org-download-dnd)
              ("<C-drag-n-drop>" . org-download-dnd)
              ("<M-drag-n-drop>" . org-download-dnd))
  :custom ((org-download-screenshot-method "flameshot gui --raw > %s")
           (org-download-method 'attach) ; 'attach, 'directory,
           ;; if you don't want the #+DOWNLOADED: annotation in your Org document
           (org-download-annotate-function (lambda (_) ""))
           (org-download-backend t) ; url-retrieve (t), wget, curl.
           (org-download-image-dir "data/images")
           )
  :init (org-download-enable))

;;; [ org-web-tools ] -- retrieving web page content and processing it into Org-mode content.

(use-package org-web-tools
  :ensure t
  :defer t
  :commands (org-web-tools-insert-link-for-url ; insert Org link
             org-web-tools-insert-web-page-as-entry ; insert web page as Org entry
             org-web-tools-read-url-as-org ; display web page as Org in new buffer
             org-web-tools-convert-links-to-page-entries ; convert all links in current Org entry to Org headings
             ))

;;; [ org-board ] -- Org mode's web archiver for single web page or website.

(use-package org-board
  :ensure t
  :defer t
  :preface
  (unless (boundp 'org-board-prefix)
    (define-prefix-command 'org-board-prefix))
  (define-key Org-prefix (kbd "C-a") 'org-board-prefix)
  :commands (org-board-new
             org-board-archive org-board-archive-dry-run org-board-cancel
             org-board-open)
  :custom ((org-board-default-browser 'eww) ; press [&] in eww to open in external browser
           (org-board-make-relative t)) ; org-attach use relative path
  :bind (:map org-board-prefix
              ("n" . org-board-new)
              ("a" . org-board-archive)
              ("o" . org-board-open)
              ("r" . org-board-archive-dry-run)
              ("k" . org-board-cancel)
              ("d" . org-board-delete-all)
              ("f" . org-board-diff)
              ("3" . org-board-diff3))
  :init (add-to-list 'display-buffer-alist '("org-board-wget-call" . (display-buffer-below-selected))))


(provide 'init-org-attach)

;;; init-org-attach.el ends here
