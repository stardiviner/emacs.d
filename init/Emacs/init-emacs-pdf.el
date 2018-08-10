;;; init-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pdf-tools ] -- Emacs support library for PDF files.

(use-package pdf-tools
  :ensure t
  :ensure-system-package (pdfinfo . "sudo pacman -S --noconfirm poppler poppler-data")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defer t
  :init (pdf-tools-install)
  :config
  ;; [ PDF View ]
  ;; - [SPC] :: scroll continuous
  ;; - [n/p] :: scroll to next/previous page
  (setq pdf-view-use-scaling t ; open PDFs scaled to fit page.
        pdf-view-use-unicode-ligther nil ; to speed-up pdf-tools by don't try to find unicode.
        )

  ;; helpful accessibility shortcuts
  (define-key pdf-view-mode-map (kbd "q") 'kill-current-buffer)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  ;; save after adding annotation comment
  (defun pdf-tools-save-buffer ()
    "This function is used to save pdf-annot commits."
    (save-buffer))
  (advice-add 'pdf-annot-edit-contents-commit :after 'pdf-tools-save-buffer)

  (defun my-pdf-tools-setup ()
    ;; auto slice page white spans
    (pdf-view-auto-slice-minor-mode 1)
    ;; Vim like basic scroll keys.
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
    ;; change key [k] to [K] to avoid mis-press.
    ;; (define-key pdf-view-mode-map (kbd "k") nil)
    (pdf-outline-minor-mode 1))
  (add-hook 'pdf-view-mode-hook #'my-pdf-tools-setup)
  
  ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
  (defun my/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (my/pdf-generate-bookmark-name))))
  (defun my/pdf-jump-last-viewed-bookmark ()
    (interactive)
    (when (and (not (and (boundp 'org-noter-doc) org-noter-doc)) ; Fix not compatible with `org-noter'.
               (my/pdf-has-last-viewed-bookmark))
      (bookmark-jump (my/pdf-generate-bookmark-name))))
  (defun my/pdf-has-last-viewed-bookmark ()
    (assoc (my/pdf-generate-bookmark-name) bookmark-alist))
  (defun my/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))
  (defun my/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer (and (buffer-name buf) buf)
        (my/pdf-set-last-viewed-bookmark))))
  (add-hook 'pdf-view-mode-hook 'my/pdf-jump-last-viewed-bookmark)
  (add-hook 'kill-buffer-hook 'my/pdf-set-last-viewed-bookmark) ; Cause "selecting deleted buffer" error.
  (unless noninteractive  ; as `save-place-mode' does
    (add-hook 'kill-emacs-hook #'my/pdf-set-all-last-viewed-bookmarks))

  
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\.pdf\\(<[^>]+>\\)?$" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*Outline .*pdf\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*PDF-Occur\\*" . (display-buffer-reuse-window display-buffer-below-selected)))
  )


;;; [ org-pdfview ] -- org-link support for pdf-view-mode

(use-package org-pdfview
  :ensure t
  :after org
  :load (org-pdfview)
  :init
  (org-link-set-parameters "pdfview"
                           :follow #'org-pdfview-open
                           :export #'org-pdfview-export
                           :complete #'org-pdfview-complete-link
                           :store #'org-pdfview-store-link)
  ;; change Org-mode default open PDF file function.
  ;; If you want, you can also configure the org-mode default open PDF file function.
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link))))
  )

;; [ org-noter ] -- Emacs document annotator, using Org-mode.

(use-package org-noter
  :ensure t
  :defer t
  :preface (unless (boundp 'Org-prefix)
             (define-prefix-command 'Org-prefix))
  :bind (:map Org-prefix ("n" . org-noter)))

;; [ pdf-tools-org ] -- integrate pdf-tools annotations with Org-mode.

(use-package pdf-tools-org
  :quelpa (pdf-tools-org :fetcher github :repo "machc/pdf-tools-org" :upgrade nil)
  :defer t
  :init
  (defun my/pdf-tools-org-setup ()
    (when (eq major-mode 'pdf-view-mode)
      (pdf-tools-org-export-to-org)))
  (add-hook 'after-save-hook #'my/pdf-tools-org-setup))

;; [ paperless ] -- Emacs assisted PDF document filing.

(use-package paperless
  :ensure t
  :defer t
  :commands (paperless)
  :config
  (setq paperless-capture-directory "~/Downloads"
        paperless-root-directory "~/Org"))


(provide 'init-emacs-pdf)

;;; init-emacs-pdf.el ends here
