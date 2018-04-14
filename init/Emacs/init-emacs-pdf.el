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
  (setq pdf-view-display-size 'fit-width
        pdf-view-continuous t
        ;; pdf-view-image-relief 2
        ;; pdf-view-bounding-box-margin 0.05
        pdf-view-use-imagemagick nil
        pdf-view-use-scaling t ; open PDFs scaled to fit page.
        pdf-view-resize-factor 1.25 ; more fine-grained zooming.
        pdf-view-use-unicode-ligther nil ; to speed-up pdf-tools by don't try to find unicode.
        ;; mouse-wheel-follow-mouse t
        )

  ;; helpful accessibility shortcuts
  (define-key pdf-view-mode-map (kbd "q") 'kill-current-buffer)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  ;; [ PDF Tools ]
  ;; [ isearch ]
  (require 'pdf-isearch)
  ;; revert to emacs default isearch instead of `swiper' from custom global search utility.
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  
  ;; [ outline ]
  (require 'pdf-outline)
  (setq pdf-outline-display-labels t
        pdf-outline-enable-imenu t
        pdf-outline-imenu-use-flat-menus nil
        )

  ;; [ annotation ]
  (require 'pdf-annot)
  (setq pdf-annot-activate-created-annotations t ; automatically annotate highlights.
        pdf-annot-tweak-tooltips t
        pdf-annot-minor-mode-map-prefix (kbd "C-c C-a")
        )

  ;; save after adding annotation comment
  (defun pdf-tools-save-buffer ()
    "This function is used to save pdf-annot commits."
    (save-buffer))
  (advice-add 'pdf-annot-edit-contents-commit :after 'pdf-tools-save-buffer)

  (defun my-pdf-tools-setup ()
    ;; Vim like basic scroll keys.
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

    ;; change key [k] to [K] to avoid mis-press.
    ;; (define-key pdf-view-mode-map (kbd "k") nil)

    ;; "auto" slice from bounding box
    (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode)

    ;; toggle midnight mode theme
    (defun my-pdf-tools-set-face (theme)
      "Set `pdf-tools' faces based on `circadian' color `THEME' switching."
      ;; color-theme adaptive colors.
      (setq pdf-view-midnight-colors `(,(face-background 'default) . ,(face-foreground 'default)))
      ;; green color on black background
      ;; (setq pdf-view-midnight-colors '("#00B800" . "#000000" ))
      ;; amber color on black background
      ;; (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" ))
      ;; original solarized colors
      ;; (setq pdf-view-midnight-colors '("#839496" . "#002b36" ))
      )
    (add-hook 'circadian-after-load-theme-hook #'my-pdf-tools-set-face)
    ;; (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

    ;; `pdf-tools-enabled-modes'
    (pdf-tools-enable-minor-modes)
    ;;
    ;; (pdf-isearch-minor-mode)
    ;; (pdf-occur-global-minor-mode)
    ;; (pdf-outline-minor-mode)
    ;; (pdf-links-minor-mode)
    ;; (pdf-annot-minor-mode)
    ;; (pdf-misc-minor-mode)
    ;; (pdf-sync-minor-mode)
    )

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
               '("\\*PDF-Occur\\*" . (display-buffer-below-selected)))
  )


;;; [ org-pdfview ] -- org-link support for pdf-view-mode

(use-package org-pdfview
  :ensure t
  :load (org-pdfview)
  :init
  (org-link-set-parameters "pdfview" :export #'org-pdfview-export)
  ;; change Org-mode default open PDF file function.
  ;; If you want, you can also configure the org-mode default open PDF file function.
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link))))
  )

;; [ org-noter ] -- Emacs document annotator, using Org-mode.

(use-package org-noter
  :ensure t
  :defer t
  :preface
  (unless (boundp 'Org-prefix)
    (define-prefix-command 'Org-prefix))
  :bind (:map Org-prefix ("n" . org-noter)))

;; [ pdf-tools-org ] -- integrate pdf-tools annotations with Org-mode.

(use-package pdf-tools-org
  :quelpa (pdf-tools-org :fetcher github :repo "machc/pdf-tools-org" :upgrade nil)
  :defer t
  :init
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'pdf-view-mode)
                (pdf-tools-org-export-to-org))))
  )

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
