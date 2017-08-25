;;; init-my-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pdf-tools ] -- Emacs support library for PDF files.

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  
  ;; [ PDF View ]
  ;; - [SPC] :: scroll continuous
  ;; - [n/p] :: scroll to next/previous page
  (setq pdf-view-display-size 'fit-width
        pdf-view-continuous t
        ;; pdf-view-image-relief 2
        ;; pdf-view-bounding-box-margin 0.05
        pdf-view-use-imagemagick nil
        pdf-view-use-scaling t
        ;; pdf-view-resize-factor 1.25

        ;; mouse-wheel-follow-mouse t
        )

  ;; [ PDF Tools ]
  ;; [ isearch ]
  (require 'pdf-isearch)
  (add-hook 'pdf-isearch-minor-mode-hook
            (lambda ()
              ;; revert to emacs default isearch from custom global search utility.
              (define-key pdf-isearch-minor-mode-map (kbd "C-s") 'isearch-forward)
              ))

  ;; [ outline ]
  (require 'pdf-outline)
  (setq pdf-outline-display-labels t
        pdf-outline-enable-imenu t
        pdf-outline-imenu-use-flat-menus nil
        )

  ;; [ annotation ]
  (require 'pdf-annot)
  (setq pdf-annot-activate-created-annotations t
        pdf-annot-tweak-tooltips t
        pdf-annot-minor-mode-map-prefix (kbd "C-c C-a")
        )

  (defun my-pdf-tools-setup ()
    ;; Vim like basic scroll keys.
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

    ;; change key [k] to [K] to avoid mis-press.
    ;; (define-key pdf-view-mode-map (kbd "k") nil)

    ;; "auto" slice from bounding box
    (pdf-view-auto-slice-minor-mode)

    ;; use midnight mode theme
    ;; (pdf-view-midnight-minor-mode)

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
    (when
        (my/pdf-has-last-viewed-bookmark)
      (bookmark-jump (my/pdf-generate-bookmark-name))))

  (defun my/pdf-has-last-viewed-bookmark ()
    (assoc
     (my/pdf-generate-bookmark-name) bookmark-alist))

  (defun my/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

  (defun my/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (my/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook 'my/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'my/pdf-jump-last-viewed-bookmark)
  (unless noninteractive  ; as `save-place-mode' does
    (add-hook 'kill-emacs-hook #'my/pdf-set-all-last-viewed-bookmarks))
  )


;;; export annotations at once
;;
;; 1. Try M-x pp-eval-expression (pdf-annot-getannots nil '(text)) RET
;;
;; 2.
;; (mapconcat
;;  (lambda (x)
;;    (alist-get 'contents x))
;;  (pdf-annot-getannots nil nil)
;;  "\n\n\nNext note:\n")


;;; [ org-pdfview ] -- org-link support for pdf-view-mode

;;; Usage:
;;
;; - [[file:filename.pdf::(page-number)]]
;; - org-pdfview will remember file reading position with `org-pdfview-store-link'.
;;   But it will disappear after close Emacs session.
;; - `org-pdfview-open'
;; - `org-pdfview-export'

(use-package org-pdfview
  :ensure t
  :config
  (with-eval-after-load 'org
    (require 'org-pdfview))

  ;; change Org-mode default open PDF file function.
  ;; If you want, you can also configure the org-mode default open PDF file function.
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link))))
  ;; DEPRECATED
  ;; (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  ;; (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

  (org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export)
  )

;;; [ pdf-tools-org ] -- integrate pdf-tools annotations with Org-mode.

(use-package pdf-tools-org
  :quelpa (pdf-tools-org :fetcher github :repo "machc/pdf-tools-org")
  :config
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'pdf-view-mode)
                (pdf-tools-org-export-to-org))))
  )

;;; display PDFs on the side window of Emacs.

(defvar pdf-minimal-width 72
  "Minimal width of a window displaying a pdf.
If an integer, number of columns.  If a float, fraction of the
original window.")

(defvar pdf-split-width-threshold 120
  "Minimum width a window should have to split it horizontally
for displaying a pdf in the right.")

(defun pdf-split-window-sensibly (&optional window)
  "A version of `split-window-sensibly' for pdfs.
It prefers splitting horizontally, and takes `pdf-minimal-width'
into account."
  (let ((window (or window (selected-window)))
        (width (- (if (integerp pdf-minimal-width)
                      pdf-minimal-width
                    (round (* pdf-minimal-width (window-width window)))))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right width)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))

(defun display-buffer-pop-up-window-pdf-split-horizontally (buffer alist)
  "Call `display-buffer-pop-up-window', using `pdf-split-window-sensibly'
when needed."
  (let ((split-height-threshold nil)
        (split-width-threshold pdf-split-width-threshold)
        (split-window-preferred-function #'pdf-split-window-sensibly))
    (display-buffer-pop-up-window buffer alist)))

(add-to-list 'display-buffer-alist
             '("\\.pdf\\(<[^>]+>\\)?$" .
               (display-buffer-pop-up-window-pdf-split-horizontally)))


;;; [ paperless ] -- Emacs assisted PDF document filing.

(use-package paperless
  :ensure t
  :config
  ;; (setq paperless-root-directory
  ;;       paperless-capture-directory
  ;;       )
  )


(provide 'init-my-emacs-pdf)

;;; init-my-emacs-pdf.el ends here
