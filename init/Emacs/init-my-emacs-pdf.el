;;; init-my-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ doc-view ]

;; (require 'doc-view)
;;
;; (setq doc-view-continuous t
;;       ;; doc-view--image-type ; 'png, 'tiff
;;       ;; doc-view-doc-type ; Can be `dvi', `pdf', or `ps'.
;;       ;; doc-view-cache-directory "/tmp/docview1000"
;;       doc-view-scale-internally t
;;       doc-view-image-width 850
;;       )


;;; [ pdf-tools ] -- Emacs support library for PDF files.

(use-package pdf-tools
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; (setq pdf-tools-modes
  ;;       pdf-tools-enabled-modes)
  
  ;; [ PDF View ]
  ;; - [SPC] :: scroll continuous
  ;; - [n/p] :: scroll to next/previous page
  (setq pdf-view-display-size 'fit-width
        pdf-view-continuous t
        pdf-cache-image-limit 64
        pdf-cache-prefetch-delay 0.5
        pdf-view-image-relief 2
        pdf-view-bounding-box-margin 0.05
        ;; pdf-view-use-imagemagick nil
        pdf-view-use-scaling t
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

  
  (add-hook 'pdf-tools-enabled-hook
            (lambda ()
              ;;; Recolor
              ;; (pdf-info-setoptions :render/usecolors t
              ;;                      :render/background "white"
              ;;                      :render/foreground "black"
              ;;                      )
              ))
  
  (set-face-attribute 'pdf-view-rectangle nil
                      :background "yellow"
                      )
  (set-face-attribute 'pdf-view-region nil
                      :background "orange"
                      )
  
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              ;; change key [k] to [K] to avoid mis-press.
              (define-key pdf-view-mode-map (kbd "k") nil)

              ;; "auto" slice from bounding box
              (pdf-view-auto-slice-minor-mode)

              ;; use midnight mode theme
              ;; (pdf-view-midnight-minor-mode)

              ;; `pdf-tools-enabled-modes'
              (pdf-isearch-minor-mode)
              (pdf-occur-global-minor-mode)
              (pdf-outline-minor-mode)
              (pdf-links-minor-mode)
              (pdf-annot-minor-mode)
              (pdf-misc-minor-mode)
              (pdf-sync-minor-mode)
              ))
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
  ;; integrate it into Org-mode seamlessly.
  ;; (org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export)
  ;; change Org-mode default open PDF file function.
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
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


(provide 'init-my-emacs-pdf)

;;; init-my-emacs-pdf.el ends here
