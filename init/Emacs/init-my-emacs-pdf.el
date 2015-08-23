;;; init-my-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ doc-view ]

;;; Usage:
;;
;; - [] ::

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

;;; Usage:
;;
;; - `pdf-view-mode'
;;   - `pdf-view-auto-slice-minor-mode'
;; - `pdf-tools-enable-minor-modes'
;;   - `pdf-outline-minor-mode'
;;   - `pdf-annot-minor-mode'
;;   - `pdf-cache-prefetch-minor-mode'
;;   - `pdf-history-minor-mode'
;;   - `pdf-isearch-minor-mode'
;;   - `pdf-links-minor-mode'
;;   - `pdf-misc-minor-mode'
;;     - `pdf-misc-context-menu-minor-mode'
;; - use bookmark.el to remember PDF position.


(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(add-hook 'pdf-tools-enabled-hook
          (lambda ()
            ;; Recolor
            (pdf-info-setoptions :render/usecolors t
                                 :render/background "white"
                                 :render/foreground "black"
                                 )))

;;; PDF View

;; - [SPC] :: scroll continuous
;; - [n/p] :: scroll to next/previous page

(setq pdf-view-display-size 'fit-width
      pdf-view-continuous t
      pdf-cache-image-limit 64
      pdf-cache-prefetch-delay 0.5
      pdf-view-image-relief 0
      pdf-view-bounding-box-margin 0.05
      ;; pdf-view-use-imagemagick nil
      pdf-view-use-scaling t
      )

(use-package pdf-tools
  :config
  (set-face-attribute 'pdf-view-rectangle nil
                      :background (color-darken-name (face-background 'default) 5)
                      )
  (set-face-attribute 'pdf-view-region nil
                      :background (color-darken-name (face-background 'default) 3)
                      )
  )

(add-hook 'pdf-view-mode-hook
          (lambda ()
            ;; change key [k] to [K] to avoid mis-press.
            ;; (define-key pdf-view-mode-map [remap image-kill-buffer] 'quit-window)
            (define-key pdf-view-mode-map (kbd "k") nil)
            (define-key pdf-view-mode-map (kbd "K") 'image-kill-buffer)

            ;; "auto" slice from bounding box
            (pdf-view-auto-slice-minor-mode)

            ;; use midnight mode theme
            ;; (pdf-view-midnight-minor-mode)
            ))

;;; PDF Tools

(setq
 pdf-tools-enabled-modes
 '(pdf-outline-minor-mode
   pdf-isearch-minor-mode
   pdf-links-minor-mode
   pdf-annot-minor-mode
   pdf-history-minor-mode
   pdf-misc-minor-mode
   pdf-misc-size-indication-minor-mode
   pdf-misc-menu-bar-minor-mode
   pdf-misc-context-menu-minor-mode
   pdf-sync-minor-mode
   pdf-cache-prefetch-minor-mode
   pdf-occur-global-minor-mode))

;;; PDF Tools - isearch

(setq pdf-isearch-hyphenation-character "--")

;; (set-face-attribute 'pdf-isearch-batch nil
;;                     )
;; (set-face-attribute 'pdf-isearch-lazy nil
;;                     )
;; (set-face-attribute 'pdf-isearch-match nil
;;                     )

(add-hook 'pdf-isearch-minor-mode-hook
          (lambda ()
            ;; revert to emacs default isearch from custom global search utility.
            (define-key pdf-isearch-minor-mode-map (kbd "C-s") 'isearch-forward)))

;;; PDF Tools -- outline

(setq pdf-outline-buffer-indent 2
      pdf-outline-display-labels t ; the outline should display labels instead of page numbers.
      pdf-outline-enable-imenu t
      pdf-outline-imenu-use-flat-menus nil ; Imenu should be a tree or flatted.
      )

;;; PDF Tools - annotation

(setq pdf-annot-activate-created-annotations t
      ;; pdf-annot-activate-handler-functions nil
      ;; pdf-annot-attachment-display-buffer-action nil
      pdf-annot-default-markup-annotation-properties '((label . "stardiviner") (popup-is-open))
      pdf-annot-default-text-annotation-properties '((icon . "Note")
                                                     (color . "#ff0000")
                                                     (label . "stardiviner")
                                                     (popup-is-open))
      ;; pdf-annot-edit-contents-display-buffer-action '((display-buffer-reuse-window display-buffer-split-below-and-attach)
      ;;                                                 (inhibit-same-window . t)
      ;;                                                 (window-height . 0.25))
      ;; pdf-annot-list-display-buffer-action '((display-buffer-reuse-window display-buffer-pop-up-window)
      ;;                                        (inhibit-same-window . t))
      pdf-annot-list-listed-types '(text file squiggly highlight underline strike-out
                                         3d circle square free-text ink line link poly-line polygon popup
                                         stamp printer-mark watermark widget
                                         sound movie)
      pdf-annot-minor-mode-map-prefix (kbd "C-c C-a")
      ;; pdf-annot-print-annotation-functions '(pdf-annot-print-annotation-latex-maybe)
      pdf-annot-tweak-tooltips t
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

(eval-after-load 'org
  '(require 'org-pdfview))

;; integrate it into Org-mode seamlessly.
;; (org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export)
;; change Org-mode default open PDF file function.
(add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
(add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))


(provide 'init-my-emacs-pdf)

;;; init-my-emacs-pdf.el ends here
