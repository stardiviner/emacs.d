;;; init-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pdf-tools ] -- Emacs support library for PDF files.

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-tools-install-noverify pdf-view-mode)
  :preface (pdf-loader-install)
  :init (pdf-tools-install-noverify) ; (pdf-tools-install)
  (setq pdf-view-use-scaling t ; open PDFs scaled to fit page.
        pdf-view-use-unicode-ligther nil ; speed-up pdf-tools by don't try to find unicode.
        )
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode) ; for PDF binary header byte.
  :config
  ;; helpful accessibility shortcuts
  (define-key pdf-view-mode-map (kbd "q") 'kill-current-buffer)

  ;; set the view mode colors to fit your color-theme for `pdf-view-midnight-minor-mode'.
  (setq pdf-view-midnight-colors
        (cons
         (frame-parameter nil 'foreground-color)
         (frame-parameter nil 'background-color)))
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))
  
  (add-hook 'pdf-view-mode-hook #'pdf-annot-minor-mode)
  ;; save after adding annotation comment
  (advice-add 'pdf-annot-edit-contents-commit :after 'save-buffer)

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
  
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\.pdf\\(<[^>]+>\\)?$" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*Outline .*pdf\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*PDF-Occur\\*" . (display-buffer-reuse-window display-buffer-below-selected)))
  )

;;; [ pdf-view-restore ] -- support for opening last known pdf position in pdf-view-mode.

(use-package pdf-view-restore
  :ensure t
  :defer t
  :after pdf-tools
  :init (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;; [ org-pdfview ] -- org-link support for pdf-view-mode

(use-package org-pdfview
  :ensure t
  :defer t
  :after org
  :commands (org-pdfview-open
             org-pdfview-export org-pdfview-complete-link org-pdfview-store-link)
  :init
  (org-link-set-parameters "pdfview"
                           :follow #'org-pdfview-open
                           :export #'org-pdfview-export
                           :complete #'org-pdfview-complete-link
                           :store #'org-pdfview-store-link)
  ;; change Org-mode default open PDF file function.
  ;; If you want, you can also configure the org-mode default open PDF file function.
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link)))))

;; [ org-noter ] -- Emacs document annotator, using Org-mode.

(use-package org-noter
  :ensure t
  :defer t
  :commands (org-noter)
  :preface (unless (boundp 'Org-prefix) (define-prefix-command 'Org-prefix))
  :bind (:map Org-prefix ("n" . org-noter))
  :init (setq org-noter-auto-save-last-location t))

;; [ pdf-tools-org ] -- integrate pdf-tools annotations to exporting/importing with Org Mode.

(use-package pdf-tools-org
  :ensure t
  :defer t
  :commands (pdf-tools-org-export-to-org pdf-tools-org-import-from-org)
  :config
  (defun my/pdf-tools-org-setup ()
    (when (eq major-mode 'pdf-view-mode)
      (pdf-tools-org-export-to-org)))
  (add-hook 'after-save-hook #'my/pdf-tools-org-setup))

;; [ paperless ] -- Emacs assisted PDF document filing.

;; (use-package paperless
;;   :ensure t
;;   :defer t
;;   :commands (paperless)
;;   :init (setq paperless-capture-directory "~/Downloads"
;;               paperless-root-directory "~/Org"))

;;; [ pdfgrep ] -- Grep PDF for searching PDF.

(use-package pdfgrep
  :ensure t
  :defer t
  :commands (pdfgrep pdfgrep-mode)
  :init (add-hook 'pdf-view-mode-hook #'pdfgrep-mode))


(provide 'init-emacs-pdf)

;;; init-emacs-pdf.el ends here
