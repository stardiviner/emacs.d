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
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode) ; for PDF binary header byte.
  ;; :init  (pdf-tools-install-noverify) ; (pdf-tools-install)
  :config
  (setq pdf-view-use-scaling t ; open PDF scaled to fit page.
        ;; speed-up pdf-tools by don't try to find unicode.
        pdf-view-use-unicode-ligther nil)
  
  ;; helpful accessibility shortcuts
  (define-key pdf-view-mode-map (kbd "q") 'kill-current-buffer)

  ;; set the view mode colors to fit your color-theme for `pdf-view-midnight-minor-mode'.
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  (defun my/pdf-view-midnight-colors-reset (theme)
    "Reset pdf-view midnight colors to fit color themes."
    (interactive)
    (setq pdf-view-midnight-colors
          (cons (frame-parameter nil 'foreground-color)
                (frame-parameter nil 'background-color)))
    ;; programatically refresh (auto revert buffer) PDF buffers.
    (mapc
     (lambda (buffer)
       (if-let ((filename (buffer-file-name buffer)))
           (if (string-equal (file-name-extension filename) "pdf")
               (with-current-buffer buffer
                 (revert-buffer nil 'noconfirm)))))
     (buffer-list)))
  (add-hook 'load-theme-after-hook #'my/pdf-view-midnight-colors-reset)
  (if (featurep 'circadian)
      (add-hook 'circadian-after-load-theme-hook #'my/pdf-view-midnight-colors-reset))
  
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
               '("\\*PDF-Occur\\*" . (display-buffer-reuse-window display-buffer-below-selected))))

;;; [ pdf-view-restore ] -- support for opening last known pdf position in pdf-view-mode.

(use-package pdf-view-restore
  :ensure t
  :defer t
  :after pdf-tools
  :init (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;; [ org-noter ] -- Emacs document annotator, using Org-mode.

(use-package org-noter
  :ensure t
  :defer t
  :commands (org-noter)
  :preface (unless (boundp 'Org-prefix) (define-prefix-command 'Org-prefix))
  :bind (:map Org-prefix ("n" . org-noter))
  :init (setq org-noter-auto-save-last-location t))

;; [ paperless ] -- Emacs assisted PDF document filing.

;; (use-package paperless
;;   :ensure t
;;   :defer t
;;   :commands (paperless)
;;   :init (setq paperless-capture-directory "~/Downloads"
;;               paperless-root-directory "~/Org"))

;;; [ org-pdftools ] -- A custom org link type for pdf-tools.

;; (use-package org-pdftools
;;   ;; :quelpa (org-pdftools :fetcher github :repo "fuxialexander/org-pdftools")
;;   :load-path "~/Code/Emacs/org-pdftools"
;;   :init
;;   (with-eval-after-load 'org
;;     (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdftools-open link))))))

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

;;; [ pdfgrep ] -- Grep PDF for searching PDF.

(use-package pdfgrep
  :ensure t
  :defer t
  :commands (pdfgrep pdfgrep-mode)
  :init (add-hook 'pdf-view-mode-hook #'pdfgrep-mode))


(provide 'init-emacs-pdf)

;;; init-emacs-pdf.el ends here
