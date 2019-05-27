;;; init-prog-lang-xml.el --- init XML programming language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nxml ] -- A new XML mode.

(use-package nxml
  :mode ("\\.xml\\'" . nxml-mode)
  :defer t
  :config
  ;; `company-nxml'
  (defun my-company-nxml-settings ()
    (require 'company-nxml)
    (setq-local company-minimum-prefix-length 1)
    (add-to-list (make-local-variable 'company-backends) 'company-nxml))
  (add-hook 'nxml-mode-hook #'my-company-nxml-settings)

  ;; format XML with `xmllint'.
  (defun my:xml-lint ()
    "Format an XML buffer with `xmllint'."
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     "xmllint -format -" (current-buffer) t
     "*Xmllint Error Buffer*" t))
  (define-key nxml-mode-map (kbd "C-c m f") 'my:xml-lint)

  ;; open Org XML source block with `nxml-mode'.
  (with-eval-after-load 'nxml-mode
    (add-to-list 'org-src-lang-modes '("xml" . nxml))))

;;; [ x-path-walker ] -- navigation for JSON/XML/HTML based on path (imenu like)

(use-package x-path-walker
  :ensure t
  :defer t
  :commands (helm-x-path-walker)
  :init
  (dolist (hook '(html-mode-hook
                  web-mode-hook
                  nxml-mode-hook
                  json-mode-hook))
    (add-hook hook (lambda () (local-set-key (kbd "C-c C-j") 'helm-x-path-walker)))))


(provide 'init-prog-lang-xml)

;;; init-prog-lang-xml.el ends here
