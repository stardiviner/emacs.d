;;; init-my-prog-lang-xml.el --- init XML programming language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nxml-mode ]

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 2
        nxml-auto-insert-xml-declaration-flag nil
        )

  ;;; XML completion
  (setq nxml-bind-meta-tab-to-complete-flag t ; M-Tab to complete
        nxml-slash-auto-complete-flag nil ; </ to complete
        )
  ;; company-nxml
  (defun my-company-nxml-settings ()
    (setq-local company-minimum-prefix-length 1)
    (add-to-list (make-local-variable 'company-backends)
                 'company-nxml)
    )

  (add-hook 'nxml-mode-hook 'my-company-nxml-settings)

  ;; format XML with `xmllint'.
  (defun my-xml-format ()
    "Format an XML buffer with `xmllint'."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             "xmllint -format -"
                             (current-buffer)
                             t
                             "*Xmllint Error Buffer*"
                             t))

  (define-key nxml-mode-map (kbd "C-c m f") 'my-xml-format)
  )


;;; [ ob-xml ]

(with-eval-after-load 'nxml-mode
  (add-to-list 'org-src-lang-modes '("xml" . nxml)))


;;; [ x-path-walker ] -- navigation for JSON/XML/HTML based on path (imenu like)

(use-package x-path-walker
  :ensure t
  :config
  (dolist (hook '(html-mode-hook
                  web-mode-hook
                  nxml-mode-hook
                  json-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (local-set-key (kbd "C-c C-j") 'helm-x-path-walker))))
  )



(provide 'init-my-prog-lang-xml)

;;; init-my-prog-lang-xml.el ends here
