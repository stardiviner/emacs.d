;;; init-my-prog-lang-xml.el --- init XML programming language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nxml-mode ]

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))

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


;;; [ auto-complete-nxml ]

(use-package auto-complete-nxml
  :ensure t
  :defer t
  :config
  ;; If you want to start completion manually from the beginning
  (setq auto-complete-nxml-automatic-p nil)
  
  ;; Keystroke for popup help about something at point.
  (setq auto-complete-nxml-popup-help-key "M-h")
  
  ;; Keystroke for toggle on/off automatic completion.
  ;; (setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
  )


;;; [ x-path-walker ] -- navigation for JSON/XML/HTML based on path (imenu like)

(use-package x-path-walker
  :ensure t
  :defer t
  :init
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
