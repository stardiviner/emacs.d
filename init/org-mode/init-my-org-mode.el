;;; init-my-org-mode.el --- init for Org-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (use-package org-plus-contrib
    :ensure t
    :no-require t)
  )


(unless (boundp 'Org-prefix)
  (define-prefix-command 'Org-prefix))
(global-set-key (kbd "C-c o") 'Org-prefix)


;;; [ Org Modules ]
;; Modules that should always be loaded together with org.el.
(setq org-modules
      '(org-pcomplete
        org-faces
        ;; org-fstree
        org-table ; org-compat
        org-protocol
        org-timer org-clock org-habit org-notify
        org-info org-bibtex org-docview
        org-plot
        org-irc ; org-gnus org-mhe org-rmail
        ;; org-w3m
        ))


(let ((org-dir "~/Org"))
  (if (file-exists-p org-dir)
      (setq org-directory org-dir)
    (make-directory org-dir)))

(require 'init-my-org-document-structure)
(require 'init-my-org-view)

(require 'init-my-org-face)
(require 'init-my-org-face-extra)

(defun my-org-mode-set-face (&args)
  (load "init-my-org-face")
  (load "init-my-org-face-extra"))
(add-hook 'circadian-after-load-theme-hook 'my-org-mode-set-face)

(require 'init-my-org-keybindings)
(require 'init-my-org-complete)
(require 'init-my-org-table)
(require 'init-my-org-hyperlink)
;; (require 'init-my-org-bibliography)
(require 'init-my-org-todo)
(require 'init-my-org-tag)
(require 'init-my-org-property)
(require 'init-my-org-time)
(require 'init-my-org-clock)
(require 'init-my-org-babel)
(require 'init-my-org-latex)
(require 'init-my-org-image)
(require 'init-my-org-capture)
(require 'init-my-org-agenda)
(require 'init-my-org-export)
(require 'init-my-org-publish)
(require 'init-my-org-search)
(require 'init-my-org-attach)
(require 'init-my-org-protocol)
(require 'init-my-org-extensions)
(require 'init-my-org-programming)
(require 'init-my-org-mindmap)
(require 'init-my-org-presentation)
(require 'init-my-org-contacts)
(require 'init-my-org-password)
(require 'init-my-org-drill)
;; (require 'init-my-org-mobile)
(require 'init-org-trello)



(provide 'init-my-org-mode)

;;; init-my-org-mode.el ends here
