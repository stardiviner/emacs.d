;;; init-org-mode.el --- init for Org-mode
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


;; (use-package org
;;   :preface
;;   ;; [ Org-mode modules ] -- modules that should always be loaded together with org.el.
;;   ;; t: greedy load all modules.
;;   ;; nil: disable all extra org-mode modules to speed-up Org-mode file opening.
;;   (setq org-modules nil)
;;   :load-path "~/Code/Emacs/org-mode/lisp/"
;;   :pin manual
;;   ;; :mode (("\\.org$" . org-mode))
;;   :config
;;   (use-package org-plus-contrib
;;     :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
;;     :no-require t
;;     :pin manual)
;;   ;; add source code version Org-mode Info into Emacs.
;;   (with-eval-after-load 'info
;;     (info-initialize)
;;     (add-to-list 'Info-directory-list
;;                  "~/Code/Emacs/org-mode/doc/"))
;;   ;; reload org-mode.
;;   ;; (unload-feature 'org)
;;   (org-reload)
;;
;;   ;; TODO: a workaround of different versions org-mode incompatible.
;;   (with-eval-after-load "org"
;;     (setq org-structure-template-alist
;;           '((?a . "export ascii")
;;             (?c . "center")
;;             (?C . "comment")
;;             (?e . "example")
;;             (?E . "export")
;;             (?h . "export html")
;;             (?l . "export latex")
;;             (?q . "quote")
;;             (?s . "src")
;;             (?v . "verse"))))
;;   )

(unless (boundp 'Org-prefix)
  (define-prefix-command 'Org-prefix))
(global-set-key (kbd "C-c o") 'Org-prefix)


(let ((org-dir "~/Org"))
  (if (file-exists-p org-dir)
      (setq org-directory org-dir)
    (make-directory org-dir)))

(require 'init-org-document-structure)
(require 'init-org-view)

(require 'init-org-face)
(require 'init-org-face-extra)

(defun my-org-mode-set-face (theme)
  "Reload customized faces on `circadian' `THEME' toggling."
  (load "init-org-face")
  (load "init-org-face-extra"))
(add-hook 'circadian-after-load-theme-hook 'my-org-mode-set-face)

(require 'init-org-keybindings)
(require 'init-org-complete)
(require 'init-org-table)
(require 'init-org-hyperlink)
;; (require 'init-org-bibliography)
(require 'init-org-todo)
(require 'init-org-tag)
(require 'init-org-property)
(require 'init-org-time)
(require 'init-org-clock)
(require 'init-org-babel)
(require 'init-org-latex)
(require 'init-org-image)
(require 'init-org-capture)
(require 'init-org-agenda)
(require 'init-org-export)
(require 'init-org-publish)
(require 'init-org-search)
(require 'init-org-attach)
(require 'init-org-protocol)
(require 'init-org-extensions)
(require 'init-org-programming)
(require 'init-org-mindmap)
(require 'init-org-presentation)
(require 'init-org-contacts)
(require 'init-org-password)
(require 'init-org-drill)
;; (require 'init-org-mobile)
(require 'init-org-trello)



(provide 'init-org-mode)

;;; init-org-mode.el ends here
