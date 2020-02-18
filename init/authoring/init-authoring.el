;;; init-authoring.el --- init for Authoring & Writing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ captain ] -- CAPiTalization is Automatic IN emacs.

(use-package captain
  :ensure t
  :commands (captain-capitalize-word)
  :config (global-captain-mode))

;;; [ org-if ] -- Interactive Fiction Authoring System for Emacs and Org-Mode.

(use-package org-if
  :ensure t
  :defer t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((org-if . t)))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(if (featurep 'doom-modeline)
    (with-eval-after-load 'doom-modeline
      (setq doom-modeline-enable-word-count t)))

;;; [ org-wc ] count words under every org-mode trees

(use-package org-wc
  :ensure t
  :defer t
  :commands (org-wc-display org-wc-count-subtrees))

;;; [ wc-mode ] -- minor mode of command `wc' for word counting.

;; (use-package wc-mode
;;   :ensure t
;;   :init (wc-mode nil))

;;; [ writeroom-mode ] -- distraction-free writing for Emacs.

(use-package writeroom-mode
  :ensure t
  :defer t
  :commands (writeroom-mode global-writeroom-mode))

;;; [ olivetti ] -- Olivetti is a simple Emacs minor mode for a nice writing environment.

(use-package olivetti
  :ensure t
  :defer t
  :commands (olivetti-mode))

;;; [ poet-theme ] -- A theme well suited for modes using variable pitch: particularly org-mode and markdown-mode.

;; (use-package poet-theme
;;   :ensure t
;;   :config (load-theme 'poet t))


(provide 'init-authoring)

;;; init-authoring.el ends here
