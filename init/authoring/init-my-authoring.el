;;; init-my-authoring.el --- init for Authoring & Writing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ visual-fill-column ] -- wrapping visual-line-mode buffers at fill-column.

(use-package visual-fill-column
  :ensure t
  :defer t
  :bind (("C-x t v" . visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-center-text nil)
  )


;;; [ org-if ] -- Interactive Fiction Authoring System for Emacs and Org-Mode.

;; (use-package org-if
;;   :ensure t
;;   :defer t
;;   :init
;;   (org-babel-do-load-languages 'org-babel-load-languages '((org-if . t)))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   )


;;; [ wc-mode ] -- minor mode of command `wc' for word counting.

;; (use-package wc-mode
;;   :ensure t
;;   :init
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (when (and (buffer-file-name)
;;                          ;; (string= (file-name-directory (buffer-file-name))
;;                          ;;          "/Users/jcs/org/blog/")
;;                          )
;;                 ;; It’s important to add the hook in the :init section,
;;                 ;; otherwise it won’t work until wc-mode is enabled manually at
;;                 ;; least once.
;;                 (wc-mode 1))))
;;   ;; :bind ("M-=" . wc-mode)
;;   :config
;;   (wc-mode nil)
;;   ;; (run-with-idle-timer (* 60 1) nil
;;   ;;                      'wc-mode-update)
;;   )


(provide 'init-my-authoring)

;;; init-my-authoring.el ends here
