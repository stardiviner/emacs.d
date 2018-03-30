;;; init-authoring.el --- init for Authoring & Writing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-if ] -- Interactive Fiction Authoring System for Emacs and Org-Mode.

(use-package org-if
  :ensure t
  :defer t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((org-if . t)))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )

;;; [ org-wc ] count words under every org-mode trees

(use-package org-wc
  :ensure t
  :defer t)

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

;;; [ writegood-mode ] -- polish up poor writing on the fly (weasel words, passive voice, duplicates).

;; (use-package writegood-mode
;;   :ensure t
;;   :init
;;   (mapc
;;    (lambda (hook)
;;      (add-hook hook #'writegood-mode))
;;    '(org-mode-hook markdown-mode-hook text-mode-hook))
;;   ;; :config
;;   ;; (add-to-list 'writegood-weasel-words "test")
;;   )

;;; [ writeroom-mode ] -- distraction-free writing for Emacs.

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode global-writeroom-mode))

;;; [ olivetti ] -- Olivetti is a simple Emacs minor mode for a nice writing environment.

(use-package olivetti
  :ensure t
  :commands (olivetti-mode))

;;; [ poet-theme ] -- A theme well suited for modes using variable pitch: particularly org-mode and markdown-mode.

(use-package poet-theme
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'olivetti
    (add-hook 'olivetti-mode-hook
              (lambda ()
                (load-theme 'poet)
                (variable-pitch-mode 1) ; only in Org-mode instead of global!!!
                (set-face-attribute 'default nil :height 130)
                (set-face-attribute 'fixed-pitch nil :family "Fira Code")
                (set-face-attribute 'variable-pitch nil :family "Georgia")
                )))
  ;; TODO: how to disable upper settings after disable `olivetti-mode'?
  (defun poet-theme-disable ()
    (interactive)
    (set-face-attribute 'default nil :height 100)
    ;; reset custom-theme.
    (disable-theme 'poet)
    (circadian-setup))
  )


(provide 'init-authoring)

;;; init-authoring.el ends here
