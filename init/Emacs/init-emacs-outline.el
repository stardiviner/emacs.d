;;; init-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (unless (boundp 'outline-prefix)
;;   (define-prefix-command 'outline-prefix))
;; (global-set-key (kbd "C-C SPC") 'outline-prefix)


;;; [ allout ]

(use-package allout
  :ensure t
  :defer t
  :delight allout-minor-mode
  ;; :init (add-hook 'prog-mode-hook #'allout-minor-mode)
  (setq allout-auto-activation t
        allout-command-prefix (kbd "C-c SPC"))
  :config
  (setq allout-default-layout '(-2 : -1 *)
        ;; [buffer-local] allout-layout '(0 : -1 -1 0)
        allout-widgets-auto-activation t)
  (setq-default allout-use-mode-specific-leader nil
                allout-stylish-prefixes t
                allout-primary-bullet "*" ; used by level-1
                allout-header-prefix "."
                allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
                allout-plain-bullets-string-len 5
                allout-plain-bullets-string "*+#>." ; + -> #N -> > -> *
                ))

;;; [ outline ] -- outline mode commands for Emacs.

(use-package outline ; [ C-c @]
  :delight outline-minor-mode
  :init (add-hook 'prog-mode-hook 'outline-minor-mode))

;;; [ hideshow ] -- minor mode cmds to selectively display code/comment blocks.

;; (use-package hideshow
;;   :ensure t
;;   :defer t
;;   :delight hs-minor-mode
;;   :init (add-hook 'prog-mode-hook #'hs-minor-mode)
;;   ;; :config
;;   ;; (use-package hideshowvis
;;   ;;   :ensure t)
;;   )



(provide 'init-emacs-outline)

;;; init-emacs-outline.el ends here
