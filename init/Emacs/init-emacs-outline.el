;;; init-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (unless (boundp 'outline-prefix)
;;   (define-prefix-command 'outline-prefix))
;; (global-set-key (kbd "C-C SPC") 'outline-prefix)


;;; [ allout ]

(use-package allout ; [C-c SPC] prefix
  :ensure t
  :ensure allout-widgets
  :defer t
  :delight allout-minor-mode
  :commands (allout-mode allout-minor-mode)
  :custom ((allout-auto-activation t)
           (allout-command-prefix (kbd "C-c SPC"))
           (allout-default-layout '(-2 : -1 *)) ; [buffer-local] allout-layout '(0 : -1 -1 0)
           (allout-widgets-auto-activation t)
           (allout-use-mode-specific-leader nil)
           (allout-stylish-prefixes t)
           (allout-primary-bullet "*")  ; used by level-1
           (allout-header-prefix ".")
           (allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^")
           (allout-plain-bullets-string-len 5)
           (allout-plain-bullets-string "*+#>.") ; + -> #N -> > -> *
           )
  :hook (;; (prog-mode . allout-mode)
         (allout-mode . allout-widgets-mode)))

;;; [ outline ] -- outline mode commands for Emacs.

(use-package outline ; [C-c @] prefix
  :defer t
  :delight outline-minor-mode
  :init (add-hook 'prog-mode-hook 'outline-minor-mode))

;;; [ hideshow ] -- minor mode cmds to selectively display code/comment blocks.

;; (use-package hideshow
;;   :ensure t
;;   :ensure hideshowvis
;;   :defer t
;;   :delight hs-minor-mode
;;   :hook (prog-mode . hs-minor-mode))



(provide 'init-emacs-outline)

;;; init-emacs-outline.el ends here
