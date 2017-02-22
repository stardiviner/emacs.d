;;; init-my-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'outline-prefix)
  (define-prefix-command 'outline-prefix))
(global-set-key (kbd "C-c @") 'outline-prefix)


;;; [ allout ]

(use-package allout
  :ensure t
  :config
  (setq allout-auto-activation t
        ;; allout-layout
        allout-default-layout '(-2 : -1 *)
        ;; [buffer-local] allout-layout '(0 : -1 -1 0)
        allout-widgets-auto-activation t
        allout-command-prefix (kbd "C-c SPC")
        )
  (setq-default allout-use-mode-specific-leader nil
                allout-stylish-prefixes t
                allout-primary-bullet "*" ; used by level-1
                allout-header-prefix "."
                allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
                allout-plain-bullets-string-len 5
                allout-plain-bullets-string "*+#>." ; + -> #N -> > -> *
                )
  
  (allout-minor-mode 1)
  )


(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
