;;; init-my-emacs-outline.el --- init Emacs for outline.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'outline-prefix)
  (define-prefix-command 'outline-prefix))
(global-set-key (kbd "C-C C-h") 'outline-prefix)


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

;;; [ outline ] -- outline mode commands for Emacs.

(use-package outline ; [ C-c @]
  :config
  (set-face-attribute 'outline-1 nil
                      :background "#268bd2" :height 1.25
                      :weight 'bold
                      )
  (set-face-attribute 'outline-2 nil
                      :background "#2aa198" :height 1.15
                      :weight 'bold
                      )
  (set-face-attribute 'outline-3 nil
                      :background "#b58900" :height 1.05
                      :weight 'bold
                      )

  (add-hook 'prog-mode-hook 'outline-minor-mode)
  )

;;; [ outshine ] -- outline with outshine outshines outline.

(use-package helm-navi
  :ensure t
  :bind (:map outline-prefix
              ;; ("C-j" . helm-navi) ; this is very slow!!!
              ("C-h" . helm-navi-headings)
	      :map org-mode-map
	      ("M-j" . helm-navi-headings))
  :config
  (use-package navi-mode
    :ensure t
    :config
    ;; Add "use-package" lines to `navi-keywords'.
    (setf (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
          "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? "))
  (use-package outshine
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    :config
    ;; (add-hook 'outline-minor-mode-hook 'outshine-hook-function) ; This will add font lock colors to comments outline.
    )
  )



(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
