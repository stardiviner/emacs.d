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

  ;; so that you can active/inactive allout-minor-mode to edit/navigate/folding with it.
  ;; (define-key my-edit-prefix (kbd "o") 'allout-minor-mode)
  ;; activate outline mode for current buffer, and establish a default file-var setting for `allout-layout'.
  (defun my-allout-toggle ()
    "Toggle allout for current buffer."
    (interactive)
    (if (allout-mode-p)
        (allout-mode -1)
      (outlineify-sticky)
      (allout-hide-bodies)))

  (define-key allout-mode-map (kbd "C-c SPC C-l") 'allout-hide-bodies)
  
  ;; (define-key my-edit-prefix (kbd "o") 'outlineify-sticky)
  (define-key outline-prefix (kbd "@") 'my-allout-toggle)

  ;; (unless (boundp 'outline-prefix)
  ;;   (define-prefix-command 'outline-prefix))
  ;; (define-key my-edit-prefix (kbd "o") 'outline-prefix)
  ;;
  ;; (define-key outline-prefix (kbd "n") 'allout-next-heading)
  )


(provide 'init-my-emacs-outline)

;;; init-my-emacs-outline.el ends here
;;;_* Dummy outline topic header -- see ‘allout-mode’ docstring: ‘C-h m’.
;;;_* Local emacs vars.
;;;_ + Local variables:
;;;_ + allout-layout: (-1 : 0)
;;;_ + End:

