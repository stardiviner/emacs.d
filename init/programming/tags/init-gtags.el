;;; init-gtags.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:
;;
;; - $ gtags
;;   go to the root of your linux, or any C project, directory and type gtags.
;;   This will create all the infrastructure that we will soon be using in emacs.


;;; Code:

;;; [ gtags ] -- (GNU Global)

;;; [ gxref ] -- gtags backend for xref.

(use-package gxref
  :ensure t
  :init
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))


;;; [ ggtags ] -- Emacs frontend to GNU Global source code tagging system.

(use-package ggtags
  :ensure t
  :config
  (setq ggtags-navigation-mode nil
        ggtags-enable-navigation-keys nil
        ;; ggtags-mode-prefix-key "M-."
        )
  
  (defun my-ggtags-setup-keybindings ()
    (ggtags-mode -1)
    
    (ggtags-setup-highlight-tag-at-point ggtags-highlight-tag)

    ;; keybindings
    (unless (boundp 'ggtags-prefix)
      (define-prefix-command 'ggtags-prefix))
    (define-key tags-prefix (kbd "g") 'ggtags-prefix)
    
    (define-key ggtags-prefix (kbd "M-.") 'ggtags-find-tag-dwim)
    (define-key ggtags-prefix (kbd "o") 'ggtags-find-other-symbol)
    (define-key ggtags-prefix (kbd "r") 'ggtags-find-reference)
    (define-key ggtags-prefix (kbd "d") 'ggtags-find-definition)
    (define-key ggtags-prefix (kbd "f") 'ggtags-find-file)
    (define-key ggtags-prefix (kbd "r") 'ggtags-find-tag-regexp)
    (define-key ggtags-prefix (kbd "s") 'ggtags-show-definition)
    (define-key ggtags-prefix (kbd "g") 'ggtags-grep)
    (define-key ggtags-prefix (kbd ">") 'ggtags-next-mark)
    (define-key ggtags-prefix (kbd "<") 'ggtags-prev-mark)
    (define-key ggtags-prefix (kbd "h") 'ggtags-view-tag-history)
    (define-key ggtags-prefix (kbd "R") 'ggtags-query-replace)
    (define-key ggtags-prefix (kbd "C") 'ggtags-create-tags)
    (define-key ggtags-prefix (kbd "U") 'ggtags-update-tags)
    
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    )

  (add-hook 'c-mode-hook 'my-ggtags-setup-keybindings)
  (add-hook 'c++-mode-hook 'my-ggtags-setup-keybindings)
  )


;;; [ helm-gtags ] -- helm interface for gtags.

(use-package helm-gtags
  :ensure t
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key ""
        helm-gtags-suggested-key-mapping t
        )

  (defun my-helm-gtags-setup-keybindings ()
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)

    (unless (boundp 'helm-gtags-prefix)
      (define-prefix-command 'helm-gtags-prefix))
    (define-key tags-prefix (kbd "h") 'helm-gtags-prefix)
    
    (define-key helm-gtags-prefix (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-prefix (kbd "a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-prefix (kbd "l") 'helm-gtags-select)
    (define-key helm-gtags-prefix (kbd "s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-prefix (kbd "p") 'helm-gtags-find-pattern)
    (define-key helm-gtags-prefix (kbd "t") 'helm-gtags-find-tag)
    (define-key helm-gtags-prefix (kbd "r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-prefix (kbd "f") 'helm-gtags-find-files)
    (define-key helm-gtags-prefix (kbd "<") 'helm-gtags-previous-history)
    (define-key helm-gtags-prefix (kbd ">") 'helm-gtags-next-history)
    (define-key helm-gtags-prefix (kbd "C") 'helm-gtags-create-tags)
    (define-key helm-gtags-prefix (kbd "U") 'helm-gtags-update-tags)
    )

  (dolist (hook '(c-mode-hook
                  c++-mode-hook))
    (add-hook hook 'my-helm-gtags-setup-keybindings))

  (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)
  )


(provide 'init-gtags)

;;; init-gtags.el ends here
