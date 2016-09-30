;;; init-my-prog-tags-gtags.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:
;;
;; - $ gtags
;;   go to the root of your linux, or any C project, directory and type gtags.
;;   This will create all the infrastructure that we will soon be using in emacs.


;;; Code:

;;; [ gtags ] -- (GNU Global)


;;; [ ggtags ] -- Emacs frontend to GNU Global source code tagging system.

(use-package ggtags
  :ensure t
  :config
  (setq ggtags-mode-prefix-key "M-."
        ggtags-navigation-mode nil
        ggtags-enable-navigation-keys nil)
  
  (defun my-ggtags-setup-keybindings ()
    (ggtags-mode -1)
    
    (ggtags-setup-highlight-tag-at-point ggtags-highlight-tag)

    ;; keybindings
    (unless (boundp 'ggtags-prefix)
      (define-prefix-command 'ggtags-prefix))
    (define-key my-prog-lookup-map (kbd "g") 'ggtags-prefix)
    
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

;; (use-package helm-gtags
;;   :ensure t
;;   :config
;;   (setq helm-gtags-ignore-case t
;;         helm-gtags-auto-update t
;;         helm-gtags-use-input-at-cursor t
;;         helm-gtags-pulse-at-cursor t
;;         helm-gtags-prefix-key ""
;;         helm-gtags-suggested-key-mapping t
;;         )
;;
;;   (defun my-helm-gtags-setup-keybindings ()
;;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;
;;     (define-key my-prog-lookup-map (kbd "M-.") 'helm-gtags-dwim)
;;     (define-key my-prog-lookup-map (kbd "a") 'helm-gtags-tags-in-this-function)
;;     (define-key my-prog-lookup-map (kbd "l") 'helm-gtags-select)
;;     (define-key my-prog-lookup-map (kbd "s") 'helm-gtags-find-symbol)
;;     (define-key my-prog-lookup-map (kbd "p") 'helm-gtags-find-pattern)
;;     (define-key my-prog-lookup-map (kbd "t") 'helm-gtags-find-tag)
;;     (define-key my-prog-lookup-map (kbd "r") 'helm-gtags-find-rtag)
;;     (define-key my-prog-lookup-map (kbd "f") 'helm-gtags-find-files)
;;     (define-key my-prog-lookup-map (kbd "<") 'helm-gtags-previous-history)
;;     (define-key my-prog-lookup-map (kbd ">") 'helm-gtags-next-history)
;;     (define-key my-prog-lookup-map (kbd "C") 'helm-gtags-create-tags)
;;     (define-key my-prog-lookup-map (kbd "U") 'helm-gtags-update-tags)
;;     )
;;
;;   (dolist (hook '(c-mode-hook
;;                   c++-mode-hook))
;;     (add-hook hook 'my-helm-gtags-setup-keybindings))
;;
;;   (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)
;;   )


(provide 'init-my-prog-tags-gtags)

;;; init-my-prog-tags-gtags.el ends here
