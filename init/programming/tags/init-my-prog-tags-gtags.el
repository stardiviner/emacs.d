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
  (hook-modes '(c-mode c++-mode objc-mode)
    (ggtags-mode -1)
    
    (ggtags-setup-highlight-tag-at-point ggtags-highlight-tag)

    ;; keybindings
    (unless (boundp 'my-prog-lookup-map)
      (define-prefix-command 'my-prog-lookup-map))
    (local-set-key (kbd "M-.") 'my-prog-lookup-map)
    
    (define-key my-prog-lookup-map (kbd "M-.") 'ggtags-find-tag-dwim)
    (define-key my-prog-lookup-map (kbd "o") 'ggtags-find-other-symbol)
    (define-key my-prog-lookup-map (kbd "r") 'ggtags-find-reference)
    (define-key my-prog-lookup-map (kbd "d") 'ggtags-find-definition)
    (define-key my-prog-lookup-map (kbd "f") 'ggtags-find-file)
    (define-key my-prog-lookup-map (kbd "r") 'ggtags-find-tag-regexp)
    (define-key my-prog-lookup-map (kbd "s") 'ggtags-show-definition)
    (define-key my-prog-lookup-map (kbd "g") 'ggtags-grep)
    (define-key my-prog-lookup-map (kbd ">") 'ggtags-next-mark)
    (define-key my-prog-lookup-map (kbd "<") 'ggtags-prev-mark)
    (define-key my-prog-lookup-map (kbd "h") 'ggtags-view-tag-history)
    (define-key my-prog-lookup-map (kbd "R") 'ggtags-query-replace)
    (define-key my-prog-lookup-map (kbd "C") 'ggtags-create-tags)
    (define-key my-prog-lookup-map (kbd "U") 'ggtags-update-tags)
    
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    )
  
  (hook-modes '(c-mode c++-mode objc-mode) 'my-ggtags-setup-keybindings)
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
;;   ;; Enable helm-gtags-mode
;;   (add-hook 'dired-mode-hook 'helm-gtags-mode)
;;   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;   (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;
;;   (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;
;;   (define-key my-prog-lookup-map (kbd "a") 'helm-gtags-tags-in-this-function)
;;   (define-key my-prog-lookup-map (kbd "l") 'helm-gtags-select)
;;   (define-key my-prog-lookup-map (kbd "h") 'helm-gtags-dwim)
;;   (define-key my-prog-lookup-map (kbd "<") 'helm-gtags-previous-history)
;;   (define-key my-prog-lookup-map (kbd ">") 'helm-gtags-next-history)
;;   )


(provide 'init-my-prog-tags-gtags)

;;; init-my-prog-tags-gtags.el ends here
