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
  (hook-modes c-dialects-mode
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      (ggtags-mode 1))
    )

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag-dwim)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )


;;; [ helm-gtags ] -- helm interface for gtags.

(use-package helm-gtags
  :ensure t
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t
        )

  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (define-key helm-gtags-mode-map (kbd "C-c . a") 'helm-gtags-tags-in-this-function)
  
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  )


(provide 'init-my-prog-tags-gtags)

;;; init-my-prog-tags-gtags.el ends here
