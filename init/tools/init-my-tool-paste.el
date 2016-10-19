;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key my-tools-prefix (kbd "p") 'paste-prefix)


;;; [ yagist ] -- Yet Another gist

(use-package yagist
  :ensure t
  :defer t
  :init
  (define-key paste-prefix (kbd "p") 'yagist-region-or-buffer)
  (define-key paste-prefix (kbd "P") 'yagist-region-or-buffer-private)
  (define-key paste-prefix (kbd "r") 'yagist-region)
  (define-key paste-prefix (kbd "b") 'yagist-buffer)
  (define-key paste-prefix (kbd "l") 'yagist-list)
  
  :config
  ;; *Encrypt your token by using `kaesar' package with AES encryption algorithm.
  ;; (setq yagist-encrypt-risky-config t)

  (setq yagist-view-gist t ; view gist URL after posted.
        yagist-working-directory "~/.gist"
        ;; yagist-working-directory-alist
        yagist-github-token (my/json-read-value my/account-file 'yagist)
        )
  )



;;; htmlize-buffer (convert current buffer into HTML output)
(define-key paste-prefix (kbd "h") 'htmlize-buffer)



(provide 'init-my-tool-paste)

;;; init-my-tool-paste.el ends here
