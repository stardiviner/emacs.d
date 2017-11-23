;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key tools-prefix (kbd "p") 'paste-prefix)


;;; [ yagist ] -- Yet Another gist

(use-package yagist
  :ensure t
  :defer t
  :bind (:map paste-prefix
              ("p" . yagist-region-or-buffer)
              ("P" . yagist-region-or-buffer-private)
              ("r" . yagist-region)
              ("b" . yagist-buffer)
              ("l" . yagist-list))
  :config
  ;; *Encrypt your token by using `kaesar' package with AES encryption algorithm.
  ;; (setq yagist-encrypt-risky-config t)

  (setq yagist-view-gist t ; view gist URL after posted.
        yagist-working-directory "~/.gist"
        ;; yagist-working-directory-alist
        yagist-github-token (my/json-read-value my/account-file 'yagist)
        )
  )

;;; [ webpaste ] -- paste text to pastebin-like services.

(use-package webpaste
  :ensure t
  :defer t
  :bind (:map paste-prefix
              ("C-b" . webpaste-paste-buffer)
              ("C-r" . webpaste-paste-region)))


;;; [ pastery ] -- paste to pastery from Emacs

;; (use-package pastery
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq pastery-api-key "9dMWka2QzhnsFyRb4aKb2Fc6tZMudoZb")
;;   )


;;; htmlize-buffer (convert current buffer into HTML output)
(define-key paste-prefix (kbd "h") 'htmlize-buffer)


(provide 'init-my-tool-paste)

;;; init-my-tool-paste.el ends here
