;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key tools-prefix (kbd "p") 'paste-prefix)


;;; convert selected region to Markdown and copy to clipboard for pasting
;;; on sites like GitHub, and Stack Overflow.

(defun my-org-md-convert-region-to-md ()
  "Convert selected region to Markdown and copy to clipboard.

For pasting on sites like GitHub, and Stack Overflow."
  (interactive)
  (require 'ox-md)
  
  ;; (with-temp-buffer-window
  ;;  "*org->markdown temp*"
  ;;  (org-export-to-buffer 'md "*org->markdown temp*"
  ;;    async subtreep visible-only) 'delete-window)

  (unless (org-region-active-p) (user-error "No active region to replace"))
  (x-set-selection 'CLIPBOARD
                   (org-export-string-as
                    (buffer-substring (region-beginning) (region-end)) 'md t))
  (deactivate-mark))

(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key paste-prefix (kbd "m") 'my-org-md-convert-region-to-md)

;;; copy formatted text from org-mode to applications.

(defun my-org-formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*org-mode formatted copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))




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

  ;; Fix `yagist' detect major-mode issue.
  (defun yagist-anonymous-file-name-for-org-babel ()
    "Fix `yagist' detect major-mode issue."
    (let* ((ext (cdr (assoc
                      (replace-regexp-in-string "-mode" "" (symbol-name major-mode))
                      org-babel-tangle-lang-exts))
                ;; (cdr (assoc (nth 0 (org-babel-get-src-block-info)) org-babel-tangle-lang-exts))
                ))
      (setq buffer-file-name
            (concat (file-name-sans-extension (buffer-file-name)) (format ".%s" ext)))
      ))
  (advice-add 'yagist-anonymous-file-name :before #'yagist-anonymous-file-name-for-org-babel)
  )

;;; [ webpaste ] -- paste text to pastebin-like services.

;; (use-package webpaste
;;   :ensure t
;;   :defer t
;;   :bind (:map paste-prefix
;;               ("C-b" . webpaste-paste-buffer)
;;               ("C-r" . webpaste-paste-region))
;;   )


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
