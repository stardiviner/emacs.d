;;; init-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key tools-prefix (kbd "p") 'paste-prefix)

;;; convert selected region to Markdown and copy to clipboard for pasting
;;; on sites like GitHub, and Stack Overflow.
(use-package ox-gfm
  :ensure t
  :defer t
  :init
  (defun my:org-convert-region-to-gfm ()
    "Convert selected region to GitHub Flawed Markdown and copy to clipboard.
For pasting on sites like GitHub, and Stack Overflow."
    (interactive)
    (require 'ox-gfm)
    (unless (org-region-active-p) (user-error "No active region selected"))
    (gui-set-selection
     'CLIPBOARD
     (org-export-string-as
      (buffer-substring (region-beginning) (region-end))
      'gfm t
      '(:with-toc nil)))
    (deactivate-mark))
  (define-key paste-prefix (kbd "m") 'my:org-convert-region-to-gfm))

(defun my:org-convert-region-to-md ()
  "Convert selected region to Markdown and copy to clipboard.
For pasting on sites like GitHub, and Stack Overflow."
  (interactive)
  (require 'ox-md)
  (unless (org-region-active-p) (user-error "No active region selected"))
  (gui-set-selection
   'CLIPBOARD
   (org-export-string-as
    (buffer-substring (region-beginning) (region-end))
    'md t
    '(:with-toc nil)))
  (deactivate-mark))

(define-key paste-prefix (kbd "M") 'my:org-convert-region-to-md)

;;; copy formatted text from org-mode to applications.
(defun my:org-convert-region-to-html ()
  "Export region to HTML, and copy it to the clipboard.
For pasting source code in Email."
  (interactive)
  (require 'ox-html)
  (unless (org-region-active-p) (user-error "No active region selected"))
  (gui-set-selection
   'CLIPBOARD
   (htmlize-region-for-paste (region-beginning) (region-end)))
  (deactivate-mark))

(define-key paste-prefix (kbd "h") 'my:org-convert-region-to-html)
;;; `htmlize-buffer' (convert current buffer into HTML output)
(define-key paste-prefix (kbd "H") 'htmlize-buffer)


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



(provide 'init-tool-paste)

;;; init-tool-paste.el ends here
