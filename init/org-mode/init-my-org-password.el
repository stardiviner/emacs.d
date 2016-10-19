;;; init-my-org-password.el --- init for Org Passwords Manager
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-org-password-prefix)
  (define-prefix-command 'my-org-password-prefix))
(define-key my-org-prefix (kbd "P") 'my-org-password-prefix)


;;; [ org-passwords ]

;; (use-package org-passwords
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq org-passwords-file "~/Git/dotfiles/passwords.gpg"
;;         ;; org-passwords-time-opened "1 min"
;;         )
;;  
;;   (defun my-org-passwords-search ()
;;     "Search entry in org-passwords."
;;     (interactive)
;;     (org-passwords)
;;     (switch-to-buffer "passwords.gpg")
;;     (if (boundp 'vr/isearch-forward) ; TODO: add thing-at-point support here.
;;         (vr/isearch-forward)
;;       (isearch-forward-regexp)))
;;
;;   (define-key my-org-prefix (kbd "P") 'my-org-passwords-search)
;;  
;;   :config
;;   (define-key org-passwords-mode-map
;;     (kbd "C-c u")
;;     'org-passwords-copy-username)
;;   (define-key org-passwords-mode-map
;;     (kbd "C-c p")
;;     'org-passwords-copy-password)
;;   (define-key org-passwords-mode-map
;;     (kbd "C-c o")
;;     'org-passwords-open-url)
;;
;;   ;; Making new entries in the database
;;   ;; To enter new passwords, you can use 'org-capture' and a minimal template like:
;;   ;;
;;   ;; ("p" "password" entry (file "~/documents/passwords.gpg")
;;   ;;  "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{TAGS}p")
;;
;;   ;; When asked for the password you can then call either
;;   ;; `org-passwords-generate-password' or `org-passwords-random-words'.
;;   ;; Be sure to enable recursive minibuffers to call those functions from the minibuffer:
;;   (setq enable-recursive-minibuffers t)
;;   )


;;; [ org-password-manager ]

(use-package org-password-manager
  :ensure t
  :init
  (define-key my-org-password-prefix (kbd "u") 'org-password-manager-get-username)
  (define-key my-org-password-prefix (kbd "p") 'org-password-manager-get-password)
  (define-key my-org-password-prefix (kbd "s") 'org-password-manager-get-property)
  (define-key my-org-password-prefix (kbd "g") 'org-password-manager-generate-password)

  :config
  (setq org-password-manager-default-pwgen-command
        "pwgen --secure --symbols --capitalize --numerals 25 1")
  )


(provide 'init-my-org-password)

;;; init-my-org-password.el ends here
