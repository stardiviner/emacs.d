;;; init-my-org-password.el --- init for Org Passwords Manager
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-org-password-prefix)
  (define-prefix-command 'my-org-password-prefix)
  (define-key my-org-prefix (kbd "P") 'my-org-password-prefix))


;;; [ org-passwords ]

;;; Usage:
;; - [M-x org-passwords] ::
;;   - [C-x C-q] :: switch read-only mode. for editing file.
;; - [M-x org-passwords-copy-password] ::
;; - [M-x org-passwords-open-url] ::
;; - [M-x org-passwords-generate-password] ::

;; (require 'org-passwords)
;;
;; (setq org-passwords-file "~/Git/dotfiles/passwords.gpg"
;;       ;; org-passwords-default-password-size "20"
;;       ;; org-passwords-random-words-dictionary "/etc/dictionaries-common/words"
;;       ;; org-passwords-random-words-substitutions '(("for" . "4") ("s" . "5"))
;;       ;; org-passwords-password-property "PASSWORD"
;;       ;; org-passwords-username-property "USERNAME"
;;       ;; org-passwords-url-property "URL"
;;       org-passwords-time-opened "1 min"
;;       )
;;
;; (eval-after-load "org-passwords"
;;   '(progn
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c u")
;;        'org-passwords-copy-username)
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c p")
;;        'org-passwords-copy-password)
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c o")
;;        'org-passwords-open-url)))

;;; Making new entries in the database
;;; To enter new passwords, you can use 'org-capture' and a minimal template like:
;;
;; ("p" "password" entry (file "~/documents/passwords.gpg")
;;  "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{TAGS}p")

;; When asked for the password you can then call either
;; 'org-passwords-generate-password' or 'org-passwords-random-words'.
;; Be sure to enable recursive minibuffers to call those functions from the minibuffer:
;; (setq enable-recursive-minibuffers t)
;;
;; (defun my-org-passwords-search ()
;;   "Search entry in org-passwords."
;;   (interactive)
;;   (org-passwords)
;;   (switch-to-buffer "passwords.gpg")
;;   (if (boundp 'vr/isearch-forward) ; TODO: add thing-at-point support here.
;;       (vr/isearch-forward)
;;     (isearch-forward-regexp)))
;;
;; (define-key my-org-prefix (kbd "P") 'my-org-passwords-search)


;;; [ org-password-manager ]

;;; Usage
;;
;; - [C-u] [C-c C-p u] :: get/query username.
;; - [C-u] [C-c C-p p] :: get/query password.
;; - [C-u] [C-c C-p g] :: generate password.

;; (require 'org-password-manager)

;; (add-hook 'org-mode-hook 'org-password-manager-key-bindings)

;; (setq org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 25 1")

;; (define-key my-org-password-prefix (kbd "u") 'org-password-manager-get-username)
;; (define-key my-org-password-prefix (kbd "p") 'org-password-manager-get-password)
;; (define-key my-org-password-prefix (kbd "s") 'org-password-manager-get-property)
;; (define-key my-org-password-prefix (kbd "g") 'org-password-manager-generate-password)



(provide 'init-my-org-password)

;;; init-my-org-password.el ends here
