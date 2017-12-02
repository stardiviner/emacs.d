;;; init-my-org-extensions.el --- init for Org Extensions
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-crypt ] -- public key encryption for Org entries

(require 'org-crypt)

;; You can change the tag to any complex tag matching string by
;; setting the `org-crypt-tag-matcher' variable.
(setq org-crypt-tag-matcher "encrypt")

;;; add `org-crypt' required tag to default tag list.
(setq org-tag-alist
      (append '((:startgroup . nil)
                ("org-crypt" . nil)
                (:grouptags . nil)
                ("encrypt" . ?C)
                (:endgroup . nil))
              org-tag-alist))

;;; set your public keyring.
(setq org-crypt-key "F09F650D7D674819892591401B5DF1C95AE89AC3")

;;; To automatically encrypt all necessary entries when saving a file.
(org-crypt-use-before-save-magic)

;;; set keybindings for org-crypt functions.
(define-key org-mode-map (kbd "C-c C-r") 'org-encrypt-entry)
(define-key org-mode-map (kbd "C-c M-r") 'org-decrypt-entry)



(provide 'init-my-org-extensions)

;;; init-my-org-extensions.el ends here
