;;; init-my-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Electric ]

;; (electric-indent-mode t)

;; (dolist (hook
;;          '(org-mode-hook
;;            ruby-mode-hook
;;            python-mode-hook
;;            html-mode-hook
;;            css-mode-hook
;;            c-mode-hook
;;            ;; ess-mode-hook
;;            ))
;;   (add-hook hook #'(lambda () (electric-pair-mode t))))


;;; [ skeleton ]

(setq skeleton-pair t
      skeleton-pair-alist
      '((?\" _ "\"" >)
        (?\' _ "\'" >)
        (?\( _ ")" >)
        (?\[ _ "]" >)
        (?\{ _ "}" >)
        ;; chinese pairs
        (?“ _ "”" >)
        (?‘ _ "’" >)
        (?\（ _ "）" >)
        (?\【 _ "】" >)
        (?\〖 _ "〗" >)
        )
      )


(provide 'init-my-emacs-edit-electric)

;;; init-my-emacs-edit-electric.el ends here
