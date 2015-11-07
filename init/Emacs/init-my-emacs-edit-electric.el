;;; init-my-emacs-edit-electric.el --- init for Electric
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Electric ]

;; NOTE: when electric auto insert ) for (, but when you delete (, electric will not auto delete ) for you.

;; NOTICE: this conflict with ParEdit and auto-pair.
;; (when (fboundp 'electric-pair-mode)
;;   (setq-default electric-pair-mode 1))
;; ------------------------------------------
;; (electric-pair-mode t) ; automatically insert delimiter pairs.

;; (electric-indent-mode t)

;; (dolist (hook
;;          '(org-mode-hook
;;            ruby-mode-hook
;;            python-mode-hook
;;            html-mode-hook
;;            css-mode-hook
;;            c-mode-hook
;;            ;; ess-mode-hook                ; Emacs Speaks Statistics
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
