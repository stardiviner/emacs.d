;; TODO when electric auto insert ) for (, but when you delete (, electric will not auto delete ) for you.

;; (require 'electric)

(autoload 'electric "electric" t)

;; (electric-pair-mode t) ; Emacs built-in smart auto pairs complete
;; (electric-indent-mode t)

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


(provide 'init-my-emacs-electric)
