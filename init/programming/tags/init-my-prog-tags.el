;;; init-my-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:

;;; Code:
;; like tags: function, variable, class, scope etc lookup.

(unless (boundp 'prog-lookup-prefix)
  (define-prefix-command 'prog-lookup-prefix))
(global-set-key (kbd "C-c l") 'prog-lookup-prefix)

(unless (boundp 'prog-lookup-tags-prefix)
  (define-prefix-command 'prog-lookup-tags-prefix))
(define-key prog-lookup-prefix (kbd "t") 'prog-lookup-tags-prefix)


(setq tags-table-files nil
      tags-table-list nil)

;;; [ xref ]

(require 'xref)

;; disable the following elements to avoid jump to other window when xref.
(setq xref-prompt-for-identifier '(not xref-find-definitions
                                       ;; xref-find-definitions-other-window
                                       ;; xref-find-definitions-other-frame
                                       ))

;;; [ helm-xref ] -- Helm interface for xref results.

;; (use-package helm-xref
;;   :ensure t
;;   :config
;;   (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

;;; [ dumb-jump ] -- An Emacs "jump to definition" package using ag, ripgrep etc.

;; (use-package dumb-jump
;;   :ensure t
;;   :init
;;   (unless (boundp 'jump-prefix)
;;     (define-prefix-command 'jump-prefix))
;;   (global-set-key (kbd "M-g j") 'jump-prefix)
;;   :bind (:map jump-prefix
;;               ("j" . dumb-jump-go)
;;               ("k" . dumb-jump-back)
;;               ("q" . dumb-jump-quick-look)
;;               ("o" . dumb-jump-go-other-window)
;;               ("x" . dumb-jump-go-prefer-external)
;;               ("z" . dumb-jump-go-prefer-external-other-window))
;;   :config
;;   ;; (setq dumb-jump-selector 'popup) 'ivy
;;   (dumb-jump-mode 1)
;;   )


;;; [ tags settings ]

(setq tags-add-tables t ; always add new tags to tables
      ;; tags-included-tables
      ;; tags-table-list (list
      ;;                  (expand-file-name "/usr/share/lib/TAGS"))
      tags-revert-without-query t
      ;; tags-completion-table
      tags-apropos-verbose t
      )

;;; Build Tags


;; NOTE: use etags & gtags, because company-mode support.
(require 'init-my-prog-tags-etags)
;; (require 'init-my-prog-tags-gtags)
;; (require 'init-my-prog-tags-ctags)
;; (require 'init-my-prog-tags-cscope)
(require 'init-my-prog-tags-rtags)


(provide 'init-my-prog-tags)

;;; init-my-prog-tags.el ends here
