;;; init-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:

;; like tags: function, variable, class, scope etc lookup.

;;; Code:


(unless (boundp 'tags-prefix)
  (define-prefix-command 'tags-prefix))
(global-set-key (kbd "M-g t") 'tags-prefix)

;;; [ tags settings ]

(setq tags-add-tables t ; always add new tags to tables
      tags-revert-without-query t
      tags-apropos-verbose t)

;;; [ xref ]

(use-package xref
  ;; disable the following elements to avoid jump to other window when xref.
  :custom (xref-prompt-for-identifier '(not xref-find-definitions
                                            ;; xref-find-definitions-other-window
                                            ;; xref-find-definitions-other-frame
                                            ))
  :config (add-to-list 'display-buffer-alist
                       '("^\\*xref\\*$" (display-buffer-below-selected))))

;;; [ helm-xref ] -- Helm interface for xref results.

;; (use-package helm-xref
;;   :ensure t
;;   :custom (xref-show-xrefs-function 'helm-xref-show-xrefs))

;;; [ dumb-jump ] -- An Emacs "jump to definition" package using ag, ripgrep etc.

;; (use-package dumb-jump
;;   :ensure t
;;   :init
;;   :bind (:map tags-prefix
;;               ("j" . dumb-jump-go)
;;               ("k" . dumb-jump-back)
;;               ("q" . dumb-jump-quick-look)
;;               ("o" . dumb-jump-go-other-window)
;;               ("x" . dumb-jump-go-prefer-external)
;;               ("z" . dumb-jump-go-prefer-external-other-window))
;;   :custom (dumb-jump-selector 'popup) ; 'ivy
;;   :config (dumb-jump-mode 1))

;;; [ Imenu ] -- [M-x imenu]

(use-package imenu
  :ensure t
  :bind (([remap imenu] . counsel-imenu) ; [C-x j]: `helm-imenu', `counsel-imenu'.
         ("C-c ." . helm-imenu)))

;; NOTE: use etags & gtags, because company-mode support.
;; (require 'init-etags)
;; (require 'init-gtags)
;; (require 'init-ctags)
;; (require 'init-cscope) ; <----- prefer "cscope"
;; (require 'init-rtags)


(provide 'init-prog-tags)

;;; init-prog-tags.el ends here
