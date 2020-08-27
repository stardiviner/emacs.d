;;; init-math-proof-assistant.el --- init for Proof assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof General ]

(use-package proof-general
  :ensure t
  ;; :load-path (lambda () (expand-file-name "site-lisp/ProofGeneral/generic" user-emacs-directory))
  :defer t
  ;; :preface
  ;; (setq proof-splash-enable nil)
  :init
  (load-file (expand-file-name "site-lisp/ProofGeneral/generic/proof-site.el" user-emacs-directory))
  (require 'proof-site nil t)
  (proof-ready-for-assistant 'coq)
  (require 'proof-config)
  (require 'proof-shell)
  (require 'coq-syntax)
  (require 'coq)
  )

;; (use-package coq
;;   :load-path (lambda () (expand-file-name "site-lisp/ProofGeneral/coq" user-emacs-directory))
;;   :defer t
;;   :commands (run-coq)
;;   :init
;;   (require 'coq)
;;   ;; (require 'coq-inferior)
;;   (require 'inferior-coq) ; TODO: a temporary fix workaround.
;;   ;; (autoload 'coq-mode "gallina" "Major mode for editing Coq vernacular." t)
;;   (autoload 'run-coq "inferior-coq" "Run an inferior Coq process." t)
;;   (add-to-list 'display-buffer-alist '("\\*coq\\*" . (display-buffer-below-selected))))

;; [ company-coq ]
(use-package company-coq
  :ensure t
  :defer t
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (add-hook 'coq-mode-hook
            '(lambda ()
               (set (make-local-variable 'prettify-symbols-alist)
                    '((":=" . ?≜) ("Proof." . ?∵) ("Qed." . ?■)
                      ("Defined." . ?□) ("Time" . ?⏱) ("Admitted." . ?😱)))))
  :config
  (setq company-coq-dynamic-autocompletion t)
  ;; (setq company-coq-autocomplete-modules nil)
  ;; (setq company-coq-autocomplete-context nil)
  ;; (setq company-coq-autocomplete-symbols nil)
  ;; (setq company-coq-autocomplete-block-end nil)
  ;; (setq company-coq-autocomplete-search-results nil)
  )

;;; [ ob-coq ]

(use-package ob-coq
  :defer t
  :commands (org-babel-execute:coq)
  :config
  (add-to-list 'org-babel-load-languages '(coq . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("coq" . "coq")))

;;; disable auto save.
(require 'coq-compile-common)
(setq coq-compile-auto-save 'ignore)

;;; [ coq-commenter ] -- Emacs commeting support tools for Coq proof assistance.

(use-package coq-commenter
  :ensure t
  :bind (:map coq-commenter-mode-map
              ("C-;" . coq-commenter-comment-proof-in-region)
              ("C-x C-;" . coq-commenter-comment-proof-to-cursor)
              ("C-'" . coq-commenter-uncomment-proof-in-region)
              ("C-x C-'" . coq-commenter-uncomment-proof-in-buffer)
              )
  :config
  (add-hook 'coq-mode-hook 'coq-commenter-mode)
  )


(provide 'init-math-proof-assistant)

;;; init-math-proof-assistant.el ends here
