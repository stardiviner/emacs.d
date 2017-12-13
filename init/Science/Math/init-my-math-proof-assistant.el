;;; init-my-math-proof-assistant.el --- init for Proof assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof General ]

(use-package proof-general
  :ensure-system-package ((proofgeneral . "yaourt -S --noconfirm proofgeneral")
                          (coqc . "sudo pacman -S --noconfirm coq coq-doc coqide"))
  :load-path "~/.emacs.d/site-lisp/ProofGeneral/generic"
  :defer t
  :config
  (require 'proof-site nil t)

  ;; [ company-coq ]
  (use-package company-coq
    :ensure t
    :config
    (setq company-coq-dynamic-autocompletion t)
    ;; (setq company-coq-autocomplete-modules nil)
    ;; (setq company-coq-autocomplete-context nil)
    ;; (setq company-coq-autocomplete-symbols nil)
    ;; (setq company-coq-autocomplete-block-end nil)
    ;; (setq company-coq-autocomplete-search-results nil)

    (add-hook 'coq-mode-hook
              '(lambda ()
                 (set (make-local-variable 'prettify-symbols-alist)
                      '((":=" . ?≜) ("Proof." . ?∵) ("Qed." . ?■)
                        ("Defined." . ?□) ("Time" . ?⏱) ("Admitted." . ?😱)))))

    (require 'proof-site nil t)
    (add-hook 'coq-mode-hook #'company-coq-mode)
    )
  )


;;; [ ob-coq ]
(require 'ob-coq)
(add-to-list 'org-babel-load-languages '(coq . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("coq" . "coq"))

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


(provide 'init-my-math-proof-assistant)

;;; init-my-math-proof-assistant.el ends here
