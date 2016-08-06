;;; init-my-math-proof-assistant.el --- init for Proof assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof General ]

(if (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
    (require 'proof-site)
  (warn "You need to install Proof General."))


;;; [ coq-mode ]

;; from system package "coq". "/usr/share/emacs/site-lisp/coq.el"
(require 'coq)


;;; [ company-coq ]

(use-package company-coq
  :ensure t
  :config
  (require 'proof-site)
  (add-hook 'coq-mode-hook #'company-coq-initialize)
  (add-hook 'coq-mode-hook
            '(lambda ()
               (set (make-local-variable 'prettify-symbols-alist)
                    '((":=" . ?≜) ("Proof." . ?∵) ("Qed." . ?■)
                      ("Defined." . ?□) ("Time" . ?⏱) ("Admitted." . ?😱)))))

  (setq company-coq-dynamic-autocompletion t)

  ;; (setq company-coq-autocomplete-modules nil)
  ;; (setq company-coq-autocomplete-context nil)
  ;; (setq company-coq-autocomplete-symbols nil)
  ;; (setq company-coq-autocomplete-block-end nil)
  ;; (setq company-coq-autocomplete-search-results nil)

  )


(provide 'init-my-math-proof-assistant)

;;; init-my-math-proof-assistant.el ends here
