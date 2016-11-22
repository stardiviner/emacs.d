;;; init-my-math-proof-assistant.el --- init for Proof assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof General ]

(use-package proof-general
  :load-path "~/.emacs.d/site-lisp/ProofGeneral/generic"
  :init
  (require 'proof-site nil t)
  )


;;; [ company-coq ]

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

;;; [ ob-coq ]

(use-package org-plus-contrib
  :ensure t
  :config
  (require 'ob-coq))


(provide 'init-my-math-proof-assistant)

;;; init-my-math-proof-assistant.el ends here
