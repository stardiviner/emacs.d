;;; init-my-math-proof-assistant.el --- init for Proof assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof General ]

;; (add-to-list 'package-archives
;;              '("emacs-pe" . "https://emacs-pe.github.io/packages/"))
;; (use-package coq
;;   :ensure t
;;   :defer t
;;   :pin emacs-pe)

(use-package proof-general
  :load-path "~/.emacs.d/site-lisp/ProofGeneral/generic"
  :init
  (require 'proof-site nil t)
  )

(use-package coq
  :load-path "~/.emacs.d/site-lisp/ProofGeneral/coq"
  :init
  (require 'coq nil t)
  )

;; (use-package proof-general
;;   :load-path "/usr/share/emacs/site-lisp/ProofGeneral/generic"
;;   :init
;;   (require 'proof-site)
;;   )

;; (use-package coq
;;   :load-path "/usr/share/emacs/site-lisp/ProofGeneral/coq"
;;   :init
;;   ;; FIXME: temporary fix solution for (proof-ass completion-table).
;;   (defvar coq-completion-table nil)
;;   (require 'coq)
;;   )


;;; [ company-coq ]

(use-package company-coq
  :ensure t
  :defer t
  :init
  (require 'proof-site nil t)
  
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


(provide 'init-my-math-proof-assistant)

;;; init-my-math-proof-assistant.el ends here
