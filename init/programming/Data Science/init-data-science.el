;;; init-data-science.el --- init Emacs for Data Science

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ jupyter ] -- An interface to communicate with Jupyter kernels in Emacs.

(use-package jupyter
  :ensure t
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-associate-buffer)
  :init
  (use-package ob-jupyter
    :defer t
    :commands (org-babel-execute:jupyter-python
               org-babel-execute:jupyter-clojure
               org-babel-execute:jupyter-julia
               org-babel-execute:jupyter-ruby)
    :custom (jupyter-long-timeout 25) ; for Clojure Clojupyter long time startup.
    :init
    (when (featurep 'ob-async)
      (with-eval-after-load 'ob-async
        (add-to-list 'ob-async-no-async-languages-alist "jupyter-python")
        (add-to-list 'ob-async-no-async-languages-alist "jupyter-clojure")
        (add-to-list 'ob-async-no-async-languages-alist "jupyter-julia")
        (add-to-list 'ob-async-no-async-languages-alist "jupyter-ruby")
        (add-to-list 'ob-async-no-async-languages-alist "jupyter-sql"))
      (use-package ob-python
        :commands (org-babel-variable-assignments:python)))
    (add-to-list 'display-buffer-alist '("^\\*jupyter-repl.*\\*" . (display-buffer-below-selected)))
    :config
    (add-to-list 'org-babel-load-languages '(jupyter . t) 'append)
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
  (use-package jupyter-org-extensions
    :defer t
    :commands (jupyter-org-interaction-mode
               jupyter-org-hydra/body jupyter-org-insert-src-block)
    :bind (:map org-babel-map ("M-j" . jupyter-org-insert-src-block))))

;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

;; (use-package ein
;;   :ensure t
;;   :defer t
;;   :defines ein:%notebook% ; FIXME: void variable `ein:%notebook%'
;;   :commands (ein:jupyter-server-start
;;              org-babel-execute:ein-python org-babel-execute:ein-clojure)
;;   :load (ein-notebook) ; fix void variable `ein:%notebook%'
;;   ;; :config
;;   ;; ;; [ ob-ein ] #+begin_src ein[-??]
;;   ;; (require 'ob-ein)
;;   ;; (setq ein:org-inline-image-directory "data/images")
;;   ;; (ein:org-register-lang-mode "ein-python" 'python)
;;   ;; (ein:org-register-lang-mode "ein-R" 'R)
;;   ;; (ein:org-register-lang-mode "ein-clojure" 'clojure)
;;   ;; ;; Dynamic JavaScript
;;   ;; (require 'ein-skewer)
;;   ;; (setq ein:enable-dynamic-javascript t)
;;   )


(provide 'init-data-science)

;;; init-data-science.el ends here
