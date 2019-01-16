;;; init-data-science.el --- init Emacs for Data Science

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

(use-package ein
  :ensure t
  ;; [ ob-ein ] #+begin_src ein[-??]
  :load (ob-ein)
  :commands (ein:jupyter-server-start)
  :config
  (setq ein:org-inline-image-directory "data/images")
  ;; Python backend
  (ein:org-register-lang-mode "ein-python" 'python)
  ;; R backend
  (ein:org-register-lang-mode "ein-R" 'R)
  ;; Clojure backend
  (ein:org-register-lang-mode "ein-clojure" 'clojure)
  ;; Dynamic JavaScript
  (require 'ein-skewer)
  (setq ein:enable-dynamic-javascript t))

;;; [ ob-ipython ]

;; (use-package ob-ipython
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-to-list 'org-babel-load-languages '(ipython . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (add-to-list 'org-babel-tangle-lang-exts '("ipython" . "ipynb"))
;;   :config
;;   ;; by default open ipython block block with `python-mode'
;;   ;; (add-to-list 'org-src-lang-modes '("ipython" . python))
;;   ;; use IJulia backend for IPython notebook
;;   ;; (add-to-list 'org-src-lang-modes '("ipython" . julia))
;;
;;   (setq org-babel-default-header-args:ipython
;;         '((:exports . "both")
;;           ;; (:session . nil)
;;           ;; (:dir . "data/images")
;;           ))
;;
;;   ;; support ox-latex + minted.
;;   (add-to-list 'org-latex-minted-langs '(ipython "python")))


;;; [ Apache Hadoop ]


;;; [ Apache Pig ]

(use-package pig-mode
  :ensure t
  :defer t)

;;; [ Apache Hive ]

(use-package hive
  :ensure t
  :defer t)


(provide 'init-data-science)

;;; init-data-science.el ends here
