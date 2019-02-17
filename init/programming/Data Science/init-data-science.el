;;; init-data-science.el --- init Emacs for Data Science

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ jupyter ] -- An interface to communicate with Jupyter kernels in Emacs.

(use-package jupyter
  :ensure t
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-repl-associate-buffer)
  :init (require 'ob-jupyter)
  (add-to-list 'org-babel-load-languages '(jupyter . t) 'append)
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'display-buffer-alist
               '("^\\*jupyter-repl.*\\*" (display-buffer-below-selected)))
  (with-eval-after-load 'ob-async
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-python")
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-clojure")
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-julia")
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-ruby")))

;;; [ Emacs IPython Notebook (EIN) ] -- IPython notebook client in Emacs

(use-package ein
  :ensure t
  :defer t
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
