;;; init-prog-lang-ML.el --- init for ML.

;;; Time-stamp: <2018-11-14 20:30:29 stardiviner>

;;; Commentary:



;;; Code:

;;; [ sml-mode ] -- Major mode for editing Standard ML.

(use-package sml-mode
  :ensure t
  :commands (run-sml)
  :init (setq sml-program-name "smlnj")
  :config
  ;; `ob-sml' use `*sml*' session to execute source block.
  (advice-add 'run-sml :after
              (lambda (cmd arg &optional host)
                (if (string= sml-program-name "smlnj")
                    (switch-to-buffer-other-window "*smlnj*"))
                (rename-buffer "*sml*"))))

;;; [ ob-sml ] -- Org Mode Babel support for Standard ML.

(use-package ob-sml
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(sml . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("sml" . "sml")))



(provide 'init-prog-lang-ML)

;;; init-prog-lang-ML.el ends here
