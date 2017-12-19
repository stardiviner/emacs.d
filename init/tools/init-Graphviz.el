;;; init-Graphviz.el --- init for Graphviz tools.

;;; Commentary:



;;; Code:

;;; [ graphviz-dot-mode ]

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :init
  (setq default-tab-width 2)
  
  (defun graphviz-dot-mode-find-file ()
    (if (string= "dot" (file-name-extension buffer-file-name))
        (progn
          (message "Enabling Setings for dot-mode")
          (setq fill-column 1000)
          (base-auto-pair)
          (local-set-key (kbd "<C-f6>") 'compile)
          )))
  
  (add-hook 'find-file-hook #'graphviz-dot-mode-find-file)
  )


;;; [ ob-dot ]

;;; Example
;; #+BEGIN_SRC dot :file some_filename.png :cmdline -Kdot -Tpng
;;   <context of graphviz source goes here>
;; #+END_SRC

(require 'ob-dot)



(provide 'init-Graphviz)

;;; init-Graphviz.el ends here
