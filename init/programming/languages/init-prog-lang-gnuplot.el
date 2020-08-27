;;; init-prog-lang-gnuplot.el --- init Emacs for gnuplot

;;; Commentary:

;;; Code:

;;; [ gnuplot ]

(use-package gnuplot
  :ensure t
  :defer t
  :init (add-to-list 'display-buffer-alist '("\\*gnuplot\\*" . (display-buffer-below-selected))))


;;; [ gnuplot-mode ]

(use-package gnuplot-mode
  :ensure t
  :preface (define-derived-mode gnuplot-mode prog-mode "Gnuplot")
  :mode (("\\.gp\\'" . gnuplot-mode)
         ("\\.plt\\'" . gnuplot-mode)
         ("\\.gnuplot\\'" . gnuplot-mode))
  :commands (gnuplot-mode run-gnuplot)
  :bind (:map gnuplot-mode-map
              ("<f5>" . gnuplot-make-buffer)
              ("C-h d d" . gnuplot-info-at-point)
              ("C-c C-d" . gnuplot-info-lookup-symbol)
              ("C-c C-/" . gnuplot-help-function)
              ("C-c M-i" . gnuplot-inline-display-mode)
              ("C-c C-s" . run-gnuplot)
              ("C-c C-z" . run-gnuplot)
              ("C-c C-c" . gnuplot-show-gnuplot-buffer))
  :init
  (setq gnuplot-info-display 'window
        gnuplot-tab-completion t
        gnuplot-use-context-sensitive-completion nil
        gnuplot-eldoc-mode t)
  :config
  (defun my-gnuplot-mode-settings ()
    ;; context sensitive for gnuplot completion & eldoc-mode.
    ;; - `completion-at-point'
    ;; - `gnuplot-info-at-point' / [C-c C-/]
    ;; - `gnuplot-help-function' / [C-c C-d]
    ;;
    ;; (gnuplot-context-sensitive-mode 1)

    ;; (setq gnuplot-completion-at-point-function
    ;;       #'gnuplot-context-completion-at-point)
    (add-hook 'completion-at-point-functions
              'gnuplot-context-completion-at-point nil t)
    
    ;; Setup Eldoc
    (set (make-local-variable 'eldoc-documentation-function)
         'gnuplot-eldoc-function)
    (eldoc-add-command 'completion-at-point)     ; Check for eldoc after completion
    (when (fboundp 'comint-dynamic-complete)
      (eldoc-add-command 'comint-dynamic-complete)))
  (add-hook 'gnuplot-mode-hook #'my-gnuplot-mode-settings)

  ;; auto enable `gnuplot-inline-display-mode' in gnuplot comint process buffer.
  (add-hook 'gnuplot-comint-mode-hook 'gnuplot-inline-display-mode))


;;; [ ob-gnuplot ]

(use-package org-plus-contrib
  :pin manual
  :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
  :no-require t
  :init
  (use-package ob-gnuplot
    :defer t
    :commands (org-babel-execute:gnuplot)
    :config
    (add-to-list 'org-babel-load-languages '(gnuplot . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    (add-to-list 'org-babel-tangle-lang-exts '("gnuplot" . "gnuplot"))
    (add-to-list 'org-babel-default-header-args:gnuplot
                 '(:exports . "both"))))



(provide 'init-prog-lang-gnuplot)

;;; init-prog-lang-gnuplot.el ends here
