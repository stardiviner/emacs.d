;;; init-my-prog-lang-gnuplot.el --- init Emacs for gnuplot

;;; Commentary:

;;; Code:

;;; [ gnuplot ]

(use-package gnuplot
  :ensure t)


;;; [ gnuplot-mode ]

(use-package gnuplot-mode
  :ensure t
  :init
  (setq auto-mode-alist (append (list
                                 '("\\.gp\\'" . gnuplot-mode)
                                 '("\\.plt\\'" . gnuplot-mode)
                                 '("\\.gnuplot\\'" . gnuplot-mode)
                                 )
                                auto-mode-alist))
  :config
  (require 'gnuplot-context)
  
  (setq gnuplot-info-display 'window
        gnuplot-tab-completion t
        gnuplot-use-context-sensitive-completion nil
        gnuplot-eldoc-mode t
        )

  (add-hook 'gnuplot-mode-hook
            (lambda ()
              ;; context sensitive for gnuplot completion & eldoc-mode.
              ;; - `completion-at-point'
              ;; - `gnuplot-info-at-point' / [C-c C-/]
              ;; - `gnuplot-help-function' / [C-c C-d]
              ;;
              ;; (gnuplot-context-sensitive-mode 1)

              ;; (setq gnuplot-completion-at-point-function
              ;;       #'gnuplot-context-completion-at-point)
              (add-to-list (make-local-variable 'completion-at-point-functions)
                           #'gnuplot-context-completion-at-point)
              
              ;; Setup Eldoc
              (eldoc-mode 1)
              (set (make-local-variable 'eldoc-documentation-function)
                   'gnuplot-eldoc-function)
              (eldoc-add-command 'completion-at-point)     ; Check for eldoc after completion
              (when (fboundp 'comint-dynamic-complete)
                (eldoc-add-command 'comint-dynamic-complete))
              
              
              (define-key gnuplot-mode-map (kbd "<f5>") 'gnuplot-make-buffer)
              (define-key gnuplot-mode-map (kbd "C-h d d") 'gnuplot-info-at-point)
              (define-key gnuplot-mode-map (kbd "C-c C-d") 'gnuplot-info-lookup-symbol)
              (define-key gnuplot-mode-map (kbd "C-c C-/") 'gnuplot-help-function)
              (define-key gnuplot-mode-map (kbd "C-c M-i") 'gnuplot-inline-display-mode)

              (define-key gnuplot-mode-map (kbd "C-c C-s") 'run-gnuplot)
              (define-key gnuplot-mode-map (kbd "C-c C-z") 'run-gnuplot)
              ))

  ;; auto enable `gnuplot-inline-display-mode' in gnuplot comint process buffer.
  (add-hook 'gnuplot-comint-mode-hook 'gnuplot-inline-display-mode)

  (define-key gnuplot-mode-map (kbd "C-c C-c") 'gnuplot-show-gnuplot-buffer)
  )



(provide 'init-my-prog-lang-gnuplot)

;;; init-my-prog-lang-gnuplot.el ends here
