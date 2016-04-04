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
  (setq gnuplot-info-display 'window
        gnuplot-tab-completion t)

  ;; context sensitive for gnuplot completion & eldoc-mode.
  (gnuplot-context-sensitive-mode 1)
  
  (add-hook 'gnuplot-mode-hook
            (lambda ()
              (define-key gnuplot-mode-map (kbd "<f5>") 'gnuplot-make-buffer)
              (define-key gnuplot-mode-map (kbd "C-h d d") 'gnuplot-info-lookup-symbol)
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
