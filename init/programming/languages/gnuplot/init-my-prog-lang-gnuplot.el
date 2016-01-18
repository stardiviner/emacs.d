;;; init-my-prog-lang-gnuplot.el --- init Emacs for gnuplot

;;; Commentary:

;;; Code:

;;; [ gnuplot-mode ]

(use-package gnuplot-mode
  :ensure t
  :config
  (require 'gnuplot)
  
  ;; add gnuplot info file.
  ;; FIXME: this does not work for `gnuplot-info-lookup-symbol'
  ;; (with-eval-after-load 'info
  ;;   (info-initialize)
  ;;   (add-to-list 'Info-directory-list "~/.emacs.d/elpa/gnuplot-*/"))
  (setq gnuplot-info-display 'window)

  (setq auto-mode-alist (append (list
                                 '("\\.gp\\'" . gnuplot-mode)
                                 '("\\.plt\\'" . gnuplot-mode)
                                 '("\\.gnuplot\\'" . gnuplot-mode)
                                 )
                                auto-mode-alist))

  (setq gnuplot-tab-completion t)
  
  (add-hook 'gnuplot-mode-hook
            '(lambda ()
               ;; context sensitive for gnuplot completion & eldoc-mode.
               (gnuplot-context-sensitive-mode 1)
               
               (define-key gnuplot-mode-map (kbd "<f5>") 'gnuplot-make-buffer)
               (define-key gnuplot-mode-map (kbd "C-h d d") 'gnuplot-info-lookup-symbol)
               (define-key gnuplot-mode-map (kbd "C-c M-i") 'gnuplot-inline-display-mode)
               ))

  ;; auto enable `gnuplot-inline-display-mode' in gnuplot comint process buffer.
  (add-hook 'gnuplot-comint-mode-hook 'gnuplot-inline-display-mode)

  (define-key gnuplot-mode-map (kbd "C-c C-c") 'gnuplot-show-gnuplot-buffer)
  )



(provide 'init-my-prog-lang-gnuplot)

;;; init-my-prog-lang-gnuplot.el ends here
