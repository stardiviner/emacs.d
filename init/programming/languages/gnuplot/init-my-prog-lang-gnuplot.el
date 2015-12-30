;;; init-my-prog-lang-gnuplot.el --- init Emacs for gnuplot

;;; Commentary:

;;; Code:

;;; [ gnuplot-mode ]

;; add gnuplot info file.
;; FIXME: this does not work for `gnuplot-info-lookup-symbol'
;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (add-to-list 'Info-directory-list "~/.emacs.d/elpa/gnuplot-*/"))

(setq auto-mode-alist (append (list
                               '("\\.gp\\'" . gnuplot-mode)
                               '("\\.plt\\'" . gnuplot-mode)
                               '("\\.gnuplot\\'" . gnuplot-mode)
                               )
                              auto-mode-alist))

(setq gnuplot-info-display 'window)

(add-hook 'gnuplot-mode-hook
          (lambda ()
            (define-key gnuplot-mode-map (kbd "<f5>") 'gnuplot-make-buffer)
            (define-key gnuplot-mode-map (kbd "C-h d d") 'gnuplot-info-lookup-symbol)
            ))



(provide 'init-my-prog-lang-gnuplot)

;;; init-my-prog-lang-gnuplot.el ends here
