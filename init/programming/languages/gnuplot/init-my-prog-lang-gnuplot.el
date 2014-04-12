;;; init-my-prog-lang-gnuplot.el --- init Emacs for gnuplot

;;; Commentary:

;;; Code:

;;; [ gnuplot ]

(require 'gnuplot)

;;; [ gnuplot-mode ]

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append (list
                               '("\\.gp$" . gnuplot-mode)
                               '("\\.plt$" . gnuplot-mode)
                               )
                              auto-mode-alist))



(provide 'init-my-prog-lang-gnuplot)

;;; init-my-prog-lang-gnuplot.el ends here
