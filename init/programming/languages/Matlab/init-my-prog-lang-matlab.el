;;; init-my-prog-lang-matlab.el --- init for Matlab
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ matlab-mode ]

(require 'matlab)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))


(setq matlab-indent-function t)
(setq matlab-shell-command "octave")
(setq matlab-shell-command-switches '())
(setq matlab-shell-echoes nil)



(provide 'init-my-prog-lang-matlab)

;;; init-my-prog-lang-matlab.el ends here
