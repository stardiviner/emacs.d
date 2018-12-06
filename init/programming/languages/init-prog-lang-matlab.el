;;; init-prog-lang-matlab.el --- init for Matlab

;;; Commentary:



;;; Code:


;;; [ matlab-mode ]

(use-package matlab-mode
  :ensure t
  :defer t
  :mode ("\\.m\\'" . octave-mode)
  :config
  (setq matlab-indent-function t)
  (setq matlab-shell-command "octave")
  (setq matlab-shell-command-switches '())
  (setq matlab-shell-echoes nil))

;;; [ ob-matlab ]

(require 'ob-matlab)
(add-to-list 'org-babel-load-languages '(matlab . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("matlab" . "m"))



(provide 'init-prog-lang-matlab)

;;; init-prog-lang-matlab.el ends here
