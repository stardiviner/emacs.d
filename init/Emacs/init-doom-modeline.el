;;; init-doom-modeline.el --- init for doom-modeline.

;;; Time-stamp: <2018-12-13 08:45:10 stardiviner>

;;; Commentary:



;;; Code:

;;; [ doom-modeline ] -- A minimal and modern mode-line.

(use-package ghub+
  :ensure t
  :commands ghubp-get-notifications)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-init)
  :init (setq doom-modeline-buffer-file-name-style 'truncate-with-project))



(provide 'init-doom-modeline)

;;; init-doom-modeline.el ends here
