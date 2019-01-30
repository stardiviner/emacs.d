;;; init-doom-modeline.el --- init for doom-modeline.

;;; Time-stamp: <2019-01-30 16:11:34 stardiviner>

;;; Commentary:



;;; Code:

;;; [ doom-modeline ] -- A minimal and modern mode-line.

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-buffer-file-name-style 'buffer-name
              doom-modeline-icon nil ; don't use icon will be faster
              doom-modeline-github nil))



(provide 'init-doom-modeline)

;;; init-doom-modeline.el ends here
