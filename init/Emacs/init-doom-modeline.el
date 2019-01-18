;;; init-doom-modeline.el --- init for doom-modeline.

;;; Time-stamp: <2019-01-18 13:46:01 stardiviner>

;;; Commentary:



;;; Code:

;;; [ doom-modeline ] -- A minimal and modern mode-line.

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-init)
  :init (setq doom-modeline-buffer-file-name-style 'buffer-name
              ;; doom-modeline-icon nil ; don't use icon will be faster
              doom-modeline-github nil))



(provide 'init-doom-modeline)

;;; init-doom-modeline.el ends here
