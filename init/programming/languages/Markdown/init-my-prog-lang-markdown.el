;;; init-my-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

(use-package markdown-mode
  :ensure t
  :defer t)


;;; [ flymd ] -- Emacs on the fly markdown preview.

(use-package flymd
  :ensure t
  :defer t
  :init
  ;; (add-hook 'markdown-mode-hook #'flymd-flyit)
  :config
  ;; for Chrome browser compatible, set default browser to Firefox.
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  
  (setq flymd-browser-open-function 'my-flymd-browser-function)
  )



(provide 'init-my-prog-lang-markdown)

;;; init-my-prog-lang-markdown.el ends here
