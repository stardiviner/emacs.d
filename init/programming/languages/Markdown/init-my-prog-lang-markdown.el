;;; init-my-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

(use-package markdown-mode
  :ensure t
  :ensure-system-package (markdown . "sudo pacman -S --noconfirm discount")
  :defer t)


(provide 'init-my-prog-lang-markdown)

;;; init-my-prog-lang-markdown.el ends here
