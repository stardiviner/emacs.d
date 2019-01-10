;;; init-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

(use-package markdown-mode
  :ensure t
  :ensure-system-package (markdown . "sudo pacman -S --noconfirm discount")
  :defer t)

;;; [ flymd ] -- On the fly markdown preview.

(use-package flymd
  :ensure t
  :commands (flymd-flyit))


(provide 'init-prog-lang-markdown)

;;; init-prog-lang-markdown.el ends here
