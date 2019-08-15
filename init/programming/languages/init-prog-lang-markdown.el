;;; init-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

(use-package markdown-mode
  :ensure t
  :defer t)

;;; [ flymd ] -- On the fly markdown preview.

(use-package flymd
  :ensure t
  :commands (flymd-flyit))

;;; [ grip-mode ] -- Instant Github-flavored Markdown/Org preview using grip (Github README Instant Preview).

;; (use-package grip-mode
;;   :ensure t
;;   :bind (:map markdown-mode-command-map ("g" . grip-mode)))


(provide 'init-prog-lang-markdown)

;;; init-prog-lang-markdown.el ends here
