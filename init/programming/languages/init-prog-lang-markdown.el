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

;;; [ maple-preview ] -- Markdown, Org Mode or HTML realtime preview on Emacs.

;; (use-package maple-preview
;;   :quelpa (maple-preview :fetcher github :repo "honmaple/emacs-maple-preview")
;;   :commands (maple-preview-mode)
;;   :init (setq maple-preview:port 8083))


(provide 'init-prog-lang-markdown)

;;; init-prog-lang-markdown.el ends here
