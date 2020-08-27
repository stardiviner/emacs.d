;;; init-stack-overflow.el --- init for Stack Exchange
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SX ] -- Stack Exchange

(use-package sx
  :ensure t
  :defer t
  :init
  (unless (boundp 'stack-exchange-prefix)
    (define-prefix-command 'stack-exchange-prefix))
  (define-key tools-prefix (kbd "s") 'stack-exchange-prefix)
  :bind (:map stack-exchange-prefix
              ("q" . sx-tab-all-questions)
              ("i" . sx-inbox)
              ("o" . sx-open-link)
              ("u" . sx-tab-unanswered-my-tags)
              ("a" . sx-ask)
              ("s" . sx-search))
  :custom ((sx-question-mode-display-buffer-function #'pop-to-buffer)
           (sx-question-mode-recenter-line 2))
  :init
  ;; manage popup windows
  (add-to-list 'display-buffer-alist
               '("^\\*question-list: All-Questions \\*$" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*sx-question\\*$" . (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("^\\*sx draft answer.*\\*$" . (display-buffer-below-selected)))
  ;; (use-package texfrag ; show MathJax in HTML and sx.el
  ;;   :ensure t
  ;;   :hook (sx-question-mode . texfrag-mode))
  )


(provide 'init-stack-exchange)

;;; init-stack-exchange.el ends here
