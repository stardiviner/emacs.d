;;; init-my-prog-framework-web.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ web-mode ]

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))



(eval-after-load 'web-mode
  '(progn
     (defun my-web-mode-defaults ()
       ;; Customizations
       (setq web-mode-markup-indent-offset 4
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 4
             web-mode-disable-autocompletion t)
       (local-set-key (kbd "RET") 'newline-and-indent))
     (setq my-web-mode-hook 'my-web-mode-defaults)

     (add-hook 'web-mode-hook
               (lambda ()
                 (run-hooks 'my-web-mode-hook)))))





(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
