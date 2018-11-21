;;; init-prog-lang-css.el --- init CSS settings language for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(defvar css-dialects-mode
  '(css-mode
    sass-mode
    scss-mode
    less-css-mode))

(hook-modes css-dialects-mode
  (css-eldoc-enable)
  (rainbow-mode 1)
  )

;;; [ css-mode ]

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(setq css-indent-offset 2)


;;; [ ob-css ]

(require 'ob-css)

(add-to-list 'org-babel-load-languages '(css . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("css" . "css"))


;;; [ css-eldoc ]

(use-package css-eldoc
  :ensure t
  :defer t
  :config
  (css-eldoc-enable))

;;; [ counsel-css ] -- An ivy-mode backend for css selectors (scss/less too).

(use-package counsel-css
  :ensure t
  :defer t
  :commands (counsel-css)
  :config
  (add-hook 'css-mode-hook 'counsel-css-imenu-setup))

;;; [ show-css ] -- Show the css of the html attribute the cursor is on.

;; (use-package show-css
;;   :ensure t
;;   :defer t
;;   :init
;;
;;   ;; Personally, I find this mode to distracting to use all the time, so I use
;;   ;; this function to quickly toggle the mode on and off.
;;   (defun my/toggle-showcss()
;;     "Toggle showcss-mode"
;;     (interactive)
;;     (if (derived-mode-p
;;          'html-mode
;;          'nxml-mode
;;          'nxhtml-mode
;;          'web-mode
;;          'handlebars-mode)
;;         (showcss-mode 'toggle)
;;       (message "Not in an html mode")))
;;
;;   (with-eval-after-load 'html-mode
;;     (define-key html-mode-map (kbd "C-c C-k") 'my/toggle-showcss))
;;   (with-eval-after-load 'web-mode
;;     (define-key web-mode-map (kbd "C-c C-k") 'my/toggle-showcss))
;;   )

;;; [ stylefmt ] -- An emacs interface for stylefmt style code formatter.

;; (use-package stylefmt
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'css-mode-hook 'stylefmt-enable-on-save))

;;; [ lsp-css ] -- CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin.

(use-package lsp-css
  :ensure t
  :ensure-system-package ((css-languageserver . "npm i -g vscode-css-languageserver-bin"))
  :after lsp-mode
  :commands (lsp-css-enable
             lsp-less-enable
             lsp-sass-enable
             lsp-scss-enable)
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable))
  :config
  (lsp-org-babel-enbale "css")
  (lsp-org-babel-enbale "sass"))


(provide 'init-prog-lang-css)

;;; init-prog-lang-css.el ends here
