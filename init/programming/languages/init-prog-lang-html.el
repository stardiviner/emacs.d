;;; init-prog-lang-html.el --- init HTML for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ mhtml-mode ] -- Major mode based on ‘html-mode’, but works with embedded JS and CSS.

;;; NOTE: use `web-mode' instead now.
;; (use-package mhtml-mode
;;   :mode (("\\.html\\'" . mhtml-mode)))

;;; [ emmet-mode ]

(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'rhtml-mode-hook  'emmet-mode)
  :config
  (setq emmet-preview-default t ; set preview as the default action.
        emmet-indentation 4
        emmet-indent-after-insert t
        emmet-use-style-tag-and-attr-detection t)

  ;; By default, inserted markup will be indented with indent-region, according to
  ;; the buffer's mode. To disable this, do:
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))

  ;; If you disable indent-region, you can set the default indent level thusly:
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

  ;; If you want the cursor to be positioned between first empty quotes after expanding:
  ;; (setq emmet-move-cursor-between-quotes t) ;; default nil

  ;; Or if you don't want to move cursor after expanding:
  ;; (setq emmet-move-cursor-after-expanding nil) ;; default t

  ;; e.g. div -> <div>|</div>
  (define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-yas)

  ;; preview minor mode
  (emmet-preview-mode 1))

;;; [ tagedit ] -- A collection of paredit-like functions for editing in html-mode.

;; (use-package tagedit
;;   :ensure t
;;   :defer t
;;   :hook (html-mode-hook . tagedit-mode)
;;   :init
;;   (tagedit-add-paredit-like-keybindings)
;;   ;; auto insert <></> when you type <, and auto expand to <div></div> as you type.
;;   (tagedit-add-experimental-features))

;;; [ impatient-mode ] -- see your HTML rendered as you type.

(use-package impatient-mode
  :ensure t
  :defer t
  :commands (impatient-mode))

;;; [ ob-browser-chrome ] -- Export Org HTML SRC blocks as PNG files using Chrome in "headless" mode.

(use-package ob-html-chrome
  :quelpa (ob-html-chrome :fetcher github :repo "stardiviner/ob-html-chrome")
  :commands (org-babel-execute:html-chrome)
  :custom (org-babel-html-chrome-chrome-executable (executable-find "google-chrome-unstable"))
  :config
  (add-to-list 'org-babel-load-languages '(html-chrome . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("html-chrome" . "html")))

;;; [ cakecrumbs ] -- Show parent-chain on header for HTML / Jade / Pug / LESS / SCSS / Sass / Stylus.

;; (use-package cakecrumbs
;;   :ensure t
;;   :init (cakecrumbs-auto-setup))

;;; [ company-web ] -- Company version of ac-html, complete for web,html,emmet,jade,slim modes.

(use-package company-web
  :ensure t
  :config
  (defun my:html-mode-company-add ()
    "Add company-mode backend like `company-web-html' for completion."
    (make-variable-buffer-local 'company-backends)
    (add-to-list 'company-backends 'company-web-html))
  (add-hook 'html-mode-hook #'my:html-mode-company-add))


(provide 'init-prog-lang-html)

;;; init-prog-lang-html.el ends here
