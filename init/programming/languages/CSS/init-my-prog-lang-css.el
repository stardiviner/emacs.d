;;; init-my-prog-lang-css.el --- init CSS settings language for Emacs.
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

;; only need to add `company-css' to `css-mode-hook' once,
;; because `scss-mode' etc are derived from `css-mode'.
(add-hook 'css-mode-hook
          (lambda ()
            (my-company-add-backend-locally 'company-css)
            ))


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

(use-package stylefmt
  :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook 'stylefmt-enable-on-save)
  )


(provide 'init-my-prog-lang-css)

;;; init-my-prog-lang-css.el ends here
