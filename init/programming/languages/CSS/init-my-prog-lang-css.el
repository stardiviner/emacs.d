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
          '(lambda ()
             (my-company-add-backends-to-mode '(company-css))))


;;; [ css-eldoc ]

(use-package css-eldoc
  :ensure t
  :config
  (css-eldoc-enable))

;;; ------------------------------------------------------------------------------------
;;; Attention!

;; If your less mode's isearch became really slow, add the following code to your .emacs

;; (defun isearch-forward-noeldoc ()
;;   "close eldoc temperaily"
;;   (interactive)
;;   (eldoc-mode -1)
;;   (isearch-forward)
;;   (eldoc-mode 1))
;; (add-hook 'less-css-mode-hook (lambda ()
;;                 (local-set-key [remap isearch-forward] 'isearch-forward-noeldoc)))

;; (defun isearch-backward-noeldoc ()
;;   "close eldoc temperaily"
;;   (interactive)
;;   (eldoc-mode -1)
;;   (isearch-backward)
;;   (eldoc-mode 1))
;; (add-hook 'less-css-mode-hook (lambda ()
;;                 (local-set-key [remap isearch-backward] 'isearch-backward-noeldoc)))
;;; ------------------------------------------------------------------------------------


;;; [ show-css ] -- Show the css of the html attribute the cursor is on.

;; (use-package show-css
;;   :ensure t
;;   :config
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


;;; [ flycheck-css-colorguard ]

(use-package flycheck-css-colorguard
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'css-csslint
                               'css-colorguard 'append))
  )


;;; [ SCSS ]

(require 'init-my-prog-lang-css-scss)
(require 'init-my-prog-lang-css-less)


(provide 'init-my-prog-lang-css)

;;; init-my-prog-lang-css.el ends here
