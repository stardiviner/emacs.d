;;; init-my-prog-lang-css.el --- init CSS settings language for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ css-mode ]

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

(setq auto-mode-alist
      (append '(("\\.css$" . css-mode))
              auto-mode-alist))

(setq css-indent-offset 2)

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode 1)
            
            (add-to-list (make-local-variable 'company-backends)
                         'company-css)))


;;; [ css-eldoc ]

(css-eldoc-enable)

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


;;; [ showcss-mode ] -- Show the css of the html attribute the cursor is on.

;;; Features
;;
;; - Displays a breadcrumb of the current elements parents
;; - Displays the css for the current element grouped by file
;; - Each field that shows the css is editable, and any changes are sent back to the source file
;; - ctrl-x ctrl-s in the display buffer, saves the source buffers
;; - The default mode showcss uses is css-mode, but it can be set to any other
;;   mode such as sass-mode if your working with sass files

;;; Usage:
;;
;; - `my/toggle-showcss' / [C-c C-k] ::

;;; Personally, I find this mode to distracting to use all the time, so I use
;;; this function to quickly toggle the mode on and off.
(defun my/toggle-showcss()
  "Toggle showcss-mode"
  (interactive)
  (if (derived-mode-p
       'html-mode
       'nxml-mode
       'nxhtml-mode
       'web-mode
       'handlebars-mode)
      (showcss-mode 'toggle)
    (message "Not in an html mode")))

;; (unless (featurep 'sgml-mode)
;;   (require 'sgml-mode))

(use-package html-mode
  :config
  (define-key html-mode-map (kbd "C-c C-k") 'my/toggle-showcss))

;; FIXME:
;; (unless (featurep 'web-mode)
;;   (require 'web-mode))
;; (use-package showcss-mode
;;   :config
;;   (define-key web-mode-map (kbd "C-c C-k") 'my/toggle-showcss)
;;   )



;;; [ SCSS ]

(require 'init-my-prog-lang-css-scss)
(require 'init-my-prog-lang-css-less)


(provide 'init-my-prog-lang-css)

;;; init-my-prog-lang-css.el ends here
