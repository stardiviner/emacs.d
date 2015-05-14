;;; init-my-prog-lang-css.el --- init CSS settings language for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ css-mode ]

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

(setq auto-mode-alist
      (append '(("\\.css$" . css-mode))
              auto-mode-alist))



(add-hook 'css-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-css)))


;;; [ css-eldoc ]

(require 'css-eldoc)

(turn-on-css-eldoc)

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

(autoload 'showcss-mode "show_css"
  "Display the css of the class or id the cursor is at" t)

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
(global-set-key (kbd "C-c C-k") 'my/toggle-showcss)


;;; [ SCSS ]

(require 'init-my-prog-lang-css-scss)
(require 'init-my-prog-lang-css-less)


(provide 'init-my-prog-lang-css)

;;; init-my-prog-lang-css.el ends here
