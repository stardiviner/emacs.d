;;; init-my-prog-lang-html.el --- init HTML for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sgml-mode ] -- major mode for editing SGML documents.

;; Makes > match <.
;; Keys <, &, SPC within <>, ", / and ' can be electric depending on
;; `sgml-quick-keys'.


;;; [ zencoding-mode ] -- renamed to emmet-mode


;;; [ emmet-mode ]

;;; Usage
;;
;; - [C-j] :: preview.
;; - [C-u C-j] :: expand emmet line `emmet-expand-line'.
;;
;; Place point in a emmet snippet and press [C-j] to expand it (or
;; alternatively, alias your preferred keystroke to [M-x emmet-expand-line]) and
;; you'll transform your snippet into the appropriate tag structure.

(use-package emmet-mode
  :config
  (setq emmet-preview-default t ; set preview as the default action.
        emmet-indentation 4
        emmet-indent-after-insert t
        emmet-use-style-tag-and-attr-detection t
        )

  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'rhtml-mode-hook  'emmet-mode)

  ;; By default, inserted markup will be indented with indent-region, according to
  ;; the buffer's mode. To disable this, do:
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))

  ;; If you disable indent-region, you can set the default indent level thusly:
  ;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

  ;; If you want the cursor to be positioned between first empty quotes after expanding:
  ;; (setq emmet-move-cursor-between-quotes t) ;; default nil

  ;; Or if you don't want to move cursor after expanding:
  ;; (setq emmet-move-cursor-after-expanding nil) ;; default t

  (set-face-attribute 'emmet-preview-input nil
                      :background "#004A5D"
                      :foreground "white"
                      :box '(:color "cyan" :line-width 1)
                      )
  (set-face-attribute 'emmet-preview-output nil
                      :foreground "dark slate gray"
                      :background "#222222"
                      )
  )


;;; [ tagedit ] -- A collection of paredit-like functions for editing in html-mode.

(use-package tagedit
  :ensure t
  :defer t
  :config
  (tagedit-add-paredit-like-keybindings)
  ;; auto insert <></> when you type <, and auto expand to <div></div> as you type.
  (tagedit-add-experimental-features)
  (add-hook 'html-mode-hook 'tagedit-mode)
  )


;;; [ impatient-mode ] -- see your HTML rendered as you type.

(use-package impatient-mode
  :config
  )



(require 'init-my-prog-lang-html5)


(provide 'init-my-prog-lang-html)

;;; init-my-prog-lang-html.el ends here
