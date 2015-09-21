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
  (add-hook 'emmet-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-web-html)
              (add-to-list (make-local-variable 'company-backends)
                           'company-web-jade)
              (add-to-list (make-local-variable 'company-backends)
                           'company-web-slim)
              ))

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
                      :box '(:color "cyan" :line-width -1)
                      :underline nil
                      )
  (set-face-attribute 'emmet-preview-output nil
                      :foreground "dark slate gray"
                      :background "#222222"
                      )
  )




(require 'init-my-prog-lang-html5)


(provide 'init-my-prog-lang-html)

;;; init-my-prog-lang-html.el ends here
