;;; init-my-prog-lang-markdown.el --- init Emacs for Markdown
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ markdown-mode ]

;;; Usage:
;;
;; - [C-h m] :: check out major mode keybindings.
;; - [C-c C-t] :: header
;; - [C-c C-s] :: insert
;; - [C-c C-x] :: change header
;; - [C-c C-c] :: function
;; - [C-c C-a] :: insert links
;; - [C-c C-]] :: complete
;;
;; note: some keybindings work on text also can work on region.

(use-package markdown-mode
  :ensure t
  :config
  ;; Faces
  ;; header text
  (set-face-attribute 'markdown-header-face nil
                      :foreground "cyan"
                      :weight 'bold
                      :overline t
                      )
  ;; # header #
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "#444444"
                      )
  ;; header
  ;; ======
  (set-face-attribute 'markdown-header-rule-face nil
                      :foreground "#444444"
                      )
  ;; header (1-6)
  (set-face-attribute 'markdown-header-face-1 nil
                      :inherit 'markdown-header-face
                      :foreground "cyan"
                      :overline t)
  (set-face-attribute 'markdown-header-face-2 nil
                      :inherit 'markdown-header-face-1
                      :foreground "deep pink")
  (set-face-attribute 'markdown-header-face-3 nil
                      :inherit 'markdown-header-face-1
                      :foreground "green yellow")
  (set-face-attribute 'markdown-header-face-4 nil
                      :inherit 'markdown-header-face-1
                      :foreground "yellow")
  (set-face-attribute 'markdown-header-face-5 nil
                      :inherit 'markdown-header-face-1
                      :foreground "slate blue")
  (set-face-attribute 'markdown-header-face-6 nil
                      :inherit 'markdown-header-face-1
                      :foreground "sky blue")
  ;; line break
  (set-face-attribute 'markdown-line-break-face nil
                      :foreground "slate blue")
  ;; italic
  (set-face-attribute 'markdown-italic-face nil
                      :slant 'italic
                      :foreground "white")
  ;; bold
  (set-face-attribute 'markdown-bold-face nil
                      :weight 'bold
                      :foreground "white")
  ;; list
  (set-face-attribute 'markdown-list-face nil
                      :foreground "red")
  ;; comment
  (set-face-attribute 'markdown-comment-face nil
                      :foreground "dark gray")
  ;; link
  (set-face-attribute 'markdown-link-face nil
                      :foreground "#888888"
                      :underline '(:color "dark cyan"))
  (set-face-attribute 'markdown-url-face nil
                      :foreground "cyan")
  (set-face-attribute 'markdown-link-title-face nil
                      :foreground "cyan")
  (set-face-attribute 'markdown-missing-link-face nil
                      :foreground "#888888" :background "dark red"
                      :underline '(:color "dark cyan"))
  ;; meta-data
  (set-face-attribute 'markdown-metadata-key-face nil
                      :foreground "slate blue")
  (set-face-attribute 'markdown-metadata-value-face nil
                      :foreground "yellow")
  ;; footnote & reference
  (set-face-attribute 'markdown-footnote-face nil
                      :foreground "light blue" :background "black")
  (set-face-attribute 'markdown-reference-face nil
                      :foreground "green" :background "black")
  ;; block, quote, pre, code
  (set-face-attribute 'markdown-pre-face nil
                      :foreground "cyan" :background "black")
  ;; programming language identifier
  (set-face-attribute 'markdown-language-keyword-face nil
                      :foreground "cyan")
  (set-face-attribute 'markdown-blockquote-face nil
                      :foreground "cyan" :background "black")
  (set-face-attribute 'markdown-inline-code-face nil
                      :foreground "white" :background "dark cyan"
                      :box '(:color "cyan" :line-width -1))
  (set-face-attribute 'markdown-math-face nil
                      :foreground "orange")
  )


;;; [ flymd ] -- Emacs on the fly markdown preview.

(use-package flymd
  :ensure t
  :config
  ;; for Chrome browser compatible, set default browser to Firefox.
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  
  (setq flymd-browser-open-function 'my-flymd-browser-function)
  )



(provide 'init-my-prog-lang-markdown)

;;; init-my-prog-lang-markdown.el ends here
