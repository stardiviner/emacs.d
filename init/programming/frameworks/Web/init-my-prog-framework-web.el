;;; init-my-prog-framework-web.el --- init Web framework settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ web-mode ]

;;; web-mode.el is an autonomous emacs major mode for editing web templates aka
;;; HTML files embedding client parts (CSS/JavaScript) and server blocks.

;;; web-mode.el is compatible with many template engines: PHP, JSP, ASP, Django,
;;; Twig, Jinja(2), ERB, FreeMarker, Velocity, Cheetah, Smarty, CTemplate,
;;; Mustache, Blade, ErlyDTL, Go Template, Dust.js, Google Closure (soy), etc.

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; Using web-mode for editing plain HTML files can be done this way
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;;; ----------------------------------------------------------
;;; Associate an engine

;;; Using this association list is required as soon as the file extension is
;;; unknown (by web-mode) or is too general (e.g. *.html).
;;;
;;; The recognized file extensions are listed in the Engine families paragraph.
;;; In summary, you may have to set both auto-mode-alist and web-mode-engines-alist.

;; (add-to-list 'web-mode-engines '(()))
(setq web-mode-engines-alist
      '(("php" . "\\.phtml\\'")
        ("blade" . "\\.blade\\.")
        ))


;;; Engine families

;;; Never forget to update the auto-mode-alist.


;;; auto-pairs

;;; add auto-pair
;; (setq web-mode-extra-auto-pairs
;;       '(("erb" . (("open" "close")))
;;         ("php" . (("open" "close") ("open" "close")))
;;         ))


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


;;; snippets

;;; add a snippet
;; (setq web-mode-extra-snippets
;;       '(("erb" . (("name" . ("beg" . "end"))))
;;         ("php" . (("name" . ("beg" . "end"))
;;                   ("name" . ("beg" . "end"))))
;;         ))



;;; Faces

;; unicode symbols
(setq web-mode-enable-block-face t
      web-mode-enable-part-face t
      web-mode-enable-comment-keywords t
      web-mode-enable-whitespaces nil     ; show whitespace, lineend, etc with unicode symbols.
      web-mode-enable-heredoc-fontification t
      web-mode-enable-current-element-highlight t
      ;; web-mode-abbrev-table
      ;; web-mode-syntax-table
      ;; web-mode-display-table
      )

;; ;; effects
;; (set-face-attribute 'web-mode-folded-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-warning-face nil
;;                     :foreground "red" :background "black")
;; (set-face-attribute 'web-mode-whitespace-face nil
;;                     :foreground "red")
;; (set-face-attribute 'web-mode-comment-keyword-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; html
;; (set-face-attribute 'web-mode-doctype-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-html-tag-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-html-attr-name-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-html-attr-value-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; json
;; (set-face-attribute 'web-mode-json-key-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-json-context-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-json-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; css
;; (set-face-attribute 'web-mode-css-at-rule-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-property-name-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-function-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-priority-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-pseudo-class-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-selector-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-css-css-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; code
;; (set-face-attribute 'web-mode-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-comment-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-preprocessor-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-variable-name-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-function-name-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-constant-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-type-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-keyword-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-symbol-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-builtin-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; block
;; (set-face-attribute 'web-mode-block-control-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-block-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-block-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-block-comment-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; ;; part
;; (set-face-attribute 'web-mode-part-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-part-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-part-comment-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))
;; (set-face-attribute 'web-mode-javascript-string-face nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil))



;;; [ multi-web-mode ]



;;; [ web-beautify ] ---




(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
