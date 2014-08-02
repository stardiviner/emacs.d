;;; init-my-prog-framework-web.el --- init Web framework settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ web-mode

;;; web-mode.el is an autonomous emacs major mode for editing web templates aka
;;; HTML files embedding client parts (CSS/JavaScript) and server blocks.

;;; web-mode.el is compatible with many template engines: PHP, JSP, ASP, Django,
;;; Twig, Jinja(2), ERB, FreeMarker, Velocity, Cheetah, Smarty, CTemplate,
;;; Mustache, Blade, ErlyDTL, Go Template, Dust.js, Google Closure (soy), etc.


;;;_. Usage:

;;; http://web-mode.org

;;;_ , Shortcuts

;;;_  . General
;;
;; C-c C-; comment / uncomment line(s)
;; C-c C-f toggle folding on a tag/block
;; C-c C-i indent entire buffer
;; C-c C-m mark and expand
;; C-c C-s insert snippet
;; C-c C-w toggle display of invalid whitespaces

;;;_  . DOM
;;
;; C-c C-d d show tag mismatch
;; C-c C-d e replace HTML entities
;; C-c C-d n normalize
;; C-c C-d q replace dumb quotes
;; C-c C-d t traverse dom tree
;; C-c C-d x xpath

;;;_  . Block
;;
;; C-c C-b c block close
;; C-c C-b b block beginning
;; C-c C-b e block end
;; C-c C-b p previous block
;; C-c C-b n next block
;; C-c C-b k block kill
;; C-c C-b s block select

;;;_  . HTML element
;;
;; C-c / element close
;; C-c C-e b element beginning
;; C-c C-e c element clone
;; C-c C-e d child element (down)
;; C-c C-e e element end
;; C-c C-e i select element content (inner)
;; C-c C-e k element kill
;; C-c C-e n next element
;; C-c C-e p previous element
;; C-c C-e r rename element
;; C-c C-e s select element
;; C-c C-e u parent element (up)
;; C-c C-e u element vanish

;;;_  . HTML tag
;;
;; C-c C-t a sort attributes
;; C-c C-t b tag beginning
;; C-c C-t e tag end
;; C-c C-t m fetch matching tag (also available for active blocks)
;; C-c C-t s select tag
;; C-c C-t p previous tag
;; C-c C-t n next tag

;;;_  . HTML attribute
;;
;; C-c C-a b attribute beginning
;; C-c C-a e attribute end
;; C-c C-a s attribute select
;; C-c C-a t attribute transpose
;; C-c C-a n attribute next

;;;_ , Helper functions

;; web-mode-apostrophes-replace : replace ' by ’ (only in HTML content)
;; web-mode-entities-replace : replace html entities (only in HTML content)
;; web-mode-quotes-replace : replace dumb quotes (only in HTML content)

;;;_ , Engine families

;; Never forget to update the `auto-mode-alist'.



;;;_. config
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; Using web-mode for editing plain HTML files can be done this way
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;;;_. Associate an engine

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

;;;_. Engine families

;;; Never forget to update the auto-mode-alist.

;;;_. auto-pairs

;;; add auto-pair
;; (setq web-mode-extra-auto-pairs
;;       '(("erb" . (("open" "close")))
;;         ("php" . (("open" "close") ("open" "close")))
;;         ))

;;;_. web-mode defaults
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

;;;_. snippets

;;; add a snippet
;; (setq web-mode-extra-snippets
;;       '(("erb" . (("name" . ("beg" . "end"))))
;;         ("php" . (("name" . ("beg" . "end"))
;;                   ("name" . ("beg" . "end"))))
;;         ))

;;;_. Faces

;;;_ , unicode symbols
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

;;;_. auto-complete support

;; web-mode-ac-sources-alist
;; web-mode-before-auto-complete-hooks

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;;;_. Faces

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


;;;_ multi-web-mode


;;;_ web-beautify



;;;_
(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
