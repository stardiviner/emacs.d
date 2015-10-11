;;; init-my-prog-framework-web.el --- init Web framework settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ web-mode

(require 'web-mode)

;; Using web-mode for editing plain HTML files can be done this way
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; You can also edit plain js, jsx, css, scss, xml files.
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; TODO: test
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;;;_. Associate an engine

;;; Using this association list is required as soon as the file extension is
;;; unknown (by web-mode) or is too general (e.g. *.html).
;;;
;;; The recognized file extensions are listed in the Engine families paragraph.
;;; In summary, you may have to set both `auto-mode-alist' and `web-mode-engines-alist'.

;; `web-mode-engines'

(setq web-mode-enable-element-tag-fontification t
      web-mode-enable-element-content-fontification t)

(setq web-mode-enable-engine-detection t)

;; (setq web-mode-engines-alist '())
;; (add-to-list 'web-mode-engines-alist '("php" . "\\.phtml\\'"))
;; (add-to-list 'web-mode-engines-alist '("blade" . "\\.blade\\."))

;;;_. Associate a content type

;; web-mode.el can deal with many content types: html, xml, javascript, jsx,
;; json, css. This was needed to edit *.js.erb files for example: js files that
;; embed ruby blocks.
;;
;; Sometimes, web-mode.el can not guess the content type with the file
;; extension.  e.g. you want to associate *.api files with web-mode.
;;
;; (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("/some/react/path/*\\.js[x]?\\'" . web-mode))
;;
;; (setq web-mode-content-types-alist
;;       '(("json" . "/some/path/.*\\.api\\'")
;;         ("xml"  . "/other/path/.*\\.api\\'")
;;         ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))



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
       ;; indent
       (setq web-mode-markup-indent-offset 2 ; HTML indent
             web-mode-css-indent-offset 2    ; CSS indent
             web-mode-code-indent-offset 2   ; Script (JavaScript, Ruby, PHP, etc) indent
             web-mode-attr-indent-offset 2   ; HTML attribute offset
             web-mode-disable-autocompletion t)
       (local-set-key (kbd "RET") 'newline-and-indent)
       ;; electric
       (setq web-mode-enable-auto-pairing t)
       ;; padding (inner indent)
       (setq web-mode-style-padding 1   ; for <style>
             web-mode-script-padding 1  ; for <script>
             web-mode-block-padding 0   ; for multi-line blocks
             )
       ;; comment: 1, server (block) side comment, 2, client (HTML, CSS, JS) side comment
       (setq web-mode-comment-style 1)
       ;; CSS
       (setq web-mode-enable-css-colorization t)
       ;; (rainbow-mode 1)
       )
     (setq my-web-mode-hook 'my-web-mode-defaults)

     (add-hook 'web-mode-hook
               (lambda ()
                 (run-hooks 'my-web-mode-hook)
                 (rainbow-delimiters-mode-enable)
                 ))))

;;;_. snippets

;;; add a snippet
;; (setq web-mode-extra-snippets
;;       '(("erb" . (("name" . ("beg" . "end"))))
;;         ("php" . (("name" . ("beg" . "end"))
;;                   ("name" . ("beg" . "end"))))
;;         ))

;;;_. Syntax Highlight: Faces

;;;_ + effects
;; web-mode-folded-face, web-mode-warning-face, web-mode-whitespace-face, web-mode-comment-keyword-face
(set-face-attribute 'web-mode-folded-face nil
                    :weight 'bold :slant 'italic)
(set-face-attribute 'web-mode-warning-face nil
                    :foreground "red")
(set-face-attribute 'web-mode-whitespace-face nil
                    :background "dark magenta")
(set-face-attribute 'web-mode-comment-keyword-face nil
                    :foreground "dark gray"
                    :box '(:line-width 1))

;;;_ + html
;; web-mode-doctype-face, web-mode-html-tag-face, web-mode-html-tag-bracket-face, web-mode-html-attr-name-face, web-mode-html-attr-value-face, web-mode-html-attr-equal-face, web-mode-html-tag-custom-face, web-mode-attr-tag-custom-face
(set-face-attribute 'web-mode-doctype-face nil
                    :foreground "red" :background "dark red")
(set-face-attribute 'web-mode-html-tag-face nil ; html tag
                    :foreground "dark cyan"
                    :background (color-darken-name (face-background 'default) 2)
                    )
(set-face-attribute 'web-mode-html-tag-bracket-face nil ; html tag bracket
                    :inherit 'web-mode-html-tag-face
                    :foreground "#444444"
                    )
(set-face-attribute 'web-mode-html-tag-custom-face nil ; custom tags, e.g. <span-custom>
                    :foreground "cyan")
(set-face-attribute 'web-mode-html-attr-name-face nil ; attribute name
                    :foreground "cyan")
(set-face-attribute 'web-mode-html-attr-equal-face nil ; attribute =
                    :foreground "#444444")
(set-face-attribute 'web-mode-html-attr-value-face nil ; attribute value
                    :foreground "chartreuse")
(set-face-attribute 'web-mode-html-attr-custom-face nil ; custom attributes, e.g. data-content="".
                    :foreground "white")
(set-face-attribute 'web-mode-html-attr-engine-face nil ; engine face
                    :background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width -1)
                    )

;;;_ + json
;; web-mode-json-key-face, web-mode-json-context-face, web-mode-json-string-face
;; TODO:
(set-face-attribute 'web-mode-json-key-face nil
                    :foreground "dark red")
(set-face-attribute 'web-mode-json-context-face nil
                    :foreground "gray")
(set-face-attribute 'web-mode-json-string-face nil
                    :foreground "yellow3")

;;;_ + css
;; web-mode-css-at-rule-face, web-mode-css-property-name-face, web-mode-css-function-face, web-mode-css-priority-face, web-mode-css-pseudo-class-face, web-mode-css-selector-face, web-mode-css-string-face
;; TODO:
(set-face-attribute 'web-mode-css-selector-face nil ; e.g. .dialog
                    :foreground "sky blue")
(set-face-attribute 'web-mode-css-at-rule-face nil ; @media
                    :foreground "turquoise")
(set-face-attribute 'web-mode-css-property-name-face nil ; property: value
                    :foreground "white")
(set-face-attribute 'web-mode-css-function-face nil
                    :foreground "yellow")
(set-face-attribute 'web-mode-css-priority-face nil
                    :foreground "red")
(set-face-attribute 'web-mode-css-pseudo-class-face nil ; e.g. :hover
                    :foreground "chocolate")
(set-face-attribute 'web-mode-css-string-face nil
                    :foreground "midnight blue")

;;;_ + code
;; web-mode-builtin-face, web-mode-comment-face, web-mode-constant-face, web-mode-filter-face, web-mode-function-call-face, web-mode-function-name-face, web-mode-keyword-face, web-mode-preprocessor-face, web-mode-string-face, web-mode-symbol-face, web-mode-type-face, web-mode-variable-name-face
;; TODO:
(set-face-attribute 'web-mode-keyword-face nil ; keywords: e.g. var, function
                    :foreground "red3")
(set-face-attribute 'web-mode-builtin-face nil
                    :foreground "green")
(set-face-attribute 'web-mode-comment-face nil ; /* comment */
                    :foreground "dim gray")
(set-face-attribute 'web-mode-constant-face nil ; constant
                    :foreground "orange")
(set-face-attribute 'web-mode-filter-face nil
                    :foreground "blue")
(set-face-attribute 'web-mode-function-call-face nil ; function call
                    :foreground "cyan")
(set-face-attribute 'web-mode-function-name-face nil ; function name
                    :foreground "cyan3")
(set-face-attribute 'web-mode-preprocessor-face nil
                    :foreground "yellow")
(set-face-attribute 'web-mode-string-face nil ; string. e.g. "hello"
                    :foreground "yellow3")
(set-face-attribute 'web-mode-symbol-face nil ; symbol.
                    :foreground "deep pink")
(set-face-attribute 'web-mode-type-face nil ; type.
                    :foreground "sandy brown")
(set-face-attribute 'web-mode-variable-name-face nil ; e.g. var name = "chris"
                    :foreground "green yellow")

;;;_ + block
(setq web-mode-enable-block-face t)
;; web-mode-block-control-face, web-mode-block-delimiter-face, web-mode-block-face (see web-mode-enable-block-face), web-mode-block-string-face, web-mode-block-comment-face
;; TODO:
(set-face-attribute 'web-mode-block-delimiter-face nil ; <%= ... =>
                    :foreground "dark red"
                    :background "black")
(set-face-attribute 'web-mode-block-face nil ; variant embed template blocks
                    :background "black")
(set-face-attribute 'web-mode-block-control-face nil
                    :foreground "green")
(set-face-attribute 'web-mode-block-string-face nil ; block string
                    :inherit 'web-mode-block-face)
(set-face-attribute 'web-mode-block-comment-face nil ; block comment
                    :foreground "dim gray")

;;;_ + part
;; web-mode-part-face (see web-mode-enable-part-face), web-mode-script-face, web-mode-style-face, web-mode-part-string-face, web-mode-part-comment-face, web-mode-javascript-string-face
(setq web-mode-enable-part-face t)
(set-face-attribute 'web-mode-part-face nil ; other parts in block
                    :inherit 'web-mode-block-face
                    :foreground "dark gray")
(set-face-attribute 'web-mode-script-face nil ; <script> block
                    :background (color-darken-name (face-background 'default) 2)
                    )
(set-face-attribute 'web-mode-style-face nil ; <style> block
                    :background "#222222")
(set-face-attribute 'web-mode-part-string-face nil ; part string
                    :foreground "midnight blue")
(set-face-attribute 'web-mode-part-comment-face nil ; part comment
                    :foreground "dim gray")
(set-face-attribute 'web-mode-javascript-string-face nil ; JavaScript string: "hello"
                    :foreground "yellow3")

;;;_ + content fontification
;; web-mode-bold-face, web-mode-italic-face, web-mode-underline-face
(set-face-attribute 'web-mode-bold-face nil
                    :weight 'bold)
(set-face-attribute 'web-mode-italic-face nil
                    :slant 'italic)
(set-face-attribute 'web-mode-underline-face nil
                    :underline "#222222")

;;;_ , unicode symbols
(setq web-mode-enable-comment-keywords t
      web-mode-enable-whitespaces nil     ; show whitespace, lineend, etc with unicode symbols.
      web-mode-enable-heredoc-fontification t
      web-mode-enable-current-element-highlight t
      ;; web-mode-abbrev-table
      ;; web-mode-syntax-table
      ;; web-mode-display-table
      )

;; expanding
;; e.g. auto expand s/ into <span></span>
(setq web-mode-enable-auto-expanding t)
;;; HTML
(add-to-list 'web-mode-expanders '("P/" . "<p>\n | \n</p>"))
;;; Ruby
(add-to-list 'web-mode-expanders '("r/" . "<%= | %>")) ; ruby erb: <%= | %>.
(add-to-list 'web-mode-expanders '("%/" . "<%= | %>")) ; ruby erb: <%= | %>.
(add-to-list 'web-mode-expanders '("R/" . "<% | %>")) ; ruby erb: <% | %>.
(add-to-list 'web-mode-expanders '("#/" . "<%# | %>")) ; ruby erb comment: <%# | %>.

;;;_. company-mode support

;; Enable CSS completion between <style>...</style>
(defadvice company-css (before web-mode-set-up-ac-sources activate)
  "Set CSS completion based on current language before running `company-css'."
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language (web-mode-language-at-pos)))
        (if (string= web-mode-cur-language "css")
            (unless css-mode (css-mode))))))

;; Enable JavaScript completion between <script>...</script> etc.
(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  "Set `tern-mode' based on current language before running `company-tern'."
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language (web-mode-language-at-pos)))
        (if (or (string= web-mode-cur-language "javascript")
               (string= web-mode-cur-language "jsx"))
            (unless tern-mode (tern-mode))
          ;; (if tern-mode (tern-mode))
          ))))

;; Enable Ruby on Rails completion between rhtml tag <% ... %> or <%= ... %>.
(defadvice company-robe (before web-mode-set-up-ac-sources activate)
  "Set `robe-mode' based on current language before running `company-robe'."
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language (web-mode-language-at-pos)))
        (if (string= web-mode-cur-language "erb")
            (unless robe-mode (robe-mode))))))

;;;_. auto-complete support
;;
;; - `web-mode-ac-sources-alist'
;; - `web-mode-before-auto-complete-hooks'
;;
;; (setq web-mode-ac-sources-alist
;;       '(("css" . (ac-source-css-property))
;;         ("html" . (ac-source-html-tag
;;                    ac-source-html-attribute ac-source-html-attribute-value
;;                    ac-source-capf
;;                    ac-source-words-in-buffer))))

;;;_* disable "<" pair from smartparens.
(sp-local-pair '(web-mode)
               "<" nil
               :actions '(:rem insert))

;;;_* comment

;; (("java" . "/*") ("javascript" . "/*") ("php" . "/*"))
;; TODO: for js lines region un-comment.
;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))

;;;_* completion <- company-mode

;; https://github.com/company-mode/company-mode/commit/75e21f6ddc2a826c6ec74312d2e725a1a827385e
;; TODO: (add-hook web-mode-before-auto-complete-hooks)
;; `web-mode-language-at-pos'
;; `company--begin-new'
;;
;; reference this:
;;
;; (defadvice ac-start (before web-mode-set-up-ac-sources activate)
;;   "Set `ac-sources' based on current language before running auto-complete."
;;   (if (equal major-mode 'web-mode)
;;       (progn
;;         (run-hooks 'web-mode-before-auto-complete-hooks)
;;         (when web-mode-ac-sources-alist
;;           (let ((new-web-mode-ac-sources
;;                  (assoc (web-mode-language-at-pos)
;;                         web-mode-ac-sources-alist)))
;;             (setq ac-sources (cdr new-web-mode-ac-sources)))))))

;;;_ multi-web-mode



;;; [ web-completion-data ] -- dependency for `ac-html', `company-web'

;; `web-completion-data-sources' is pair list of framework-name and directory of completion data
;;
;; This package provide default "html" completion data.
;;
;; Completion data directory structure:
;;
;; html-attributes-complete - attribute completion
;; html-attributes-list - attributes of tags-add-tables
;; html-attributes-short-docs - attributes documantation
;; html-tag-short-docs  - tags documantation

;; If you decide extend with own completion data, let say "Bootstrap" data:
;;
;; (unless (assoc "Bootstrap" web-completion-data-sources)
;;   (setq web-completion-data-sources 
;;         (cons (cons "Bootstrap" "/path/to/complete/data")
;;               web-completion-data-sources)))


;;; [ company-web ] --

;; company-web is an alternative emacs plugin for autocompletion in html-mode,
;; web-mode, jade-mode, slim-mode and use data of ac-html. It uses company-mode.
;;
;; use company-mode with `company-web-html' in web-mode.
;; now support company completion for emmet.
;;
;; - tags after >+^ symbols
;; - attribute completion after [
;; - Dot . - complete div's class
;; - # - id (div's id if no tag)
;;
;; annotation string: `html'.

(use-package company-web
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              
              ;; (add-to-list 'company-backends 'company-nxml)
              ;; (add-to-list 'company-backends 'company-css)
              ;; (add-to-list 'company-backends 'company-tern)

              ;; (add-to-list 'company-backends 'company-web-slim)
              ;; (add-to-list 'company-backends 'company-web-jade)
              ;; (add-to-list 'company-backends 'company-web-html)
              
              (setq company-backends (copy-tree company-backends))
              (setf (car company-backends)
                    (append '(company-web-html
                              ;; company-web-jade
                              ;; company-web-slim
                              ;; company-nxml
                              company-css
                              )
                            (car company-backends)))
              ))
  )

;; you may key bind, for example for web-mode:

(use-package web-mode
  :config
  (define-key web-mode-map (kbd "C-'") 'company-web-html)
  )


;;; [ ac-html ] -- Provide accurate and intelligent auto completion to HTML and CSS.

;; ;;; If you are using html-mode:
;; (add-hook 'html-mode-hook 'ac-html-enable)
;; ;;; If you are using web-mode:
;; ;;; Additionally you need to add these lines:
;; (unless (featurep 'web-mode)
;;   (require 'web-mode))
;; (add-to-list 'web-mode-ac-sources-alist
;;              '("html" . (ac-source-html-attribute-value
;;                          ac-source-html-tag
;;                          ac-source-html-attribute)))
;;
;; ;;; Support for template languages:
;; (add-hook 'haml-mode-hook 'ac-haml-enable)
;; (add-hook 'jade-mode-hook 'ac-jade-enable)
;; (add-hook 'slim-mode-hook 'ac-slim-enable)


;;; [ ac-html-csswatcher ]


;;; [ ac-html-bootstrap ]


;;; [ web-beautify ]

;; ~/.jsbeautifyrc

;; (eval-after-load 'js2-mode
;;   '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
;; (eval-after-load 'js
;;   '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
;; (eval-after-load 'json-mode
;;   '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;; (eval-after-load 'sgml-mode
;;   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
;; (eval-after-load 'css-mode
;;   '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(use-package web-beautify
  :config
  ;; automatically format before saving a file
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
  (eval-after-load 'js
    '(add-hook 'js-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
  )


(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
