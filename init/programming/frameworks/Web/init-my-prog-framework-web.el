;;; init-my-prog-framework-web.el --- init Web framework settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ web-mode

;;; http://web-mode.org/

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

;; (string= (web-mode-language-at-pos) "html")

;;;_. config
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

;; You can also edit plain js, jsx, css, scss, xml files.

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
;; (setq web-mode-content-types-alist
;;       '(("json" . "/some/path/*\\.api\\'")
;;         ("xml"  . "/other/path/*\\.api\\'")
;;         ("jsx"  . "/some/react/path/*\\.js[x]?\\'")))



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
       ;; padding (inner indent)
       (setq web-mode-style-padding 1   ; for <style>
             web-mode-script-padding 1  ; for <script>
             web-mode-block-padding 0   ; for multi-line blocks
             )
       ;; comment: 1, server (block) side comment, 2, client (HTML, CSS, JS) side comment
       (setq web-mode-comment-style 1)

       )
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
                    :background (color-darken-name (face-background 'default) 5)
                    :box '(:color "black" :line-width -1)
                    )
(set-face-attribute 'web-mode-html-tag-bracket-face nil ; html tag bracket
                    :inherit 'web-mode-html-tag-face
                    :foreground "dim gray"
                    )
(set-face-attribute 'web-mode-html-tag-custom-face nil ; custom tags, e.g. <span-custom>
                    :foreground "cyan")
(set-face-attribute 'web-mode-html-attr-name-face nil ; attribute name
                    :foreground "light cyan")
(set-face-attribute 'web-mode-html-attr-equal-face nil ; attribute =
                    :foreground "red")
(set-face-attribute 'web-mode-html-attr-value-face nil ; attribute value
                    :foreground "orange")
(set-face-attribute 'web-mode-html-attr-custom-face nil ; custom attributes, e.g. data-content="".
                    :foreground "green")

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
(set-face-attribute 'web-mode-block-control-face nil
                    :foreground "green")
(set-face-attribute 'web-mode-block-delimiter-face nil
                    :foreground "dark red")
(set-face-attribute 'web-mode-block-face nil ; block. e.g. { color: #0aa; }
                    :background (color-darken-name (face-background 'default) 4))
(set-face-attribute 'web-mode-block-string-face nil ; block string
                    :inherit 'web-mode-block-face
                    :foreground "red")
(set-face-attribute 'web-mode-block-comment-face nil ; block comment
                    :foreground "dim gray")

;;;_ + part
;; web-mode-part-face (see web-mode-enable-part-face), web-mode-script-face, web-mode-style-face, web-mode-part-string-face, web-mode-part-comment-face, web-mode-javascript-string-face
(setq web-mode-enable-part-face t)
(set-face-attribute 'web-mode-part-face nil ; other parts in block
                    :inherit 'web-mode-block-face
                    :foreground "dark gray")
(set-face-attribute 'web-mode-script-face nil ; <script> block
                    :background "black")
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

;; expanding
;; e.g. auto expand s/ into <span></span>
(setq web-mode-enable-auto-expanding t)
;;; Ruby
(add-to-list 'web-mode-expanders '("r/" . "<%= | %>")) ; ruby erb: <%= | %>.
(add-to-list 'web-mode-expanders '("%/" . "<%= | %>")) ; ruby erb: <%= | %>.
(add-to-list 'web-mode-expanders '("R/" . "<% | %>")) ; ruby erb: <% | %>.


;;;_. auto-complete support

;; - web-mode-ac-sources-alist
;; - web-mode-before-auto-complete-hooks

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-html-tag ac-source-html-attribute ac-source-html-attribute-value
                                      ac-source-capf
                                      ac-source-words-in-buffer))))

;;;_. Faces
(set-face-attribute 'web-mode-block-face nil
                    :background "black")


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


;;;_* disable "<" pair from smartparens.
(sp-local-pair '(web-mode)
               "<" nil
               :actions '(:rem insert))

;;;_* comment

;; (("java" . "/*") ("javascript" . "/*") ("php" . "/*"))
;; TODO: for js lines region un-comment.
;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))


;;;_ multi-web-mode


;;;_ web-beautify

;;;_ restclient

;;; This is a tool to manually explore and test HTTP REST webservices. Runs
;;; queries from a plain-text query sheet, displays results as a pretty-printed
;;; XML, JSON and even images.

;;; Usage:
;;
;; `restclient-mode' is a major mode which does a bit of highlighting and supports
;; a few additional keypresses:
;;
;; buffer example:
;;
;;   GET http://www.example.com
;;   # use comment as separator for queries.
;;   :username = chris
;;   :password := (read (file "filename.txt"))
;;   POST http://www.example.com/?action=login&:username&:password
;;
;; for localhost
;; GET http://127.0.0.1:3000
;;
;; - [C-c C-c] :: runs the query under the cursor, tries to pretty-print the response (if possible)
;; - [C-c C-r] :: same, but doesn't do anything with the response, just shows the buffer
;; - [C-c C-v] :: same as C-c C-c, but doesn't switch focus to other window
;; - [C-c C-p] :: jump to the previous query
;; - [C-c C-n] :: jump to the next query
;; - [C-c C-.] :: mark the query under the cursor

(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;;;_ know-your-http-well

(require 'know-your-http-well)

;;;_ company-restclient

;; Features
;;
;; - HTTP method name completion
;; - HTTP header name completion
;; - If header name starts with uppercase character, the completion result is capitalized (e.g. "Content-Type").
;; - Otherwise, the completion result contains lowercase characters only (e.g. "content-type").
;; - Description about HTTP method and header is shown in minibuffer
;; - Variable name completion

(add-hook 'restclient-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-restclient)))


;;;_
(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
