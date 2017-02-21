;;; init-my-prog-framework-web.el --- init Web framework settings for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ web-mode

(use-package web-mode
  :ensure t
  :init
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

  :config

  ;; Associate an engine

  ;; Using this association list is required as soon as the file extension is
  ;; unknown (by web-mode) or is too general (e.g. *.html).
  ;;
  ;; The recognized file extensions are listed in the Engine families paragraph.
  ;; In summary, you may have to set both `auto-mode-alist' and `web-mode-engines-alist'.

  ;; `web-mode-engines'

  (setq web-mode-enable-element-tag-fontification t
        web-mode-enable-element-content-fontification t
        web-mode-enable-current-column-highlight t)

  (setq web-mode-enable-engine-detection t)

  ;; (setq web-mode-engines-alist '())
  ;; (add-to-list 'web-mode-engines-alist '("php" . "\\.phtml\\'"))
  ;; (add-to-list 'web-mode-engines-alist '("blade" . "\\.blade\\."))

  ;;_. Associate a content type

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



  ;;_. Engine families

  ;; Never forget to update the auto-mode-alist.

  ;; SQL
  (setq web-mode-enable-sql-detection t)

  ;;_. auto-pairs

  ;; add auto-pair
  ;; (setq web-mode-extra-auto-pairs
  ;;       '(("erb" . (("open" "close")))
  ;;         ("php" . (("open" "close") ("open" "close")))
  ;;         ))

  ;;_. web-mode defaults
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
              ))

  ;;_. snippets

  ;; add a snippet
  ;; (setq web-mode-extra-snippets
  ;;       '(("erb" . (("name" . ("beg" . "end"))))
  ;;         ("php" . (("name" . ("beg" . "end"))
  ;;                   ("name" . ("beg" . "end"))))
  ;;         ))

  ;;_ , unicode symbols
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
  ;; HTML
  (add-to-list 'web-mode-expanders '("P/" . "<p>\n | \n</p>"))
  (add-to-list 'web-mode-expanders '("B/" . "<br />\n|"))
  ;; Ruby
  (add-to-list 'web-mode-expanders '("r/" . "<%= | %>")) ; ruby erb: <%= | %>.
  (add-to-list 'web-mode-expanders '("%/" . "<%= | %>")) ; ruby erb: <%= | %>.
  (add-to-list 'web-mode-expanders '("R/" . "<% | %>")) ; ruby erb: <% | %>.
  (add-to-list 'web-mode-expanders '("#/" . "<%# | %>")) ; ruby erb comment: <%# | %>.
  (add-to-list 'web-mode-expanders '("e/" . "<% end %>")) ; ruby erb end: <% end %>

  ;;_* disable "<" pair from smartparens.
  (autoload 'sp-local-pair "smartparens" t)
  (sp-local-pair '(web-mode)
                 "<" nil
                 :actions '(:rem insert))

  ;;_* comment
  ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))

  ;;_. auto-complete support
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

  ;; improving the JSX syntax-hightlighting in web-mode
  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; close element
  (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)

  (defun web-mode-element-close-single-tag ()
    "Close single tag like <input />."
    (interactive)
    (if (eq (char-before) ?\ )
        (insert "/>")
      (insert " />")
      ))
  (define-key web-mode-map (kbd "C-c C-/") 'web-mode-element-close-single-tag)
  
  ;; open a new line upper between tag.
  (define-key web-mode-map (kbd "C-o")
    '(lambda ()
       (interactive)
       (previous-line)
       (end-of-line)
       (newline-and-indent)))

  ;; fix company-mode candidates auto lowercase.
  (add-to-list 'company-dabbrev-code-modes 'web-mode)

  ;; flyspell effective spell checking in web-mode.
  (defun web-mode-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face))
           rlt)
      (cond
       ;; Check the words with these font faces, possibly.
       ;; this *blacklist* will be tweaked in next condition
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face)))
        (setq rlt t))
       ;; check attribute value under certain conditions
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                  (thing-at-point 'symbol)))))
       ;; finalize the blacklist
       (t
        (setq rlt nil)))
      rlt))

  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
  )


;;; [ web-completion-data ] -- dependency for `ac-html', `company-web'

(use-package web-completion-data
  :ensure t)

;;; [ web-mode-edit-element ] -- helper-functions for attribute- and element-handling.

(use-package web-mode-edit-element
  :ensure t
  :init
  (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))


;;; [ company-web ] --

(use-package company-web
  :ensure t
  :init
  (defun my-company-web-backends-setup ()
    (interactive)
    (setq-local company-minimum-prefix-length 1)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-css)
    (add-to-list 'company-backends 'company-jquery)
    (add-to-list 'company-backends 'company-tern)
    ;; (add-to-list 'company-backends 'company-web-jade)
    ;; (add-to-list 'company-backends 'company-web-slim)
    (add-to-list 'company-backends 'company-web-html)
    )
  
  (dolist (hook '(web-mode-hook
                  html-mode-hook
                  ))
    (add-hook hook #'my-company-web-backends-setup))

  :config
  ;; company-mode + company-web support

  ;; Enable CSS completion between <style>...</style>
  ;; (defadvice company-css (before web-mode-set-up-ac-sources activate)
  ;;   "Set CSS completion based on current language before running `company-css'."
  ;;   (if (equal major-mode 'web-mode)
  ;;       (let ((web-mode-cur-language (web-mode-language-at-pos)))
  ;;         (if (string= web-mode-cur-language "css")
  ;;             ;; ??
  ;;             ))))

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running `company-tern'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tern-mode (tern-mode 1))
            (if tern-mode (tern-mode -1))
            ))))

  ;; [C-c ']
  ;; let Org-mode Babel src code block auto set `web-mode-engine' for rhtml.
  (defadvice org-edit-special (before org-edit-src-code activate)
    (let ((lang (nth 0 (org-babel-get-src-block-info))))
      (if (string= lang "rhtml")
          (web-mode-set-engine "erb"))))

  (defadvice org-edit-special (after org-edit-src-code activate)
    (if (string= web-mode-engine "erb")
        (add-to-list (make-local-variable 'company-backends) 'company-robe)))
  
  ;; Enable Rails completion between <%= ... %>, or <% ... %>.
  (defadvice company-robe (before web-mode-set-up-ac-sources activate)
    "Set `robe-mode' based on current language before running `company-robe'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (string= web-mode-cur-language "erb")
              (unless robe-mode (robe-mode))
            (if robe-mode (robe-mode -1))
            ))))
  )


(provide 'init-my-prog-framework-web)

;;; init-my-prog-framework-web.el ends here
