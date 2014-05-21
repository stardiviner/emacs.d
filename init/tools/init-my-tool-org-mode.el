;;; init-my-tool-org-mode.el --- init Org-mode

;;; Commentary:

;;; [ minor modes ]
;; - orgtbl-mode :: implements the table editor.
;; - orgstruct-mode :: easy list editing.

;;; Code:

(require 'org)

(require 'org-faces)
(require 'org-fstree)
(require 'org-compat)
;; (require 'org-structure)
(require 'org-table)

(require 'org-timer)
(require 'org-clock)
(require 'org-habit)

(require 'org-pcomplete)
(require 'org-ac)

;;; org-protocol need server start.
;; FIXME:
;; (unless (server-running-p)
;;   (server-start))
;; (require 'org-protocol)

;; FIXME: (require 'org-contacts)

(require 'org-plot)

;; use Org-mode as the default mode for all README files.
(add-to-list 'auto-mode-alist '("README$" . org-mode))


;;; [ Org Modules ]
;; Modules that should always be loaded together with org.el.
(setq org-modules '(org-pcomplete
                    org-faces org-fstree org-table org-compat
                    org-protocol
                    org-timer org-clock org-habit
                    org-info org-bibtex org-docview
                    org-plot
                    ;; TODO wait for BBDBv3
                    ;; org-bbdb
                    org-irc org-gnus org-mhe org-rmail
                    ;; org-w3m
                    ))


;;; [ View ]

;; (load "preview-latex.el" nil t) ; for option `org-startup-with-latex-preview'

;; startup & default view
(setq org-startup-folded t  ; t, 'overview, 'content, 'showall.
      org-startup-indented t
      org-startup-truncated t
      ;; FIXME: error: Can't preview LaTeX fragment in a non-file buffer.
      ;; org-startup-with-inline-images t ; `org-toggle-inline-images'
      ;; org-startup-with-latex-preview t
      ;; org-startup-options
      ;; coordinate grid overlays
      ;; org-table-overlay-coordinates t
      ;; org-table-coordinate-overlays t
      )

(setq org-cycle-separator-lines 2)

;; TODO write a if statement for this.
;; when org-indent-mode is on: sets org-hide-leading-stars to t and org-adapt-indentation to nil.
(setq org-hide-leading-stars nil ; only show one star *
      ;; org-hide-leading-stars-before-indent-mode
      org-hide-emphasis-markers t
      org-hide-block-startup nil ; don't fold block.
      ;; org-hide-block-overlays t ; overlays hiding blocks.
      )

;; 'auto, t, nil. ((heading . auto) (plain-list-item . auto))
(setq org-blank-before-new-entry
      '((heading . auto)
        (plain-list-item . auto)))
(setq org-list-empty-line-terminates-plain-lists t)
(setq org-indirect-buffer-display 'other-window)
(setq org-display-internal-link-with-indirect-buffer nil)



;;; indentation
;; Usage:
;; * List Indent Editing
;;   - [M-S-RET]
;;     + [TAB] to indent deeper sub-list.
;;     + [S-TAB] to indent out list.

(require 'org-indent)

(setq org-indent-mode t)
(setq org-adapt-indentation t) ; means adapt indentation to outline node level.
(setq org-indent-indentation-per-level 2)

(org-indent-mode t)


;;; [ Faces ]

;; TODO
;; set font for all rest font, then override by other face settings.
;; (add-hook 'org-mode-hook '(lambda ()
;;                             (set-face-attribute 'default nil
;;                                                 :family "Gabriola"
;;                                                 ;; :height 120
;;                                                 )))


;; Date: Saturday   27 July 2013
(set-face-attribute 'org-agenda-date nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width -1 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-today nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 5 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-date-weekend nil
                    :foreground "deep pink"
                    :background "#222222"
                    :box '(:color "cyan" :line-width -1 :style nil)
                    :weight 'bold)
(set-face-attribute 'org-agenda-current-time nil
                    :foreground "cyan" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold nil)
;; Diary entry (holidays)
(set-face-attribute 'org-agenda-diary nil
                    :foreground "green"
                    :background nil
                    :bold 'normal
                    :box '(:color "green" :line-width 2 :style nil))
;; clocking
(set-face-attribute 'org-agenda-clocking nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold nil)
;; Day-agenda (W30) -> Week number
(set-face-attribute 'org-agenda-structure nil
                    :foreground "blue"
                    :weight 'extra-bold)
(set-face-attribute 'org-agenda-filter-tags nil
                    :foreground "green yellow")
(set-face-attribute 'org-agenda-dimmed-todo-face nil
                    :foreground "#444444"
                    :background "#222222"
                    :strike-through t)
;; DONE (org agenda log state change tasks, )
(set-face-attribute 'org-agenda-done nil
                    :foreground "#444444"
                    :background "black")
;; Priority
(set-face-attribute 'org-priority nil
                    :foreground "black" :background "green yellow"
                    :weight 'bold
                    :box '(:color "black" :line-width 1 :style nil)
                    )

;;; org-verbatim: =org verbatim highlight=
(set-face-attribute 'org-verbatim nil
                    :background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :family "DejaVu Sans Mono"
                    :bold nil)
;;; Formula face
(set-face-attribute 'org-formula nil
                    :background "green yellow"
                    :foreground "black"
                    :inverse-video nil
                    :box '(:color "green yellow" :line-width 1 :style nil))

;;; cyan style code block colorscheme
;; ;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
;; (set-face-attribute 'org-block-begin-line nil
;;                     :foreground "cyan" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width -1)
;;                     :bold nil :height 80
;;                     )
;; (set-face-attribute 'org-block-end-line nil
;;                     :foreground "cyan" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width -1)
;;                     :bold nil :height 80
;;                     )
;; (set-face-attribute 'org-block-background nil
;;                     :background "#004A5d"
;;                     :foreground nil
;;                     )
;; (set-face-attribute 'org-block nil
;;                     :background "#004A5D"
;;                     :foreground "white"
;;                     )
;; ;; code block face => #+RESULTS: : result.
;; (set-face-attribute 'org-code nil
;;                     :background "#004A5D" :foreground "white"
;;                     :box '(:color "cyan" :line-width 1 :style nil)
;;                     ;; :underline '(:color "cyan") :box nil
;;                     :family "DejaVu Sans Mono"
;;                     :bold nil)

;;; black style code block colorscheme
;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
(set-face-attribute 'org-block-begin-line nil
                    :foreground "cyan" :background "black"
                    :box '(:color "#444444" :line-width 1)
                    :bold t :slant 'normal
                    )
(set-face-attribute 'org-block-end-line nil
                    :foreground "cyan" :background "black"
                    :box '(:color "#444444" :line-width 1)
                    :bold t :slant 'normal
                    )
(set-face-attribute 'org-block-background nil
                    :background "black"
                    :foreground nil
                    )
(set-face-attribute 'org-block nil        ; selected line color in code block begin/end line.
                    :foreground "white" :background "#004A5D"
                    )
;; code block face => #+RESULTS: : result.
(set-face-attribute 'org-code nil
                    :background "#222222" :foreground "orange"
                    ;; :box '(:color "cyan" :line-width 1 :style nil)
                    ;; :underline '(:color "cyan") :box nil
                    :family "DejaVu Sans Mono"
                    :bold nil :box nil)


;; inline code face => src_ruby{require 'something'}
;;
;; (REGEXP . FACE)
;;     Highlight REGEXP with FACE
;; (REGEXP N FACE)
;;     Highlight group N in REGEXP with FACE
;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
;;     Highlight group Ni in REGEXP with FACEi
;;
;; src_lang[:header arguments]{code...}
(font-lock-add-keywords 'org-mode
                        '(("src_\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
                           (1 '(:foreground "cyan" :weight 'bold)) ; "lang" part.
                           (2 '(:foreground "gray" :height 70)) ; [:header arguments] part.
                           (3 'org-code) ; "code..." part.
                           )))

;;; @<kbd>C-h h@</kbd> inline key codes highlight
(font-lock-add-keywords 'org-mode
                        '(("@<kbd>\\([^@]*\\)@</kbd>" 1 'org-code)))

;;; headline faces
;;; the ahead stars face when org indentation. (org-hide)
(set-face-attribute 'org-hide nil
                    :foreground "#002B36" :background "#002B36")
(set-face-attribute 'org-level-1 nil
                    :family "DejaVu Sans Mono"
                    :height 1.5 :weight 'bold
                    :foreground "#FF3870"
                    ;; :box '(:color "black" :line-width 1 :style nil)
                    )
(set-face-attribute 'org-level-2 nil
                    :foreground "cyan"
                    :height 1.2 :weight 'bold
                    ;; :box '(:color "black" :line-width 1 :style nil)
                    )
(set-face-attribute 'org-level-3 nil
                    :foreground "#009E00"
                    :inherit 'org-level-2
                    )
(set-face-attribute 'org-level-4 nil
                    :foreground "#C8C800"
                    :inherit 'org-level-3
                    )
(set-face-attribute 'org-level-5 nil
                    :foreground "#008080"
                    :inherit 'org-level-4
                    )
(set-face-attribute 'org-level-6 nil
                    :foreground "#166DEF"
                    :inherit 'org-level-5
                    )
(set-face-attribute 'org-level-7 nil
                    :foreground "deep sky blue"
                    :inherit 'org-level-6
                    )
(set-face-attribute 'org-level-8 nil
                    :foreground "white"
                    :inherit 'org-level-7
                    )
;;; tags
(set-face-attribute 'org-tag nil
                    :foreground "cyan"
                    :underline nil :weight 'normal :slant 'normal
                    :box '(:color "cyan")
                    :height 90)
;;; checkbox faces
(set-face-attribute 'org-checkbox nil
                    :bold 'normal
                    :box '(:line-width 1 :color "dim gray" :style nil)
                    :foreground "gray"
                    :background nil)
;; * headline [7%] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    ;; :height 0.9
                    :box '(:color "cyan" :line-width 1)
                    :background "#002B36" :foreground "green yellow"
                    :weight 'bold
                    )
(set-face-attribute 'org-checkbox-statistics-done nil
                    :background "#222222" :foreground "black"
                    :box '(:color "cyan" :line-width 1)
                    :strike-through t)
;;; priority faces
(setq org-priority-faces '(:foreground "cyan" :background nil
                                       :bold 'normal
                                       :box t))
;;; link face
(set-face-attribute 'org-link nil
                    :foreground "cyan"
                    :underline "dark cyan")


;; FIXME this seems changed in other buffers too. seems globally.
;; ;;; change Org-Agenda hl-line-mode current line face attribute *buffer locally*.
;; ;; first create new face which is a copy of hl-line-face
;; (copy-face 'hl-line 'hl-line-agenda-face)
;; ;; change what you want in this new face
;; (set-face-attribute 'hl-line-agenda-face nil
;;                     :bold 'normal
;;                     :box '(:color "deep pink" :line-width 2 :style nil)
;;                     )
;; ;; the function to use the new face
;; (defun my-org-agenda-hl-line ()
;;   (set (make-local-variable 'hl-line-face) ; this is how to make it buffer local.
;;        'hl-line-agenda-face)
;;   (hl-line-mode))
;; ;; finally, the hook
;; (add-hook 'org-agenda-mode-hook 'my-org-agenda-hl-line)


;; toggle some displays: e.g. \pi will display as Pi.
(setq org-pretty-entities t
      ;; org-entities 
      org-pretty-entities-include-sub-superscripts t)
;; (add-hook 'org-mode-hook 'org-toggle-pretty-entities) ; special symbols (UTF-8 characters) [C-c C-x \]
;; org makes superscripts and subscripts by directly modifying the display property of the string.
;; TODO change subscript/superscript color.
(setq org-script-display
      '(((raise -0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise 0.3)
         (height 0.7)
         (:foreground "yellow"))
        ((raise -0.5))
        ((raise 0.5))
        )
      )


(setq org-ascii-headline-spacing '(1 . 2))



;;; Org-bullets

;; (load-file "~/.emacs.d/my-init/extensions/org-bullets.el")

(require 'org-bullets nil t)

;;; 希腊/罗马数字: Ⅰ、Ⅱ、Ⅲ、Ⅳ、Ⅴ、Ⅵ、Ⅶ、Ⅷ、Ⅸ、Ⅹ、Ⅺ、Ⅻ
;;; 繁体中文:      壹，貳，叄，肆，伍，陸，柒，捌，玖，拾
;;; ⒈ ⒉ ⒊ ⒋ ⒌

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (defcustom org-bullets-bullet-list
              '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
              "This variable contains the list of bullets.
It can contain any number of symbols, which will be repeated."
              :group 'org-bullets
              :type '(repeat (string :tag "Bullet character")))

            (setq org-bullets-bullet-list
                  '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ"))

            ;; (setq org-bullets-face-name "org-bullet-face")
            ;; (set-face-attribute 'org-bullet-face nil
            ;;                     :foreground "white" :background "black")
            ))


;;; [ Editing ]

(setq org-special-ctrl-a/e t)


;; TODO try this solution
;; solve the [TAB] key between org-indent-mode with yasnippet.
;; (add-hook 'org-mode-hook
;;        (lambda ()
;;          (org-set-local 'yas/trigger-key [tab])
;;          (define-key yas/keymap [tab] 'yas/next-field-group)))
;; TODO try this new version solution.
;; if above code does not work (which it may not with later versions of yasnippet).  Then try this one:
;; (defun yas/org-very-saft-expand ()
;;   (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
;; (add-hook 'org-mode-hook
;;        (lambda ()
;;          ;; yasnippet (using the new org-cycle hooks)
;;          (make-variable-buffer-local 'yas/trigger-key)
;;          (setq yas/trigger-key [tab])
;;          (add-to-list 'org-tab-first-hook 'yas/org-very-saft-expand)
;;          (define-key yas/keymap [tab] 'yas/next-field)
;;          ))


(define-skeleton org-skeleton
  "Header info for an Org file"
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: stardiviner\n"
  "#+email: numbchild@gmail.com\n"
  "#+INFOJS_OPT: \n"
  "#+TAGS" ; for dynamic add tags for complete.
  ;; "#+BABEL: :session *R* :cache yes :results output graphics :exports both :tangle yes \n"
  "-----"
  )


;;; Complete

;; - in buffer + [M-TAB]
;; - in minibuffer

;; more smart org completion option.
(if (featurep 'ido-vertical-mode)
    (setq org-completion-use-ido t)
  (setq org-completion-use-ido nil)
  (setq org-completion-use-iswitchb t)
  (setq org-completion-fallback-command 'hippie-expand))


;;; [ org-pcomplete ]

;;; [ org-ac ]
;;; [o] -- annotation.
(require 'org-ac)
;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-ac")
(org-ac/config-default)


;;; [ Document Structure ]



;;; [ Plain List ]

(setq org-support-shift-select nil) ; - nil to support: [S-up/down] to jump to previous/next item in list.


;;; [ Drawer ]
(setq org-export-with-drawers t) ; t, nil, '(not "LOGBOOK")


;;; [ Tables ]

;; (setq org-enable-table-editor t)


;;; [ Images ]

;;; inline images [C-c C-x C-v] - `org-toggle-inline-images'.
;; (setq org-startup-with-inline-images t)

(setq org-image-actual-width 600)

;; iimage-minor-mode.
;; -----------------------------------------------------------------------------
;; (require 'iimage)
;;
;; (add-to-list 'iimage-mode-image-regex-alist
;;              (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
;;                            "\\)\\]")  1))
;;
;; (defun org-toggle-iimage-in-org ()
;;   "Display images in your org file."
;;   (interactive)
;;   (if (face-underline-p 'org-link)
;;       (set-face-underline-p 'org-link nil)
;;       (set-face-underline-p 'org-link t))
;;   (iimage-mode 'toggle))
;;
;; (define-key org-mode-map (kbd "C-c C-x C-v") 'org-toggle-iimage-in-org)
;; -----------------------------------------------------------------------------




;;; Footnote
(setq org-footnote-auto-label 'confirm)


;;; [ Hyperlinks ]

(setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s") ; xdg-open, kde-open, gnome-open.

;; open IRC link with Emacs ERC.
(setq org-irc-client 'erc)


(setq org-file-apps
      '(;; default
        (auto-mode . emacs)
        ("\\.mm\\'" . default)
        ;; ("\\.x?html?\\'" . default)
        ("\.x?html\'" . "firefox %s")
        ;; PDF
        ("\\.pdf\\'" . default)
        ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
        ;; CHM
        ("\\.chm\\'" . "kchmviewer %s")
        ;; Image
        ("\\.png\\'" . "sxiv %s")
        ("\\.jpg\\'" . "sxiv %s")
        ;; Mind Maps
        ("\\.mm\\'" . "freeplane %s")
        ;; ("\\.mm\\'" . "freemind %s")
        ;; Office
        ;; Open Text Document
        ("\\.odt'" . "libreoffice %s") ; Text Documentation
        ("\\.ods'" . "libreoffice %s") ; Spreadsheet
        ("\\.odp'" . "libreoffice %s") ; Presentation
        ("\\.odf'" . "libreoffice %s") ; Database / Formula
        ))

;; TODO add more link abbrev into this variable.
(setq org-link-abbrev-alist ; Usage: In [C-c C-l] insert link completion.
      '(;; search engines
        ("Google" . "http://www.google.com/search?q=%s")
        ("google" . "http://www.google.com/search?q=%s")
        ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
        ("blekko" . "https://blekko.com/#?q=%s")
        ("Baidu" . "http://www.baidu.com/s?wd=%s")
        ;; Wiki
        ("Wikipedia" . "http://en.wikipedia.org/w/index.php?search=%s")
        ("Wikia" . "http://www.wikia.com/index.php?search=%s")
        ("Baidu Baike" . "http://baike.baidu.com/search/none?word=%s")
        ;; Q & A
        ("Quora" . "https://www.quora.com/search?q=%s")
        ("Stack Overflow" . "http://stackoverflow.com/search?q=%s")
        ("知乎" . "http://www.zhihu.com/search?q=%s&type=question")
        ("Baidu 知道" . "http://zhidao.baidu.com/search?word=%s")
        ("Baidu 经验" . "http://jingyan.baidu.com/search?word=%s")
        ;; Maps
        ("Google Maps" . "http://maps.google.com/maps?q=%s")
        ;; Emacs
        ("Emacs Wiki" . "https://duckduckgo.com/?q=%s+site%3Aemacswiki.org")
        ;; Programming
        ;; Code Search
        ("search code" . "http://searchcode.com/?q=%s")
        ("GitHub" . "https://github.com/search?q=%s")
        ("Bitbucket" . "https://bitbucket.org/repo/all?name=%s")
        ("Google Code" . "https://code.google.com/query/#q=%s")
        ("Launchpad" . "https://launchpad.net/+search?field.text=%s")
        ("Code Project" . "http://www.codeproject.com/search.aspx?q=%s")
        ("CodePlex" . "https://www.codeplex.com/site/search?query=%s")
        ("Gitorious" . "https://gitorious.org/search?q=%s")
        ("SourceForge" . "https://sourceforge.net/directory/?q=%s")
        ("Freecode" . "http://freecode.com/search?q=%s")
        ("Active State" . "http://code.activestate.com/search/#q=%s")
        ("Ohloh Code" . "http://code.ohloh.net/search?s=%s")
        ("Snipplr" . "http://snipplr.com/search.php?q=%s")
        ;; chinese code search
        ("GitCafe" . "https://gitcafe.com/search?keyword=%s")
        ("Geakit" . "https://geakit.com/search?q=%s")
        ("Git OSC (开源中国)" . "https://git.oschina.net/search?search=%s")
        ;; Lisp
        ("lispdoc" . "http://lispdoc.com/?q=%s")
        ;; Ruby
        ("Ruby-Doc" . "http://ruby-doc.com/search.html?q=%s")
        ;; Python
        ("Python 3 Documentation" . "http://docs.python.org/3/search.html?q=%s")
        ;; Perl
        ("Perl CPAN" . "http://search.cpan.org/search?mode=all&query=%s")
        ;; PHP
        ("PHP online documentation" . "http://cn2.php.net/results.php?q=%s&p=manual")
        ;; Bug
        ("bug" . "http://bugzilla/show_bug.cgi?id=%s")
        ;; Book
        ("豆瓣读书" . "http://book.douban.com/subject_search?search_text=%s")
        ;; Movie
        ("豆瓣电影" . "http://movie.douban.com/subject_search?search_text=%s")
        ;; The Pirate Bay
        ("The Pirate Bay (海盗湾)" . "http://thepiratebay.se/search/%s")
        ;; Shopping
        ("TaoBao" . "http://s.taobao.com/search?q=%s")
        ))

;; Add the following bit of code to your startup (after loading org),
;; and you can then use links like:
;;   occur:my-file.txt#regex
;; to open a file and run occur with the regex on it.
(defun org-occur-open (uri)
  "Visit the file specified by URI, and run `occur' on the fragment
  \(anything after the first '#') in the uri."
  (let ((list (split-string uri "#")))
    (org-open-file (car list) t)
    (occur (mapconcat 'identity (cdr list) "#"))))
(org-add-link-type "occur" 'org-occur-open)

;; change [C-c C-o] to open [[file://filename.org]] in current window instead of default in other window.
;; (append) (setq org-link-protocols) ; TODO append custom link protocols into this list.
;; append "man:" protocol to it, and "firefox:" protocol,
;;; [C-c C-o] open link at point.
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))
;; [C-u C-c C-o]
;; (let ((org-link-frame-setup [whatever-policites]))
;;   (org-open-at-point))

;; (setq org-open-at-point-functions) ; a hook

;; TODO:
;; (defadvice org-open-at-point (before create-file-when-org-link-invalid activate)
;;   ())

(setq org-return-follows-link nil) ; to follow links with [RET], rather 2 key combo.

(defun find-file-at-point-ex ()
  "Open link, if does not exist, then create a file which filename with word at current point.

This is especially for create Org files."
  (interactive)
  (let ((filename
         (expand-file-name
          (thing-at-point 'filename))))
    (when (or
           (file-exists-p filename)
           (y-or-n-p (format "Create %s" filename)))
      (find-file filename))))


;;; [ TODOs Items ]

(setq org-open-directory-means-index-dot-org t)

;; log
(setq org-log-done 'time) ; 'time/'note . add timestamp or note on done tasks.
(setq org-log-into-drawer t) ; insert state change notes and time stamps into a drawer.

;;; statistics -> [1/10] or [15%]
(setq org-provide-todo-statistics t
      org-hierarchical-todo-statistics nil ; t: covers just direct children, nil: covers all entries.
      ;; you can use property for only single todos sub-tree. ->  :COOKIE_DATA: recursive
      org-checkbox-hierarchical-statistics nil ; nil: covers all entries.
      org-enforce-todo-dependencies nil ; enforce parent and sub-tasks DONE. otherwise blocked.
      )


;;; time repeat
(setq org-todo-repeat-to-state "REPEAT"
      org-log-repeat 'time
      org-agenda-repeating-timestamp-show-all t
      )


;;; [ org-linkany ]

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-linkany")

;;; Usage:

;; (require 'org-linkany)

;; (setq org-linkany/url-source-collection
;;       '((org-linkany/source-link-in-org-buffer . org-linkany/get-candidate-link-value)
;;         (org-linkany/source-url-in-other-buffer)
;;         (helm-source-w3m-bookmarks . helm-w3m-bookmarks-get-value)
;;         (anything-c-source-w3m-bookmarks . anything-c-w3m-bookmarks-get-value)
;;         (helm-source-firefox-bookmarks . helm-firefox-bookmarks-get-value)
;;         (anything-c-source-firefox-bookmarks . anything-c-firefox-bookmarks-get-value)
;;         (helm-c-source-hatena-bookmark . org-linkany/get-hatena-bookmark-candidate-url)
;;         (anything-c-source-hatena-bookmark . org-linkany/get-hatena-bookmark-candidate-url))
;;       )

;; (setq org-linkany/browse-function 'browse-url-firefox)


;;; [ Properties and Columns ]

;;; properties
(setq org-global-properties ; will be combined with constant `org-global-properties-fixed'
      '(("Effort" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("Title" . nil)
        ("Writer" . "stardiviner")
        ("Author" . "stardiviner")
        ;; ("Time" . ,(format-time-string "[%Y-%m-%d %a %H:%M:%S]")) ; [2013-07-09 Tue 13:69:56]
        ;;; (current-time-string), (format-time-string "%Y-%m-%d %H:%M:%S")
        ("Source" . nil)
        ("Original" . nil)
        ))

(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %6effort(EFFORT){:}") ; default: "%25ITEM %TODO %3PRIORITY %TAGS"



;;; [ Dates and Times ]


;;; [ Clock ]

(require 'org-clock)

;; to save the clock history across Emacs sessions.
(setq org-clock-persist 'history
      org-clock-persistence-insinuate t
      org-clock-in-resume t    ; resume when clock in.
      org-clock-into-drawer t  ; Save clock data and notes in the LOGBOOK drawer
      org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
      )

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED"
      ;; TODO: org-clock-heading ""
      ;; TODO: use a sound file.
      org-clock-sound t
      org-clock-persist t
      ;; org-clock-task-overrun
      org-clock-continuously nil ; don't continue on last clock out.
      ;; org-clock-persist-file
      ;; org-clock-leftover-time
      org-clock-out-when-done t
      org-clock-mode-line-total 'auto
      org-clock-mode-line-entry t
      ;; org-clock-task-overrun-text
      org-clock-persist-query-save nil
      org-clock-persist-query-resume t
      org-clock-clocked-in-display 'mode-line
      ;; org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)
      org-clock-report-include-clocking-task t
      ;; org-agenda-clockreport-mode
      ;; org-agenda-start-with-clockreport-mode t
      org-clock-goto-may-find-recent-task t
      ;; org-clock-total-time-cell-format "*%s*"
      )

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
(add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer)

(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)



;;; org-timer

;; (require 'org-timer)
;; (add-to-list 'org-modules 'org-timer)

(setq org-timer-default-timer 25)       ; Pomodoro time management technique.
(setq org-timer-display 'mode-line)

;; Modify the org-clock-in so that a timer is started with the default value
;; except if a timer is already started :
(add-hook 'org-clock-in-hook
          '(lambda ()
             (if (not org-timer-current-timer)
                 (org-timer-set-timer '(16)))))



;;; Effort estimate
(setq org-effort-property "Effort"
      ;; conversion factor to minutes for an effort modifier.
      ;; (MODIFIER . MINUTES)
      org-effort-durations '(("hours" . 60)
                             ("days" . 480)
                             ("weeks" . 2400)
                             ("months" . 9600)
                             ("years" . 96000)))


;;; [ Org Habit ]

;; (require 'org-habit)

;;; Example:
;; * TODO write journal
;;   SCHEDULED: <2014-01-28 Tue .+1d/3d>
;;   - State "DONE"       from "TODO"       [2014-01-27 Mon 20:50]
;;   - State "DONE"       from "TODO"       [2013-12-03 Tue 06:37]
;;   - State "DONE"       from "TODO"       [2013-12-02 Mon 21:54]
;;   :PROPERTIES:
;;   :LAST_REPEAT: [2014-01-27 Mon 20:50]
;;   :STYLE:    habit
;;   :END:

(setq org-habit-show-habits t           ; show habits in agenda.
      org-habit-show-all-today nil ; show all habits' consistency graph in today's agenda.
      org-habit-show-habits-only-for-today t
      org-habit-graph-column 70
      org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-today-glyph ?!
      org-habit-completed-glyph ?-
      )

;;; org-habit
(set-face-attribute 'org-habit-clear-future-face nil ; for future days on which a task shouldn't be done yet.
                    :background "#001D25")
(set-face-attribute 'org-habit-alert-future-face nil ; for days on which a task is due.
                    :background "gray")
(set-face-attribute 'org-habit-ready-face nil ; for days on which a task should start to be done.
                    :background "cyan")
(set-face-attribute 'org-habit-overdue-face nil ; for days on which a task is overdue.
                    :background "dark red")
;; -----------------------------------------------------------------------------------------
(set-face-attribute 'org-habit-clear-face nil ; for days on which a task shouldn't be done yet.
                    :background "white")
(set-face-attribute 'org-habit-ready-future-face nil ; for days on which a task should start to be done.
                    :background "dark cyan")
(set-face-attribute 'org-habit-alert-face nil ; for days on which a task is due.
                    :background "yellow")
(set-face-attribute 'org-habit-overdue-future-face nil ; for days on which a task is overdue.
                    :background "red")


;; TODO: create an key binding for all necessary steps for create a habit. (reference in Org-mode.org file)
(defun org-habit-apply ()
  "Apply org-habit on this task."
  (interactive)
  (lambda ()
    (org-todo "HABIT")
    (org-insert-property-drawer)        ; TODO: :STYLE: habit (value)
    (org-schedule)))

(define-key org-mode-map (kbd "C-c C-x h") 'org-habit-apply)


;;; [ Capture - Refile - Archive ]

;;; Capture:
;; - [C-c o c] :: org-capture
;; - [C-c C-c] :: finalize org-capture
;; - [C-c C-k] :: org-capture-kill (abort capture)
;;; Refile:
;; - [C-c M-w] :: org-copy.
;; - [C-c C-w] :: org-refile.
;; - [C-u C-c C-w] :: Use the refile interface to jump to a heading.
;; - [C-u C-u C-c C-w] :: org-refile-goto-last-stored. jump to the location where `org-refile' last moved a tree to.
;; - [C-2 C-c C-w] :: refile as the child of the item currently being clocked.
;; - [C-3 C-c C-w] :: refile and keep the entry in the place.
;; - [C-0 C-c C-w] :: `org-refile-cache-clear'. clear the target cache.

(require 'org-capture)

(setq org-default-notes-file (concat org-directory "/Capture/notes.org"))

(setq org-capture-templates
      '(("c" "Capture"
         entry (file+headline "~/Org/Capture/Capture.org" "Capture")
         "\n* TODO %^{prompt}\n\n%i\n\n%a\n\n%?")
        ("t" "Capture a task into Tasks"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n")
        ("u" "Capture an entry into Buy list"
         entry (file+headline "~/Org/Tasks.org" "Buy")
         "\n* TODO %^{prompt}\n\n%i\n\n%?\n\n")
        ("b" "Blog (jekyll)"
         entry (file+datetree+prompt "~/Org/Diary/Public/Blog.org")
         "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?")
        ;; ("j" "Journal"
        ;;  entry (file+datetree "~/Org/Journal.org" "Journal")
        ;;  "\n* %^{prompt}\nEntered on %U\n %i\n %?\n\n")
        ;; TODO Contacts
        ;;
        ;; Issues, Bugs, Features
        ("b" "record a bug to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer (电子科技)" "Bugs")
         "\n* BUG %^{prompt}\n\n%i\n\n%?\n\n")
        ("i" "record an issue to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer (电子科技)" "Issues")
         "\n* ISSUE %^{prompt}\n\n%i\n\n%?\n\n")
        ("f" "record a feature to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer (电子科技)" "Features")
         "\n* FEATURE %^{prompt}\n\n%i\n\n%?\n\n")
        ;; bookmark
        ("m" "Add an URL to bookmark database"
         entry (file+headline "~/Org/Wiki/Data/Bookmarks/Bookmarks.org" "Others")
         "\n* %^{prompt}\n\n%A\n\n%?\n\n")
        ;; knowledge
        ;; thought
        ;; "~/Org/Wiki/Wiki/Thought/Thought.org" "My Thought"
        ))

;; To define special keys to capture to a particular template without
;; going through the interactive template selection, you can create your
;; key binding like this:
;;
;; (define-key global-map "\C-cx"
;;   (lambda () (interactive) (org-capture nil "x")))

;;; FIXME
;;  ("c" "Contacts" entry (file+headline "~/Org/Contacts/Contacts.org")
;;          "* %(org-contacts-template-name) %^g
;; %(org-contacts-template-email)
;; :PROPERTIES:
;; :URL:
;; :WORK:
;; :HOME:
;; :MOBILE:
;; :LOCATION:
;; :BIRTHDAY:
;; :NOTE:
;; :END:")


;; ("c" "Contacts" entry (file+headline "~/Org/Contacts/Contacts.org")
;;          "* %^{name} %^G
;; :PROPERTIES:
;; :EMAIL: %?
;; :URL: %:url
;; :WORK:
;; :HOME:
;; :MOBILE:
;; :LOCATION:
;; :BIRTHDAY: %:date
;; :NOTE:
;; :END:")



;;; Refile

;; - [C-c C-w] :: org-capture-refile finalize
;; - [C-u C-c C-w] :: use the refile interface to jump to a heading.
;; - [C-u C-u C-c C-w] :: jump to the location where `org-refile' last moved a tree to.
;; - [C-2 C-c C-w] :: refile as the child of the item currently being clocked.
;; - [C-3 C-c C-w] :: refile-keep.
;; - [C-0 C-c C-w] :: refile-cache-clear.

(setq org-refile-keep nil       ; t: means org-refile will copy instead of refile.
      org-refile-markers nil    ; TODO
      ;;; Refile targets include this file and any file contributing to the
      ;;; agenda - up to 5 levels deep
      org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
      ;;; Targets start with the file name - allows creating level 1 tasks
      ;; org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      ;; org-refile-use-outline-path ; TODO
      org-refile-target-verify-function t
      org-refile-use-outline-path nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-active-region-within-subtree t ; allow refile region, first line of region as headline.
      )



;;; [ Archive ]

;; - [C-c C-x C-a] -- org-archive-subtree-default. (archive current entry)
;; - [C-c C-x C-s] -- org-archive-subtree. (archive subtree).

(setq org-archive-location "%s_archive::"
      org-archive-save-context-info '(time file olpath category todo itags ltags)
      org-archive-mark-done nil
      org-archive-stamp-time t
      org-archive-reversed-order nil
      )


;;; [ Attachments ]

;; - [C-c C-a] :: `org-attach'.



;;; [ Agenda Views ]

(require 'org-agenda)

(setq org-agenda-include-deadline t)
(setq org-agenda-log-mode-items '(closed clock)
      org-agenda-log-mode-add-notes t
      org-agenda-start-with-log-mode t)

(setq org-agenda-include-diary t)
(setq org-agenda-diary-file 'diary-file)
(setq org-agenda-insert-diary-strategy 'date-tree)
;; (org-agenda-diary-entry)
;; (org-agenda-diary-entry-in-org-file)
;; (org-agenda-get-day-entries)
;; set Org agenda to search in ~/Org directory *recursively*.
(setq org-directory "~/Org")
;;; How can include all org files in a directory in my agenda?
;; You can simply include the directory (as one of the items) in the value of
;; the variable org-agenda-files:
(setq org-agenda-files '("~/Org/INDEX.org"
                         "~/Org/Tasks.org"
                         "~/Org/Daily.org"
                         "~/Org/Work/Work.org"
                         "~/Org/Projects/Projects.org"
                         "~/Org/Capture/"
                         "~/Org/Wiki/Learning/Learning.org"
                         ;; "~/Org/Wiki/Learning/MyLearningPlan/Learn Programming.org"
                         "~/Org/Wiki/Wiki.org"
                         "~/Org/Wiki/Kung Fu/Kung Fu.org"
                         ))

;; 2.
;; (setq org-agenda-files (file-expand-wildcards "~/Org/*.org")) ; Including all org files from a directory into the agenda
;; 3.
;;; Sync with Dropbox
;; (autoload 'find-lisp-find-files "find-lisp")
;; (setq org-agenda-files
;;       (append (find-lisp-find-files "~/Sync/Dropbox" "\.org$") org-agenda-files))
;; (setq org-agenda-files (find-lisp-find-files "~/Org" "\.org$"))
;; (setq org-agenda-files
;;       (cl-remove-duplicates (append '("~/Org/GTD/"
;;                              "~/Org/Work/"
;;                              "~/Org/Capture/"
;;                              "~/Org/Projects/"
;;                              "~/Org/Wiki/Learning/My_Learning_Plan.org"
;;                              "~/Org/Wiki/Learning/Learn-English.org"
;;                              )
;;                            (find-lisp-find-files "~/Org" "\.org$"))))

;;; TODO how to remove duplicated list elements, because there are duplicated
;;; entries in Org-mode agenda.

;;; ISSUE and seems this (find-lisp-find-files) list in variable
;;; org-agenda-files can not update when already has new files added.

;; -2
;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
;; (defun find-org-file-recursively (directory &optional filext)
;;   "Return .org and .org_archive files recursively from DIRECTORY.
;; If FILEXT is provided, return files with extension FILEXT instead."
;;   (interactive "DDirectory name: ")
;;   (let* (org-file-list
;;          (case-fold-search t) ; filesystems are case sensitive
;;          (fileregex (if filext (format "^[^.#].*\\.\\(%s$\\)" filext)
;;                       "^[^.#].*\\.\\(org$\\|org_archive$\\)"))
;;          (cur-dir-list (directory-files directory t "^[^.#].*"))) ; exclude .*
;;     ;; loop over directory listing
;;     (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
;;       (cond
;;        ((file-regular-p file-or-dir) ; regular files
;;         (if (string-match fileregex file-or-dir) ; org files
;;             (add-to-list 'org-file-list file-or-dir)))
;;        ((file-directory-p file-or-dir)
;;         (dolist (org-file (find-org-file-recursively file-or-dir filext)
;;                           org-file-list) ; add files found to result
;;           (add-to-list 'org-file-list org-file)
;;           )))
;;       )
;;     ))
;; 0
;; (split-string (shell-command-to-string "find ~/Org -name '*.org' -print0"))
;; or
;; (find-lisp-find-files "~/Org" "\\.org$")
;; 1
;; (setq org-agenda-files (directory-files "~/Org/" :full "^[^.]"))
;; 2
;; (setq org-agenda-files (list "~/Org/"))
;; 3
;; (eval-after-load 'org
;;   '(progn
;;      ;; add all org files in the org directory to the agenda.
;;      (mapcar
;;       (lambda (file)
;;         (add-to-list 'org-agenda-files file))
;;       (directory-file (expand-file-name "~/Org/") t "\\.org"))
;;      ))
;; 4
;; (setq org-agenda-files (file-expand-wildcards "~/Org/*.org"))
;; 5
;; (defvar my-org-directories
;;   (list
;;    "~/Org/"
;;    ))
;; (defun my-org-files ()
;;   (mappend '(lambda (directory)
;;               (directory-files directory t "\\.org$"))
;;            my-org-directories))
;; (setq org-agenda-files (my-org-files))
;; 6
;; (setq org-agenda-text-search-extra-files) ; [C-c a s]
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-delay-if-deadline t) ; nil, t, 'post-deadline.
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-scheduled-leaders '("Scheduled: " "%2d days /// "))
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-show-all-dates t)
(setq org-agenda-show-outline-path t)
(setq org-deadline-warning-days 7) ; determines how far in advance items with
                                        ; deadlines will show up in the agenda.
(setq org-extend-today-until 6) ; I work late at night! Extend my current day past midnight.
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-window-frame-fractions '(0.5 . 0.75)) ; the min and max height of the agenda window as a fraction of frame height.
(setq org-agenda-span 'week)
(setq org-agenda-dim-blocked-tasks t)


;; toggle log mode in agenda buffer. show all possible log items.
(setq org-agenda-show-log t)
(setq org-agenda-log-mode-items '(closed clock state)
      org-agenda-log-mode-add-notes t)

;;; Custom Agenda Commands
;; [<]  :: to restrict to current buffer.
;; [<<] :: to restrict to current sub-tree.
(setq org-agenda-custom-commands
      '(("d" "Agenda and all TODO's"
         ((agenda "")
          (alltodo "")))
        ("u" "Urgent tasks"
         ((search "[#A]")
          (todo "Urgent")
          (tags "Prepare-Today")))
        ("t" "all todo entries"
         todo ""
         ((org-agenda-buffer-name "*Todo List*")))
        ("f" "Tasks to start in the future/someday."
         todo "SOMEDAY")
        ("p" "Project process - project, BUG, ISSUE, Features"
         ((todo "project")
          (todo "BUG")
          (todo "ISSUE")
          (todo "Features")))
        ("v" "Version Control System"
         ((todo "commit")
          (todo "pull")
          (todo "pull-request")
          (todo "push")
          (todo "fork")
          (todo "merge")))
        ;; ("i" tags-todo "CATEGORY=\"Task\"")
        ;; ("w" tags-todo "CATEGORY=\"Work\"")
        ))


;;; Time Grid
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;         #("----------------" 0 16
;;           (org-heading t))
;;         (800 1000 1200 1400 1600 1800 2000)))
(setq org-agenda-use-time-grid t)
(setq org-agenda-timegrid-use-ampm nil)
(setq org-agenda-show-current-time-in-grid t)


;;; TODOs status
;; `|` separate finished and unfinished two statuses, will add timestamp when finished.
;; `(t)` set shortcut
;; `(d!)` add timestamp
;; `(d@)` need add note declaration
;; `(d@/!)` add timestamp and note
(setq org-todo-keywords
      '(;; Clock
        (sequence "STARTED(@/@)" "|" "DONE(d@/!)")
        ;; habits
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Status
        (sequence "TODO(t@/!)" "Urgent(u!)" "SOMEDAY(s!)" "FAILED(X@/!)" "CACELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "|" "DONE(d@/!)")
        ;; Version Control System: Git
        (sequence "commit(!)" "pull(!)" "push(!)" "pull-request(!)"
                  "fork(!)" "merge(!)" "|" "DONE(d@/!)")
        ;; Types
        ;; use (@/!) to record/log info reference source link URL and timestamp.
        (type "Org(o@/!)" "code(c@/!)" "project(p@/!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(x@/!)" "Outside(l@/!)" "|" "DONE(d@/!)")
        ;; Work
        (type "Work(w@/!)" "Meeting(m@/!)" "|" "DONE(d@/!)")
        ))
(setq org-todo-keyword-faces
      (quote (;;; todos
              ("TODO" :foreground "white" :weight bold)
              ("Urgent" :foreground "red" :background "black"
               :weight bold :box '(:color "red" :line-width 2 :style nil))
              ("STARTED" :foreground "green" :weight bold)
              ("HABIT" :foreground "cyan" :background "black" :weight bold
               :box '(:color "cyan" :line-width 1 :style nil))
              ("SOMEDAY" :foreground "dim gray" :weight bold)
              ("DONE" :foreground "black" :weight bold :strike-through t)
              ("FAILED" :foreground "#444444" :weight bold)
              ("CANCELLED" :foreground "#444444" :weight bold)
              ;;; fixme
              ("BUG" :foreground "black" :background "red" :weight bold
               :box '(:color "red" :line-width 1 :style nil))
              ("ISSUE" :foreground "black" :background "red" :weight bold
               :box '(:color "dark red" :line-width 1 :style nil))
              ("ERROR" :foreground "red" :weight bold
               :box '(:color "red" :line-width 1 :style nil))
              ("FIXME" :foreground "black" :background "red" :weight bold
               :box '(:color "red" :line-width 1 :style nil))
              ("FEATURE" :foreground "cyan" :weight bold
               :box '(:color "cyan" :line-width 1 :style nil))
              ;;; version control
              ("commit" :foreground "sky blue")
              ("pull" :foreground "orange")
              ("push" :foreground "deep pink")
              ("pull-request" :foreground "lawn green")
              ("fork" :foreground "dark slate blue")
              ("merge" :foreground "cornflower blue")
              ("accept" :foreground "light blue")
              ;;; types
              ("Org" :foreground "cyan" :backgrund "#004A5D" :weight bold
               :box '(:color "cyan" :line-width 1 :style nil))
              ("code" :foreground "white" :background "#004A5D"
               :box '(:color "cyan" :line-width 1 :style nil))
              ("project" :foreground "white" :background "#004A5D"
               :box '(:color "cyan" :line-width 1 :style nil))
              ;; life
              ("SEX" :foreground "deep pink" :weight bold
               :box '(:color "deep pink" :line-width  1 :style nil))
              ("Outside" :foreground "yellow"
               :box '(:color "yellow" :line-width 1 :style nil))
              ;; work
              ("Work" :foreground "orange"
               :box '(:color "black" :line-width 1 :style nil))
              ("Meeting" :foreground "cornflower blue"
               :box '(:color "cyan" :line-width 1 :style nil))
              )))


;;; [ Tags ]

(setq org-auto-align-tags t
      org-export-with-tags t
      org-tags-history t
      org-tags-column -80 ; position
      ;; inheritance
      org-use-tag-inheritance t
      org-tags-match-list-sublevels t
      org-fast-tag-selection-single-key nil
      org-tags-exclude-from-inheritance '(("knowledge" . nil))
      )

(setq org-tag-persistent-alist
      '(;; Knowledge aspects
        (:startgroup . nil)
        ("knowledge" . nil)
        (:grouptags . nil)
        ("Thought" . nil) ("Philosophy") ("Psychology") ("Literature")
        ("Computer") ("Mathematics") ("Math")
        ("Strategies")
        ("Science") ("Finance") ("Business") ("Economy")
        ("History") ("Politics") ("Society")
        ("Medicine")
        (:endgroup . nil)
        ;; Programming
        (:startgroup . nil)
        ("@Programming" . ?p)
        (:grouptags . nil)
        ("code" . ?c) ("script" . ?s)
        ("Linux" . ?l) ("Mac") ("BSD") ("Windows")
        ("Emacs" . ?e) ("Vim")
        ("regexp")
        ("Git" . ?g)
        (:endgroup . nil)
        ;; Programming Languages
        (:startgroup . nil)
        ("Programming-Languages" . ?L)
        (:grouptags . nil)
        ("Shell") ("Bash") ("Zsh")
        ("Lisp") ("Common Lisp") ("Emacs Lisp") ("Guile") ("Scheme")
        ("Haskell")
        ("Ruby") ("Python") ("Perl") ("PHP") ("Erlang")
        ("C") ("C++") ("Go") ("Lua") ("Rust")
        ("Assembly") ("gas") ("nasm") ("Intel") ("att")
        ("R") ("Processing")
        ("Database") ("SQL") ("NoSQL") ("NewSQL")
        ("XML") ("JSON") ("MathML")
        ("Octave") ("Matlab")
        ("HTML") ("HTML5") ("CSS") ("CSS3")
        ("JavaScript") ("CoffeeScript") ("Dart")
        ("OCaml") ("Scala") ("Verilog") ("Julia") ("Delphi") ("Halide")
        ("J")
        (:endgroup . nil)
        ))

(setq org-tag-alist
      '( ;; personal
        (:startgroup . nil)
        ("types")
        (:grouptags . nil)
        ;; types
        ("wiki" . ?k) ("Org" . ?o) ("idea" . ?i)
        ("appointment" . ?a) ("meeting" . ?m) ("SEX" . ?X)
        ;; time
        ("Prepare-Today" . ?d) ("tomorrow" . ?t) ("future" . ?f)
        ;; places
        ("Company" . ?N) ("Home" . ?H) ("Computer" . ?C) ("Phone" . ?P)
        (:endgroup . nil)
        ;; Work
        (:startgroup . nil)
        ("work" . ?w)
        (:grouptags . nil)
        ;; Green Town
        ("Company" . nil)
        (:endgroup . nil)
        ))

(setq org-agenda-max-tags nil
      ;; org-tag-alist-for-agenda
      )

(setq org-group-tags t ; enable group tags
      ;; org-tag-groups-alist nil
      ;; org-tag-groups-alist-for-agenda nil
      )

(setq org-tag-faces
      (quote (("wiki" :foreground "green yellow")
              ("Org" :foreground "green yellow")
              ("Computer" :foreground "green" :background "black")
              ("Life" :foreground "black" :background "DimGray")
              ("SEX" :foreground "deep pink" :weight bold)
              ("code" :foreground "lawn green" :weight bold)
              ("program" :foreground "lawn green" :weight bold)
              ("Linux" :foreground "yellow" :weight bold)
              ("Mac" :foreground "#444444" :background "black" :weight bold)
              ("Emacs" :foreground "dodger blue" :weight bold)
              ("Vim" :foreground "green" :weight bold)
              ("Lisp" :foreground "deep pink" :weight bold)
              ("Ruby" :foreground "red" :weight bold)
              ("Python" :foreground "yellow" :weight bold)
              ("C" :foreground "gold" :weight bold)
              ("C++" :foreground "gold" :weight bold)
              ("Go" :foreground "gold" :weight bold)
              ("Bash" :foreground "sea green")
              ("Zsh" :foreground "sea green" :weight bold)
              )))



;;; [ Markup ]



;;; [ Exporting ]

;; TODO how to set Org export directory.
;; (setq org-export-html-link-home "~/Org/")
;; (setq org-export-publishing-directory "~/Publish/Org")

;; The last level which is still exported as a headline. Inferior levels will produce itemize lists when exported.
(setq org-export-backends '(ascii html icalendar latex md)
      org-export-babel-evaluate t ; code evaluation during export.
      ;; org-export-headline-levels 6
      )


;; FIXME:
;;; Export Org to latex PDF with color for source code block highlighting.
;; (require 'org-latex)
;; (require 'org-latex-code)
;; (require 'org-latex-color)
;;
;; (setq org-export-latex-packages-alist '("" "listings"))
;; (setq org-export-latex-packages-alist '("" "color"))
;; (setq org-export-latex-listings t)


;;; [ Publishing ]

;; - [C-c C-e P x] -- (org-publish)
;; - [C-c C-e P p] -- (org-publish-current-project)
;; - [C-c C-e P f] -- (org-publish-current-file)
;; - [C-c C-e P a] -- (org-publish-all)

(require 'org-publish)

;; (require 'org-blog)
;; (require 'org-jekyll)


;; Each element of the list configures one project, and may be in one of the two following forms:
;; In both cases, projects are configured by specifying property values. A
;; project defines the set of files that will be published, as well as the
;; publishing configuration to use when publishing those files. When a project
;; takes the second form listed above, the individual members of the :components
;; property are taken to be sub-projects, which group together files requiring
;; different publishing options. When you publish such a “meta-project”, all the
;; components will also be published, in the sequence given.

(setq org-publish-project-alist
      '(("Blog"
         :base-directory "~/Org/Diary/Public"
         :recursive t
         )
        ("Wiki"
         :base-directory "~/Org/Wiki"
         :base-extension 'any
         ;; :exclude
         ;; :include
         :recursive t
         )
        ;; ("Gallery"
        ;;  ;; TODO :base-directory "~/Org/"
        ;;  :recursive t
        ;;  )
        ("Website"
         :components ("Blog" "Gallery")
         ;; TODO can I set those variable at here for all of above publishings ?
         :publishing-directory "~/WWW/Org-publish"
         :publishing-function org-html-publish-to-html
         ;; :preparation-function
         ;; :completion-function
         :htmlized-source t
         ;; :headline-levels 4
         :section-number t
         ;; :language
         ;;; [ author ]
         :with-author "stardiviner"
         :with-email "numbchild@gmail.com"
         :with-footnotes "Get over the world!"
         ;; :with-latex
         :with-sub-superscript t
         :with-tables t
         :with-tags t
         ;;; [ tasks ]
         ;; :with-tasks t
         ;; :with-planning
         :with-todo-keywords
         ;; :with-priority
         :with-timestamps t
         ;; :with-toc
         ;;; [ html ]
         ;; TODO :html-doctype "html5"
         ;; TODO :html-xml-declaration
         ;; :html-link-up
         ;; :html-link-home
         ;; :html-head
         ;; :html-head-extra
         ;; :html-head-include-default-style
         ;; :html-head-include-scripts
         ;; :html-inline-images t
         ;; :html-preamble
         ;; :html-postamble
         ;; :html-table-attributes
         ;;; [ sitemap ]
         :auto-sitemap t
         ;; When non-nil, remove filenames' extensions from the generated sitemap. Useful to have cool URIs.
         :sitemap-sans-extension t
         ;;; [ index ]
         ;; When non-nil, generate in index in the file `theindex.org' and publish it as `theindex.html'.
         ;;
         ;; The file will be created when first publishing a project with the
         ;; :makeindex set. The file only contains a statement #+INCLUDE:
         ;; "theindex.inc". You can then build around this include statement by
         ;; adding a title, style information, etc.
         :makeindex t
         )
        )
      )

(setq org-publish-use-timestamps-flag nil)

;; Publishing to a local directory is also much faster than to a remote one, so
;; that you can afford more easily to republish entire projects. If you set
;; `org-publish-use-timestamps-flag' to nil, you gain the main benefit of
;; re-including any changed external files such as source example files you
;; might include with #+INCLUDE:. The timestamp mechanism in Org is not smart
;; enough to detect if included files have been modified.


;;; HTML: HTML5

(setq org-html-doctype "html5"
      ;; org-html-doctype-alist
      )

(setq org-html-html5-fancy t)


;;; Babel

;; (unless (package-installed-p 'ob-mongo)
;;   (package-install 'ob-mongo))

;; (require 'ob-mongo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)                     ; Emacs Lisp
   (org . t)                            ; Org-mode
   (sh . t)                             ; Shell
   ;; TODO (makefile . t)                       ; Make
   (ruby . t)                           ; Ruby
   (python . t)                         ; Python
   (perl . t)                           ; Perl
   (C . t)                              ; C
   ;; (C++ . t)                            ; C++
   (java . t)                           ; Java
   (R . t)                              ; R
   (sql . t)                            ; SQL
   (sqlite . t)                         ; SQLite
   (matlab . t)                         ; MATLAB
   (octave . t)                         ; Octave
   (calc . t)                           ; calc
   (awk . t)                            ; Awk
   (lisp . t)                           ; Lisp
   (scheme . t)                         ; Scheme
   ;; (arc . t)                            ; Arc
   (clojure . t)                        ; Clojure
   (haskell . t)                        ; Haskell
   (ocaml . t)                          ; Objective Caml
   ;; TODO (prolog . t)                         ; Prolog
   ;; TODO (datalog . t)                        ; Datalog
   (js . t)                             ; JavaScript
   (css . t)                            ; CSS
   (latex . t)                          ; LaTeX
   (ditaa . t)                          ; ditaa
   (ledger . t)                         ; ledger support in Babel
   (gnuplot . t)                        ; gnuplot
   (dot . t)                            ; Dot
   ;; (sml . t)                            ; from extension ob-sml
   (sass . t)                           ; Sass
   ;;; extras
   ;; (mongo . t)                          ; MongoDB
   ))

(setq org-confirm-babel-evaluate t)     ; org-babel-evaluate confirm.
(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil)
(setq org-confirm-shell-link-function 'yes-or-no-p)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;;; source block header arguments
(setq org-babel-default-header-args    ; #+BEGIN_SRC ruby :result [output/value]
      '((:session . "none")
        (:results . "replace output") ; "replace output", "replace", "file", "output"
        (:exports . "both")           ; "both", "results", "code" "none"
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "both")
        (:padnewline . "yes")
        ))

;;; inline source code header arguments
(setq org-babel-default-inline-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "both")
        ))

;;; language-specific header arguments
;;; `org-babel-default-header-args:<lang>' where `<lang>' is the name of the language.  See the language-specific documentation available online at `http://orgmode.org/worg/org-contrib/babel'.
(setq org-babel-default-header-args:R
       '((:session . "no")
         (:exports . "both")
         (:results . "replace")
         ))


;;; [ Working With Source Code ]

;;; source code (src)
(setq
 ;; fontify code in code blocks. (highlight code in exported HTML)
 ;; FIXME (slow-down/suspend on Emacs startup)
 org-src-fontify-natively t
 ;; preserve leading whitespace characters on export, for tangling languages such as Python.
 org-src-preserve-indentation t
 ;; default org-mode src indentation when `org-src-preserve-indentation' is non-nil.
 org-edit-src-content-indentation 2
 ;; controls the way Emacs windows are rearranged when the edit buffer is created.
 org-src-window-setup 'current-window
 ;; switch to open edit buffer without asking.
 org-src-ask-before-returning-to-edit-buffer nil
 org-edit-src-auto-save-idle-delay 0 ; 0: don't auto save.
 ;; org-edit-src-picture
 ;; org-edit-src-overlay
 ;; org-tab-first-hook '(org-hide-block-toggle-maybe
 ;;                      org-src-native-tab-command-maybe
 ;;                      org-babel-hide-result-toggle-maybe
 ;;                      org-babel-header-arg-expand)
 ;; org-babel-pre-tangle-hook '(save-buffer)
 org-babel-tangle-lang-exts '(("latex" . "tex")
                              ("emacs-lisp" . "el")
                              ("lisp" . "lisp")
                              ("ruby" . "rb")
                              ("python" . "py")
                              ("R" . "R")
                              ("sql" . "sql")
                              ("sh" . "sh")
                              ("ocaml" . "ml")
                              ("haskell" . "hs")
                              ("clojure" . "clj")
                              ("awk" . "awk")
                              ("C" . "c")
                              ("Go" . "go")
                              ("C++" . "cpp")
                              ("perl" . "pl")
                              ("js" . "js")
                              ("css" . "css")
                              ("java" . "java")
                              )
 )

;;; babel eval result
(setq org-babel-inline-result-wrap "=%s="
      org-babel-hide-result-overlays nil
      ;; org-babel-results-keyword "RESULTS"
      org-export-babel-evaluate t
      )


;;; LaTeX
(setq org-babel-latex-htlatex t)
;; (setq org-format-latex-header)
;; (setq org-format-latex-options
;;       '(:foreground default
;;                     :background default
;;                     :scale 1.0
;;                     :html-foreground "Black"
;;                     :html-background "Transparent"
;;                     :html-scale 1.0
;;                     :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
;;                     )
;;       )
(setq org-latex-create-formula-image-program 'dvipng)


;;; [ Diary ]
(setq diary-file "~/Org/Diary/Diary.org")


;;; [ Miscellaneous ]


;;;; [ Hacking ]


;;; [ Mobile Org ]
;; TODO
;; (add-hook 'after-init-hook 'org-mobile-pull)
;; (add-hook 'kill-emacs-hook 'org-mobile-push)
;; ;; mobile sync
;; (defvar org-mobile-sync-timer nil)
;; (defvar org-mobile-sync-idle-secs (* 60 10))
;; (defun org-mobile-sync ()
;;   (interactive)
;;   (org-mobile-pull)
;;   (org-mobile-push))
;; (defun org-mobile-sync-enable ()
;;   "enable mobile org idle sync"
;;   (interactive)
;;   (setq org-mobile-sync-timer
;;         (run-with-idle-timer org-mobile-sync-idle-secs t
;;                              'org-mobile-sync)));
;; (defun org-mobile-sync-disable ()
;;   "disable mobile org idle sync"
;;   (interactive)
;;   (cancel-timer org-mobile-sync-timer))
;; (org-mobile-sync-enable)



;;; Sparse Tree
(setq org-highlight-sparse-tree-matches t)
(setq org-sparse-tree-open-archived-trees nil)
(set-face-attribute 'secondary-selection nil
                    :background "dark slate gray"
                    ;; :inverse-video t
                    )



;;; variables
;; TODO
;; (setq org-todo-state-tags-triggers
;;       '(state-change (TAG . FLAG)))
(setq org-tags-match-list-sublevels t)


;; (require 'org-gtd-summary)
;; (global-set-key "\C-cs" 'org-gtd-summary)


;; TODO append templates into this list variable.
;; (setq org-structure-template-alist)

;; TODO store default style sheet .css file in HTML header link.
;; (setq org-html-head)
;; (setq org-html-head-extra)


;;; org templates (skeleton)
;; <s, <e, <q, ...

;; avoid competing with org-mode templates.
(defun org-stop-auto-complete-for-structure-templates ()
  "Avoid competing with ORG-MODE templates like <s, <e, <q etc."
  (make-local-variable 'ac-stop-words)
  (loop for template in org-structure-template-alist do
        (add-to-list 'ac-stop-words (concat "<" (car template)))))

(add-hook 'org-mode-hook 'org-stop-auto-complete-for-structure-templates)


;; deft
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org" ; "txt"
   deft-directory "~/Org/deft/"
   deft-text-mode 'org-mode) ; 'markdown-mode
  ;; (global-set-key (kbd "<f3>") 'deft)
  )


;; finance: ledger



;; gnuplot
;; (require 'org-plot)


;;; HTML & CSS
;;; TODO
;; (setq org-html-head
;;       org-html-head-extra)


;; disable line-number-mode in Org.
(dolist (hook
         '(org-mode-hook
           org-agenda-mode-hook))
  (add-hook hook
            (lambda ()
              (line-number-mode -1)
              (linum-mode -1)
              )))


;;; iCalendar
(setq org-combined-agenda-icalendar-file "~/Org/Calendar/iCalendar.ics")



;;; notify
;; TODO
;;; 1.
;; - show in modeline
;; 2.
;; - sauron (+alert.el)
;; TODO
;;; 2.
;; - org-notify (from org-clock), (notify-send)
;;   - (org-notify "body")
;;   - (org-show-notification "body")
;; TODO reference org-clock.el function source code.
;;   - (setq org-show-notification-handler '())
;;; 3.
;; use function `my-func-notify-send'.
;; (my-func-notify-send "Warning" "the end is near" "/usr/share/icons/test.png" "/usr/share/sounds/beep.ogg")


;;; 2.
;; (require 'appt) ; appointment

;; (setq appt-audible t
;;       appt-display-diary t
;;       appt-display-interval 3 ; interval in minutes.
;;       appt-message-warning-time 15     ;; warn 15 mins in advance
;;       appt-display-mode-line t     ;; show in the modeline
;;       appt-display-format 'window  ;; use our func
;;       appt-display-duration 10 ; the number of seconds an appointment message is displayed.
;;       )

;; (appt-activate 1)                  ;; active appt (appointment notification)
;; (display-time)                     ;; time display is required for this...

;; update appt each time agenda opened
;; FIXME after fix the problem of repeated timestamp caused over size of 'appt-check'.
;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little façade-function for djcb-popup
;; TODO
;; (defun my-appt-display (min-to-app new-time msg)
;;   (my-func-notify-send (format "Appointment in %s minute(s)" min-to-app) msg
;;                        "~/Pictures/Icons/Hardcore.png"
;;                        ;; FIXME
;;                        "~/Music/Sounds/Hacking\ Game/voice-please-confirm.wav"))
;; ;; TODO remove this old format setting ?
;; (setq appt-disp-window-function 'my-appt-display) ; 'org-notify,

;; (setq appt-disp-window-function 'sr-org-handler-func) ; for Sauron.


;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             ;; (turn-on-font-lock)
;;             ;; turn off fill adapt, make org can write long length sentence.
;;             ;; (turn-off-filladapt-mode)
;;             ))


;;; [ Org-contacts ]

;; FIXME:


;;; [ Org-magit]

(require 'org-magit)


;;; [ Custom Functions ]

;;; Promote all items in subtree
;; This function will promote all items in a subtree. Since I use subtrees
;; primarily to organize projects, the function is somewhat unimaginatively
;; called my-org-un-project:
(defun stardiviner/org-prompt-all-items-in-subtree ()
  "Promote all items in subtree.

This function will promote all items in a subtree."
  (interactive)
  (org-map-entries 'org-do-promote "LEVEL>1" 'tree)
  (org-cycle t))

;;; Turn a heading into an Org link
(defun stardiviner/turn-word-into-org-mode-link ()
  "Replace word at point by an Org mode link."
  (interactive)
  (when (org-at-heading-p)
    (let ((hl-text (nth 4 (org-heading-components))))
      (unless (or (null hl-text)
                 (org-string-match-p "^[ \t]*:[^:]+:$" hl-text))
        (beginning-of-line)
        (search-forward hl-text (point-at-eol))
        (replace-string
         hl-text
         (format "[[file:%s.org][%s]]"
                 (org-link-escape hl-text)
                 (org-link-escape hl-text '((?\] . "%5D") (?\[ . "%5B"))))
         nil (- (point) (length hl-text)) (point))))))



;; (setq org-indirect-dedicated-frame nil)


;;; [ Pairs ]

(add-hook 'org-mode-hook
          (lambda ()
            (paredit-mode -1)
            (autopair-mode +1)))


;;; [ Key Bindings ]

(define-key my-org-prefix-map (kbd "a") 'org-agenda)
(define-key my-org-prefix-map (kbd "c") 'org-capture)

(define-key org-mode-map (kbd "C-c o t") 'org-timeline) ; Show a time-sorted view of the entries in the current org file.


(defun my-org-agenda-switch-or-build ()
  "Switch to *Org Agenda* buffer, if not available, then build it."
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (command-execute 'org-agenda-list)
    (bury-buffer)
    (switch-to-buffer "*Org Agenda*")
    ))

;;; start Org Agenda at Emacs startup, and put in end of buffer list:
;; (add-hook 'emacs-startup-hook 'my-eshell-start-or-switch)
(define-key my-org-prefix-map (kbd "o") 'my-org-agenda-switch-or-build)



;;; custom functions

(defun my-insert-keybinding-code ()
  "Insert keybinding code in Org with a keybinding quickly.

In common insert mode or in select region text to press this keybinding \\<C-c k> , \\[my-insert-keybinding-code]."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "=[" "]="))
      ;; (insert-pair nil "=[" "]=")
    (progn
      (insert "=[]= ")
      (backward-char 3)))
  )

(define-key org-mode-map (kbd "C-c k") 'my-insert-keybinding-code)

;; TODO:
;; (defun my-wrap-source-code-with-org-src ()
;;   "Wrap source code with Org-mode source code format."
;;   (interactive)
;;   (if (region-active-p)
;;       (let ((where (cons (region-beginning) (region-end))))
;;         (???))))
;;
;; (define-key org-mode-map (kbd "C-c k s") 'my-wrap-source-code-with-org-src)



(provide 'init-my-tool-org-mode)

;;; init-my-tool-org-mode.el ends here



