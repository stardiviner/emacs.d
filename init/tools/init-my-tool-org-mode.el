;;; init-my-tool-org-mode.el --- init Org-mode

;;; Commentary:

;;; [ minor modes ]
;; - orgtbl-mode :: implements the table editor.
;; - orgstruct-mode :: easy list editing.

;;; Code:

;;;_*Org-mode

;;;_* require & load modules
(require 'org)

(require 'org-faces)
(require 'org-fstree)
(require 'org-compat)
;; (require 'org-structure)
(require 'org-table)

(require 'org-timer)
(require 'org-clock)
(require 'org-habit)
(require 'org-notify)

(require 'org-pcomplete)

;;; org-protocol need server start.
(unless (server-running-p)
  (server-start))
(require 'org-protocol)

(require 'org-plot)

(require 'ox-latex)
;; (require 'ox-bibtex)
(require 'ox-beamer)
(require 'ox-odt)
(require 'ox-html)
;; (require 'ox-deck)
;; (require 'ox-publish)
;; (require 'ox-koma-letter)
;; (require 'org-notmuch)

;; use Org-mode as the default mode for all README files.
(add-to-list 'auto-mode-alist '("README$" . org-mode))

;;; [ Org Modules ]
;; Modules that should always be loaded together with org.el.
(setq org-modules '(org-pcomplete
                    org-faces org-fstree org-table org-compat
                    ;; org-protocol
                    org-timer org-clock org-habit org-notify
                    org-info org-bibtex org-docview
                    org-plot
                    org-bbdb
                    org-irc ; org-gnus org-mhe org-rmail
                    ;; org-w3m
                    ))

;;;_* View

;; (load "preview-latex.el" nil t) ; for option `org-startup-with-latex-preview'

;; startup & default view
(setq org-startup-folded t ; t, 'overview, 'content, 'showall.
      org-startup-indented t ; use org indent-mode for all org buffers.
      org-startup-truncated t
      ;; FIXME: error: Can't preview LaTeX fragment in a non-file buffer.
      org-startup-with-inline-images t ; `org-toggle-inline-images'
      ;; org-startup-with-latex-preview t
      ;; org-startup-options
      ;; coordinate grid overlays
      ;; org-table-overlay-coordinates t
      ;; org-table-coordinate-overlays t
      )

(setq org-cycle-separator-lines 2)

;; TODO write a if statement for this.
;; when org-indent-mode is on: sets org-hide-leading-stars to t and org-adapt-indentation to nil.
(setq org-hide-leading-stars t ; only show one star *
      ;; org-hide-leading-stars-before-indent-mode
      org-hide-emphasis-markers t ; hide markers like =inline code=.
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


;;;_* indentation

;; Usage:
;; * List Indent Editing
;;   - [M-S-RET]
;;     + [TAB] to indent deeper sub-list.
;;     + [S-TAB] to indent out list.

(require 'org-indent)

(setq org-indent-mode t
      org-adapt-indentation t   ; means adapt indentation to outline node level.
      org-indent-mode-turns-on-hiding-stars t
      org-indent-indentation-per-level 2
      ;; org-indent-max-levels 20
      org-indent-stars
      org-indent-strings
      ;; org-indent-boundary-char 32
      )

(org-indent-mode t)

(set-face-attribute 'org-indent nil
                    :foreground "deep pink"
                    )

;;;_* Faces

;; TODO
;; set font for all rest font, then override by other face settings.
;; (add-hook 'org-mode-hook '(lambda ()
;;                             (set-face-attribute 'default nil
;;                                                 :family "Gabriola"
;;                                                 ;; :height 120
;;                                                 )))


;; Date: Saturday   27 July 2013
(set-face-attribute 'org-date nil
                    :foreground "gray" :background "#333333"
                    :box '(:color "black" :line-width 2 :style nil)
                    :underline nil)
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
                    :foreground "light blue" ; :background " "
                    :slant 'italic
                    ;; :box '(:line-width -1 :style nil)
                    :underline t
                    )
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
;; time grid: 18:00 ...... ----------------
(set-face-attribute 'org-time-grid nil
                    :foreground "cyan")
;; alread past deadline in agenda
(set-face-attribute 'org-warning nil
                    :foreground "red"
                    :weight 'normal)
;; comming deadline in agenda
(set-face-attribute 'org-upcoming-deadline nil
                    :foreground "orange")
;; scheduled in agenda
(set-face-attribute 'org-scheduled-today nil ; scheduled today, & org-habit
                    :foreground "green")
(set-face-attribute 'org-scheduled nil
                    :foreground "forest green")
(set-face-attribute 'org-scheduled-previously nil
                    :foreground "orange red")

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
;; (set-face-attribute 'org-block nil
;;                     :background "#004A5d"
;;                     :foreground nil
;;                     )
;; ;; code face => ~code~,  #+RESULTS: : result.
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
                    :bold t :slant 'normal
                    :box '(:color "#444444" :line-width 1)
                    )
(set-face-attribute 'org-block-end-line nil
                    :foreground "cyan" :background "black"
                    :bold t :slant 'normal
                    :box '(:color "#444444" :line-width 1)
                    )
(set-face-attribute 'org-block nil
                    :foreground nil :background "black"
                    )
;; TODO:
;; (set-face-attribute 'org-block nil        ; selected line color in code block begin/end line.
;;                     :foreground "white" :background "#004A5D"
;;                     )
;; code face => ~code~,  #+RESULTS: : result.
(set-face-attribute 'org-code nil
                    :background "#222222" :foreground "orange"
                    ;; :box '(:color "cyan" :line-width 1 :style nil)
                    ;; :underline '(:color "cyan") :box nil
                    :family "DejaVu Sans Mono"
                    :bold nil :box nil)
;; table
(set-face-attribute 'org-table nil
                    :foreground "light sky blue")

;; inline code face => src_ruby{require 'something'}
;;
;; (REGEXP . FACE)
;;     Highlight REGEXP with FACE
;; (REGEXP N FACE)
;;     Highlight group N in REGEXP with FACE
;; (REGEXP (N1 FACE1) (N2 FACE2) (N3 FACE3) …)
;;     Highlight group Ni in REGEXP with FACEi
;;
;; src_lang{code...}[:header arguments] / NOTE: override by `org-verbatim'.
;; result in following =[result]=
(setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}"
      org-babel-inline-result-wrap "=> (~%s~)" ; or "=%s=", "~%s~"
      )

;; src_
(font-lock-add-keywords 'org-mode
                        '(("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
                           (1 '(:foreground "black" :weight 'normal :height 10)) ; src_ part
                           (2 '(:foreground "cyan" :weight 'bold :height 75 :underline "red")) ; "lang" part.
                           (3 '(:foreground "#555555" :height 70)) ; [:header arguments] part.
                           (4 'org-code) ; "code..." part.
                           )))

;; src without arguments
(font-lock-add-keywords 'org-mode
                        '(("\\(src_\\)\\([^[{]+\\){\\([^}]*\\)}"
                           (1 '(:foreground "black" :weight 'normal :height 10)) ; src_ part
                           (2 '(:foreground "cyan" :weight 'bold :height 80 :underline "red")) ; "lang" part.
                           (3 'org-code) ; "code..." part.
                           )))

;; TODO:
;; emabedded latex (inline formula)
;; (font-lock-add-keywords 'org-mode
;;                         '(("$\\([^$\ ]*\\)$" 1 'org-code) ; $a=2$
;;                           ))
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\(\ \\([^\ ]*\\)\ )\\" 1 'org-code) ; \( a=2 \)
;;                           ))
;; (font-lock-add-keywords 'org-mode
;;                         '(("\$\$\ \\([^\ ]*\\)\ \$\$" 1 'org-code) ; $$ a=2 $$
;;                           ))
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\\[\ \\([^\ ]*\\)\ \\\]" 1 'org-code) ; \[ a=2 \]
;;                           ))


;;; @<kbd>C-h h@</kbd> inline key codes highlight
(font-lock-add-keywords 'org-mode
                        '(("@<kbd>\\([^@]*\\)@</kbd>" 1 'org-code)))

(defun my/org-insert-key ()
  "Insert keybinding code in Org with a keybinding quickly.

In common insert mode or in select region text to press this keybinding \\<C-c k>.
to insert <kbd>..</kbd> (HTML) org =[..]= (Org-mode)."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "=[" "]="))
    ;; (insert-pair nil "=[" "]=")
    (progn
      (insert "=[]= ")
      (backward-char 3)))
  )

(defun my/org-insert-kbd ()
  "Insert literal HTML tag <kbd></kbd>."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "@@html:<kbd>" "</kbd>@@"))
    (progn
      (insert "@@html:<kbd></kbd>@@ ")
      (backward-char 9)))
  )


;; (defun my/org-insert-kbd (key)
;;   "Ask for a KEY then insert its description.
;; Will work on both `org-mode' and any mode that accepts plain html."
;;   (interactive "kType key sequence: ")
;;   (let* ((is-org-mode (derived-mode-p 'org-mode))
;;          (tag (if is-org-mode
;;                   "@@html:<kbd>@@%s@@html:</kbd>@@"
;;                 "<kbd>%s</kbd>")))
;;     (if (null (equal key "
;; "))
;;         (insert
;;          (format tag (help-key-description key nil)))
;;       (insert (format tag ""))
;;       (forward-char (if is-org-mode -15 -6)))))


(define-key org-mode-map (kbd "C-c K") 'my/org-insert-kbd)
(define-key org-mode-map (kbd "C-c k") 'my/org-insert-key)

;;; headline faces
;;; the ahead stars face when org indentation. (org-hide)
(set-face-attribute 'org-hide nil
                    :foreground "#002B36" :background "#002B36")
(set-face-attribute 'org-level-1 nil
                    :family "DejaVu Sans Mono"
                    :height 1.2 :weight 'bold
                    :foreground "#FF3870" :background "#222222"
                    ;; :box '(:color "black" :line-width 1 :style nil)
                    :overline t
                    )
(set-face-attribute 'org-level-2 nil
                    :foreground "#C8C800"
                    :weight 'bold
                    :height 1.1
                    ;; :box '(:color "black" :line-width 1 :style nil)
                    :overline t
                    )
(set-face-attribute 'org-level-3 nil
                    :foreground "#009E00"
                    :inherit 'org-level-2
                    )
(set-face-attribute 'org-level-4 nil
                    :foreground "cyan"
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
                    :box '(:color "dark green" :line-width 2)
                    :height 80)
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
;;; list definition terms
;; (set-face-attribute 'org-list-dt nil
;;                     :foreground "green yellow")
;;; priority faces
(setq org-priority-faces '(:foreground "cyan" :background nil
                                       :bold 'normal
                                       :box t))
;;; link face
(set-face-attribute 'org-link nil
                    :foreground "cyan"
                    :underline "dark cyan")
;; <<target link>>
(set-face-attribute 'org-target nil
                    :foreground "orange" :background "black"
                    :underline "red"
                    :weight 'bold)

;; org structure faces
(set-face-attribute 'org-agenda-structure nil
                    :foreground "gray"
                    :weight 'bold)

;; set Org clock face.
;; That is, make the org-mode-line-clock no longer inherit attributes from the
;; mode-line face. It seems like it gets the attributes from mode-line or
;; mode-line-inactive as appropriate, when displayed in the mode line.
(set-face-attribute 'org-mode-line-clock nil
                    :foreground "cyan"
                    :inherit nil)


;;;_* pretty entities

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


;;;_* org-bullets

;; (load-file "~/.emacs.d/my-init/extensions/org-bullets.el")

(require 'org-bullets nil t)

;;; You Can copy symbols from Desktop Utils "Character Maps".
;; ("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
;; ("⒈" "⒉" "⒊" "⒋" "⒌" "⒍" "⒎" "⒏" "⒐" "⒑" "⒒" "⒓" "⒔" "⒕" "⒖" "⒗" "⒘" "⒙" "⒚" "⒛")
;; ("⑴" "⑵" "⑶" "⑷" "⑸" "⑹" "⑺")

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


;;;_* Editing

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

;;;_* Complete

;; - in buffer + [M-TAB]
;; - in minibuffer

;;; more smart org completion option.
;; (if (featurep 'ido-vertical-mode)
;;     (setq org-completion-use-ido t)
;;   (setq org-completion-use-ido nil)
;;   (setq org-completion-use-iswitchb t)
;;   (setq org-completion-fallback-command 'hippie-expand))

;;;_*, org-pcomplete

;;;_*, org-ac

;;; [o] -- annotation.

;; (require 'org-ac)
;; ;; Make config suit for you. About the config item, eval the following sexp.
;; ;; (customize-group "org-ac")
;; (org-ac/config-default)
;;
;; ;; (setq org-ac/ac-trigger-command-keys '("\\" "*" "SPC" ":" "[" "+"))
;; (setq org-ac/ac-trigger-command-keys '("\\" "+" ":" "[" "*"))
;;
;; ;; remove heavy auto-complete sources to speed up typing in Org.
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (ac-source-remove '(ac-source-dictionary ac-source-words-in-same-mode-buffers))))

;;;_* Document Structure

;;;_*, plain list

(setq org-support-shift-select nil
      org-list-allow-alphabetical t) ; - nil to support: [S-up/down] to jump to previous/next item in list.

;;;_*, Drawer

(setq org-export-with-drawers t) ; t, nil, '(not "LOGBOOK")

;;;_*, Tables

;; (setq org-enable-table-editor t)

;;;_*, Images

;;; inline images [C-c C-x C-v] - `org-toggle-inline-images'.
;; [C-c C-x C-M-v] - `org-redisplay-inline-images'
;; -- this function will display new added inline images. and also will enable displaying inline images. but can't toggle off.
;; (setq org-startup-with-inline-images t)

(setq org-image-actual-width 350)       ; inline image scale width


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

;;;_* embedded latex

;; (org-toggle-latex-fragment)
;; (setq org-export-filter-latex-fragment-functions nil)

;; (add-hook 'org-mode-hook 'org-toggle-latex-fragment)

;;;_*, footnote

(setq org-footnote-auto-label 'confirm)

;;;_*, hyperlinks

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
        ;; EPUB
        ("\\.epub" . "ebook-viewer %s")
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

;; TODO: [[mu4e:exporting%20link%20abbreviations][exporting link abbreviations]]
;; `org-link-types'
;; `org-add-link-type' + `org-add-link-props'
;; (org-add-link-type)
;; (org-add-link-props)

;; TODO add more link abbrev into this variable.
(setq org-link-abbrev-alist ; Usage: In [C-c C-l] insert link completion.
      '(("RFC" . "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s")
        ;; search engines
        ("Google" . "http://www.google.com/search?q=%s")
        ("google" . "http://www.google.com/search?q=%s")
        ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
        ("blekko" . "https://blekko.com/#?q=%s")
        ("Bing" . "http://cn.bing.com/search?q=")
        ("Baidu" . "http://www.baidu.com/s?wd=%s")
        ;; Wiki
        ("Wikipedia" . "http://en.wikipedia.org/w/index.php?search=%s")
        ("Wikia" . "http://www.wikia.com/index.php?search=%s")
        ("Baidu Wiki 百度百科" . "http://baike.baidu.com/search/none?word=%s")
        ;; Q & A
        ("Quora" . "https://www.quora.com/search?q=%s")
        ("ZhiHu 知乎" . "http://www.zhihu.com/search?q=%s&type=question")
        ("Baidu Know 知道" . "http://zhidao.baidu.com/search?word=%s")
        ("Baidu Experience 百度经验" . "http://jingyan.baidu.com/search?word=%s")
        ;; Maps
        ("Google Maps" . "http://maps.google.com/maps?q=%s")
        ;; Programming
        ("Stack Exchange - Stack Overflow" . "http://stackoverflow.com/search?q=%s")
        ("Stack Exchange - Programmers" . "http://programmers.stackexchange.com/search?q=%s")
        ;; Emacs
        ("Emacs Wiki" . "www.emacswiki.org/emacs?search=%s")
        ("Stack Exchange - Emacs" . "http://emacs.stackexchange.com/search?q=%s")
        ;; Document Search
        ;; API Search
        ("{API}Search apis.io" . "http://apis.io/?search=%s")
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
        ("Bugzilla" . "http://bugzilla/show_bug.cgi?id=%s")
        ;; Book
        ("DouBan Books 豆瓣读书" . "http://book.douban.com/subject_search?search_text=%s")
        ;; Movie
        ("DouBan Movies 豆瓣电影" . "http://movie.douban.com/subject_search?search_text=%s")
        ;; The Pirate Bay
        ("The Pirate Bay (海盗湾)" . "http://thepiratebay.se/search/%s")
        ;; Shopping
        ("TaoBao 淘宝" . "http://s.taobao.com/search?q=%s")
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

;;; [[grep:regexp][regexp (grep)]]
(defun follow-grep-link (regexp)
  "Run `rgrep' with REGEXP as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))

(org-add-link-type "grep" 'follow-grep-link)

;;; [[tag:]]
;; e.g. [[tag:work+phonenumber-boss][Optional Description]]
(defun follow-tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(org-add-link-type "tag" 'follow-tag-link)


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


;;; Custom Searches
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq org-create-file-search-functions)
;;             (setq org-execute-file-search-functions)))

;;;_* GTD

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

;;;_* org-linkany

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

;;;_* org-annotate-file

;; This is yet another implementation to allow the annotation of a
;; file without modification of the file itself. The annotation is in
;; org syntax so you can use all of the org features you are used to.

;; (require 'org-annotate-file)

;; (global-set-key (kbd "C-c C-l") 'org-annotate-file)

;; ;; To change the location of the annotation file:
;; (setq org-annotate-file-storage-file "~/.emacs.d/.org-annotated.org")

;; ;; Then when you visit any file and hit C-c C-l you will find yourself
;; ;; in an org buffer on a headline which links to the file you were
;; ;; visiting, e.g:
;; ;; * ~/org-annotate-file.el

;; ;; Under here you can put anything you like, save the file
;; ;; and next time you hit C-c C-l you will hit those notes again.
;; ;;
;; ;; To put a subheading with a text search for the current line set
;; ;; `org-annotate-file-add-search` to non-nil value. Then when you hit
;; ;; C-c C-l (on the above line for example) you will get:
;; (setq org-annotate-file-add-search t)

;; * ~/org-annotate-file.el
;; ** `org-annotate-file-add-search` to non-nil value. Then whe...

;; Note that both of the above will be links.


;;;_* properties and columns

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


;;;_* Dates and Times


;;;_* Clock

(require 'org-clock)

;; to save the clock history across Emacs sessions.
(setq org-clock-persist t
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
      ;; org-clock-task-overrun
      org-clock-continuously nil ; don't continue on last clock out.
      ;; org-clock-persist-file
      ;; org-clock-leftover-time
      org-clock-out-when-done t
      org-clock-mode-line-total 'auto
      org-clock-mode-line-entry t
      ;; org-clock-task-overrun-text
      org-clock-persist-query-save t
      org-clock-persist-query-resume t
      org-clock-clocked-in-display 'frame-title ; 'mode-line, 'frame-title, 'both, nil.
      ;; TODO: add clock in display into my custom mode-line.
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


;;;_* org-timer

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

;;;_*, effort estiname

(setq org-effort-property "Effort"
      ;; conversion factor to minutes for an effort modifier.
      ;; (MODIFIER . MINUTES)
      org-effort-durations '(("hours" . 60)
                             ("days" . 480)
                             ("weeks" . 2400)
                             ("months" . 9600)
                             ("years" . 96000))
      org-time-clocksum-use-effort-durations nil ; don't use upper `org-effor-durations' as time count unit.
      ;; org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
      ;; org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>")
      ;; org-time-clocksum-format '(:days "%dd " :hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      ;; org-time-clocksum-use-fractional nil
      ;; org-time-clocksum-fractional-format "%.2f"
      )

;;;_* org habit

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
      org-habit-preceding-days 14
      org-habit-following-days 7
      org-habit-today-glyph ?>
      org-habit-completed-glyph ?-
      )

;;;_*, org-habit faces

(set-face-attribute 'org-habit-clear-future-face nil ; for future days on which a task shouldn't be done yet.
                    :background " ")
(set-face-attribute 'org-habit-alert-future-face nil ; for days on which a task is due.
                    :background "orange")
;; for days on which a task should start to be done.
(set-face-attribute 'org-habit-ready-face nil ; 
                    :background "yellow")
;; for days on which a task is overdue.
(set-face-attribute 'org-habit-overdue-face nil ; the days have passed from start day.
                    :background "#5C5702")
;; -----------------------------------------------------------------------------------------
(set-face-attribute 'org-habit-clear-face nil ; for days on which a task shouldn't be done yet.
                    :background "white")
(set-face-attribute 'org-habit-ready-future-face nil ; for days on which a task should start to be done.
                    :background "green")
(set-face-attribute 'org-habit-alert-face nil ; for days on which a task is due.
                    :background "dark red")
(set-face-attribute 'org-habit-overdue-future-face nil ; for days on which a task is overdue.
                    :background "dark cyan")

;;;_*, org-habit keybindings

;; create an key binding for all necessary steps for create a habit. (reference in Org-mode.org file)
(defun org-habit-apply ()
  "Apply org-habit on this task."
  (interactive)
  (beginning-of-line)
  (org-todo "HABIT")
  ;; The format-time-string code is correct.
  ;; (format-time-string "%Y-%m-%d %H:%M .+1d" (current-time))
  ;; (org-schedule nil (format-time-string "%Y-%m-%d %H:%M" (current-time))) ; deactive
  (org-schedule nil) ; interactive
  (save-excursion
    (next-line) (beginning-of-line)
    (when (looking-at "SCHEDULED: [^>]*\\(>\\)")
      (goto-char (match-beginning 1))
      (insert (concat
               " .+"
               (read-string "Minimum interval: ")
               "d"
               "/"
               (read-string "Maximum interval: ")
               "d"))))
  
  ;; old way
  ;; (save-excursion
  ;;   (next-line) (beginning-of-line)
  ;;   (when (looking-at "SCHEDULED: [^>]*\\(>\\)")
  ;;     (goto-char (match-beginning 1))
  ;;     (insert " .+1d")))
  
  (org-set-property "STYLE" "habit")
  ;; (org-set-property "LOGGING" "TODO DONE(!)")
  )

(define-key org-mode-map (kbd "C-c C-x h") 'org-habit-apply)

(defun org-time-interval (&optional arg)
  "Set schedule and deadline time interval for headline.

Accepts universal argument \\<C-c C-x r> & \\[org-time-interval]."
  (interactive "P")
  ;; C-u is '(4) and C-u C-u is '(16)
  ;; (equal arg '(4))
  ;; So I need to use `(interactive "p")' for `(org-deadline)'.
  (org-schedule arg)
  ;; (org-deadline arg "+3d") ; this is not interactive for deadline.
  (org-deadline arg))

(define-key org-mode-map (kbd "C-c C-x r") 'org-time-interval)

;;;_* Capture - Refile - Archive

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
         "\n* TODO %^{prompt}\n\n%i\n\n%a\n\n%?"
         :prepend t
         :empty-lines 1
         )

        ;; Tasks
        ("t" "Add a task into Tasks"
         entry (file+headline "~/Org/Tasks.org" "Tasks")
         "\n* TODO %^{prompt} [/]\n\n%?\n\n"
         :empty-lines 1
         )
        ("u" "Add an entry into Buy list"
         entry (file+headline "~/Org/Tasks.org" "Buy")
         "\n* TODO %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1
         )
        ;; TODO: add an capture template which use :clock property
        ;; (:clock-in t :clock-resume t)

        ;; Blog
        ("b" "Blog (jekyll)"
         entry (file+datetree+prompt "~/Org/Diary/Public/Blog.org")
         "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?"
         :empty-lines 1
         )
        ;; ("j" "Journal"
        ;;  entry (file+datetree "~/Org/Journal.org" "Journal")
        ;;  "\n* %^{prompt}\nEntered on %U\n %i\n %?\n\n"
        ;; :empty-lines 1
        ;; )
        
        ;; TODO Contacts
        
        ;; bookmark
        ("m" "Add an URL to bookmarks database"
         entry (file+headline "~/Org/Wiki/Data/Bookmarks/Bookmarks.org" "Capture")
         "\n* %^{prompt}\n\n%A\n\n%?\n\n"
         :empty-lines 1)
        
        ;; org-passwords
        ;; FIXME:
        ("p" "password"
         entry (file+headline "~/Git/dotfiles/passwords.gpg" "Accounts")
         "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{EMAIL}p"
         :empty-lines 1)

        ;; Issues, Bugs, Features
        ("b" "record a Bug to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer" "Bugs")
         "\n* BUG %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("i" "record an Issue to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer" "Issues")
         "\n* ISSUE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        ("f" "record a Feature to list"
         entry (file+olp "~/Org/Projects/Projects.org" "Computer" "Features")
         "\n* FEATURE %^{prompt}\n\n%i\n\n%?\n\n"
         :empty-lines 1)
        
        ;; knowledge & Wiki
        ;; thought
        ;; "~/Org/Wiki/Wiki/Thought/Thought.org" "My Thought"
        ))



;; TODO:
;; (setq org-capture-templates-contexts)

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

;;;_* Refile

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

;;;_* Archive

;; - [C-c C-x C-a] -- org-archive-subtree-default. (archive current entry)
;; - [C-c C-x C-s] -- org-archive-subtree. (archive subtree).

(setq org-archive-location "%s_archive::"
      org-archive-save-context-info '(time file olpath category todo itags ltags)
      org-archive-mark-done nil
      org-archive-stamp-time t
      org-archive-reversed-order nil
      )

;;;_* Attachment

;; - [C-c C-a] :: `org-attach'.

;;;_* Agenda Views

(require 'org-agenda)

(setq org-agenda-include-deadline t)
(setq org-agenda-log-mode-items '(closed clock)
      org-agenda-log-mode-add-notes t
      org-agenda-start-with-log-mode t)

(setq org-agenda-include-diary t) ; include in the agenda entries from the Emacs Calendar's diary. `diary-file'.
;; File to which to add new entries with the `i' key in agenda and calendar.
;; When this is the symbol `diary-file', the functionality in the Emacs
;; calendar will be used to add entries to the `diary-file'.  But when this
;; points to a file, `org-agenda-diary-entry' will be used instead.
(setq org-agenda-diary-file 'diary-file)
(setq org-agenda-insert-diary-strategy 'date-tree)
;; (setq diary-date-forms '((month "/" day "[^/0-9]")
;;                          (month "/" day "/" year "[^0-9]")
;;                          (monthname " *" day "[^,0-9]")
;;                          (monthname " *" day ", *" year "[^0-9]")
;;                          (dayname "\\W")))
;; (setq org-support-shift-select nil)
;; (setq org-agenda-entry-types)
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
                         "~/Org/Capture/" ; FIXME: "~/Org/Capture/*.org" add with (file-expand-wildcards "~/Org/Capture/*.org")
                         "~/Org/Wiki/Learning/Learning.org"
                         "~/Org/Wiki/Learning/MyLearningPlan/Learn Programming.org"
                         "~/Org/Wiki/Wiki.org"
                         "~/Org/Wiki/Kung Fu/Kung Fu.org"
                         ))

;;; FIXME: use some function like `find-lisp-find-files'
;; use `add-to-list' to remove duplicates.


;; 2. use `file-expand-wildcards'
;; (setq org-agenda-files (file-expand-wildcards "~/Org/*.org")) ; Including all org files from a directory into the agenda
;; 3. use `find-lisp-find-files'
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
;; (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
;; Note that these files will only be searched for text search commands, In
;; fact, if the first element in the list is the symbol `agenda-archives', then
;; all archive files of all agenda files will be added to the search scope.
;; [C-c a s]
;; TODO: test this setting.
;; (setq org-agenda-text-search-extra-files '(agenda-archives "~/Org/Journal.org" "~/Org/Diary/"))
(setq org-agenda-text-search-extra-files '("~/Org/Journal.org" "~/Org/Diary/"))
(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-delay-if-deadline 'post-deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t
      ;; NOTE: org-agenda-ignore-properties '(effort appt stats category)
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-ignore-timestamp nil ; 'all
      org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-scheduled 'future
      )
(setq org-agenda-scheduled-leaders '("Scheduled: " "%2d days /// "))
(setq org-agenda-show-all-dates t)
(setq org-agenda-show-outline-path t)
(setq org-deadline-warning-days 7) ; determines how far in advance items with
                                        ; deadlines will show up in the agenda.
(setq org-extend-today-until 3) ; I work late at night! Extend my current day past midnight.
;; agenda view
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (search . " %i %-12:c")
        (tags . " %i %-12:c")))
(setq org-agenda-remove-times-when-in-prefix t
      ;; org-agenda-remove-tags t
      org-agenda-remove-tags-when-in-prefix t
      ;; org-agenda-remove-timeranges-from-blocks
      )

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

;; TODO:
;; (setq org-agenda-category-icon-alist)

;;;_* about TODO tasks

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
        (sequence "TODO(t@/!)" "Urgent(u!)" "SOMEDAY(s!)" "FAILED(X@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "|" "DONE(d@/!)")
        ;; Types
        ;; use (@/!) to record/log info reference source link URL and timestamp.
        (type "Org(o@/!)" "code(c@/!)" "project(p@/!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(x@/!)" "|" "DONE(d@/!)")
        ;; Work
        (type "Work(w@/!)" "Meeting(m@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "Learn(l!)" "Learning(n!)" "Review(r!)" "|" "DONE(d@/!)")
        ;; org-trello
        (type "TODO" "Doing" "|" "DONE")
        ))
(setq org-todo-keyword-faces
      (quote (;;; todos
              ("TODO" :foreground "white" :weight bold)
              ("Urgent" :foreground "red" :background "black"
               :weight bold :overline "red"
               ;; :box '(:color "red" :line-width 2 :style nil)
               )
              ("STARTED" :foreground "green" :weight bold :overline "yellow")
              ("HABIT" :foreground "cyan" :background "black" :weight bold
               :box '(:color "cyan" :line-width 1 :style nil))
              ("SOMEDAY" :foreground "dim gray" :weight bold)
              ("Doing" :foreground "cyan" :weight bold)
              ("DONE" :foreground "black" :background " " :weight bold :strike-through t)
              ("FAILED" :foreground "#444444" :background "orange" :weight bold :underline "dark red")
              ("CANCELLED" :foreground "#444444" :background "orange" :weight bold :strike-through t)
              ;;; fixme
              ("BUG" :foreground "red" :background " " :weight bold
               :box '(:color "red" :line-width 1 :style nil))
              ("ISSUE" :foreground "red" :background " " :weight bold
               :box '(:color "dark red" :line-width 1 :style nil))
              ("ERROR" :foreground "red" :weight bold
               :box '(:color "red" :line-width 1 :style nil))
              ("FIXME" :foreground "black" :background "red" :weight bold
               :box '(:color "dark red" :line-width 1 :style nil))
              ("FEATURE" :foreground "cyan" :weight bold
               :box '(:color "cyan" :line-width 1 :style nil))
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
              ;; learn
              ("Learn" :foreground "dark orange" :background " "
               :overline "orange")
              ("Learning" :foreground "green yellow" :background " "
               :overline "lawn green")
              ("Review" :foreground "yellow" :background " "
               :underline "green yellow")
              )))

;;;_*, custom functions

;;; bind key [C-l] to locate to current time now ----- in Org-Agenda buffer.
(defun my-org-agenda-jump-to-current-time ()
  "Jump to current time now."
  (interactive)
  (goto-char (text-property-any (point-min) (point-max) 'face 'org-agenda-current-time))
  )

(define-key org-agenda-mode-map (kbd "C-l") 'my-org-agenda-jump-to-current-time)


;;;_* Tags

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


;;;_* Markup


;;;_* Exporting

;; TODO how to set Org export directory.
;; (setq org-export-html-link-home "~/Org/")
;; (setq org-export-publishing-directory "~/Publish/Org")

;; The last level which is still exported as a headline. Inferior levels will produce itemize lists when exported.
(setq org-export-backends '(ascii html icalendar latex md)
      org-export-babel-evaluate t ; code evaluation during export.
      ;; org-export-headline-levels 6
      org-export-with-smart-quotes t
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

;;; PDF exporting
;;
;; (setq org-latex-to-pdf-process 
;;       '("pdflatex %f" "biber %b" "pdflatex %f" "pdflatex %f"))

;;; ------------- Export UTF-8 checkboxes ---------------------
;;; This snippet turns - [X] into ☑ and - [ ] into ☐.
(defun sacha/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<code>[-]</code>")
        (t "")))
(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))

;;;_* htmlize

;; (setq org-html-htmlize-font-prefix "org-"
;;       org-html-htmlize-output-type 'inline-css)

;;;_* Publishing

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


;;;_* org-html5

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      ;; org-html-doctype-alist
      )

;;;_* Babel

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
   (matlab . t)                         ; MATLAB
   (octave . t)                         ; Octave
   (gnuplot . t)                        ; gnuplot
   (ditaa . t)                          ; ditaa
   (dot . t)                            ; Graphviz, Dot
   (plantuml . t)                       ; PlantUML
   (ledger . t)                         ; ledger support in Babel
   ;; (sml . t)                            ; from extension ob-sml
   (sass . t)                           ; Sass
   ;;; extras
   ;; (mongo . t)                          ; MongoDB
   ))

;;; ob-julia (require ESS)
;;
;; TODO: read https://github.com/gjkerns/ob-julia/blob/master/ob-julia-doc.org
;;
(setq inferior-julia-program-name "julia")
;; 1.
;; (load "~/.emacs.d/el-get/ob-julia/ob-julia.el")
;; 2.
(add-to-list 'load-path "~/.emacs.d/el-get/ob-julia/ob-julia.el")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((julia . t)))

;; (setq org-babel-julia-eoe-indicator "print(\"org_babel_julia_eoe\")")
(setq org-babel-default-header-args:julia
      '((:results . "replace output")
        (:padnewline . "yes")))

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
;;
;; `org-babel-default-header-args:<lang>' where `<lang>' is the name of the
;; language.  See the language-specific documentation available online at
;; `http://orgmode.org/worg/org-contrib/babel'.
;; TODO:
(setq org-babel-default-header-args:latex
      '(;; generate results as #+BEGIN_LaTeX ... #+END_LaTeX block.
        ;; (:results . "latex")
        ;; (:exports . "results")
        ;; generate result as a (bitmap) image or pdf.
        ;; (:file . "temp.png")
        ))

(setq org-babel-default-header-args:R
      '((:session . "no")
        (:exports . "both")
        (:results . "replace")
        ))

(setq org-babel-default-header-args:sqlite
      '((:db . "temp.db")
        (:results . "raw")
        ;; (:echo . t)
        (:column . t)
        (:nullvalue . "Null")))

;;; how to correctly enable flycheck in babel source blocks
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name)))
    ad-do-it
    (setq buffer-file-name file-name)))


;;;_* working with source code

;;; - [C-c C-v] --- prefix map.

;;; source code (src)
(setq
 ;; fontify code in code blocks. (highlight code in exported HTML)
 ;; FIXME (slow-down/suspend on Emacs startup)
 org-src-fontify-natively t
 ;; preserve leading whitespace characters on export, for tangling languages such as Python.
 org-src-preserve-indentation nil
 ;; default org-mode src indentation when `org-src-preserve-indentation' is non-nil.
 org-edit-src-content-indentation 0
 ;; the effect of TAB in a code block is as if it were issued in the language major mode buffer.
 org-src-tab-acts-natively t
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

;;;_* Org LaTeX

(setq org-babel-latex-htlatex t)

;;; preview LaTeX fragments [C-c C-x C-l] + [C-c C-c]
;;
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


;;;_* Org Diary

(setq diary-file "~/Org/Diary/Diary.org")


;;;_* Miscellaneous


;;;_* Hacking

;;;_* Mobile Org

;; TODO:
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


;;;_* Sparse Tree

(setq org-highlight-sparse-tree-matches t)
(setq org-sparse-tree-open-archived-trees nil)
(set-face-attribute 'secondary-selection nil
                    :background "dark slate gray"
                    ;; :inverse-video t
                    )



;;;_* variables

;; TODO:
;; (setq org-todo-state-tags-triggers
;;       '(state-change (TAG . FLAG)))
(setq org-tags-match-list-sublevels t)


;; (require 'org-gtd-summary)
;; (global-set-key "\C-cs" 'org-gtd-summary)


;;; append templates into this list variable.
;;
;; default structure easy template alist
;;
;; (setq org-structure-template-alist
;;       '(("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
;;         ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
;;         ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
;;         ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
;;         ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
;;         ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
;;         ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX")
;;         ("L" "#+LaTeX: ")
;;         ("h" "#+BEGIN_HTML\n?\n#+END_HTML")
;;         ("H" "#+HTML: ")
;;         ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII")
;;         ("A" "#+ASCII: ")
;;         ("i" "#+INDEX: ?")
;;         ("I" "#+INCLUDE: %file ?")))



;;;_ + ditaa & PlantUML & Graphviz

;; Org-babel makes it easy to generate decent graphics using external packages
;; like ditaa, graphviz, PlantUML, and others.
;;
;; The setup is really easy. ditaa is provided with the org-mode source. You'll
;; have to install the `graphviz' and `PlantUML' packages on your system.

;; ditaa & PlantUML setup
(setq org-ditaa-jar-path "~/.emacs.d/init/extra/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/init/extra/plantuml.jar")

;; (add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(add-hook 'org-babel-after-execute-hook 'my/redisplay-inline-images 'append)
(defun my/redisplay-inline-images ()
  (condition-case nil
      (org-redisplay-inline-images)
    (error nil)))

;; disable this: because already has Babel `ditaa'. in `org-babel-load-languages'.
;; (add-to-list 'org-structure-template-alist
;;              '("d" "#+BEGIN_DITAA output-?.png --overwrite --round-corners\n\n#+END_DITAA"))

;;; PlantUML language reference
;; [[file:~/.emacs.d/init/extra/PlantUML%20Language%20Reference%20Guide.pdf][PlantUML Language Reference Guide]]

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;; Graphviz

;;; Example
;; #+BEGIN_SRC dot :file some_filename.png :cmdline -Kdot -Tpng
;;   <context of graphviz source goes here>
;; #+END_SRC

;;;_* LaTeX formula block generate output graph, then insert to current buffer just like upper ditaa.

;; - use Babel <s latex.

;; - re-display inline image after execute babel.
;;   -- already have upper hook: `my/redisplay-inline-images' in `org-babel-after-execute-hook'.

;; TODO:
;; (setq org-babel-header-args:latex
;;       '((:exports . "")
;;         ))

;; TODO: Add an babel headers arguments example here:

;;;_* Easy Template

;; TODO store default style sheet .css file in HTML header link.
;; (setq org-html-head)
;; (setq org-html-head-extra)

;;;_* org skeleton/template

;; <s, <e, <q, ...

;; avoid competing with org-mode templates.
;; (defun org-stop-auto-complete-for-structure-templates ()
;;   "Avoid competing with ORG-MODE templates like <s, <e, <q etc."
;;   (make-local-variable 'ac-stop-words)
;;   (loop for template in org-structure-template-alist do
;;         (add-to-list 'ac-stop-words (concat "<" (car template)))))
;;
;; (add-hook 'org-mode-hook 'org-stop-auto-complete-for-structure-templates)

;;;_* deft

(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org" ; "txt"
   deft-directory "~/Org/deft/"
   deft-text-mode 'org-mode) ; 'markdown-mode
  ;; (global-set-key (kbd "<f3>") 'deft)
  )

;;;_* finance: ledger



;;;_* plot: gnuplot

;; (require 'org-plot)


;;;_* HTML & CSS

;;; TODO:
;; (setq org-html-head
;;       org-html-head-extra)



;;;_* disable line-number-mode in Org.
(dolist (hook
         '(org-mode-hook
           org-agenda-mode-hook))
  (add-hook hook
            (lambda ()
              (line-number-mode -1)
              (linum-mode -1)
              )))

;;;_* iCalendar

(setq org-combined-agenda-icalendar-file "~/Org/Calendar/iCalendar.ics")


;;;_* Notify

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


;;;_*, org-notify

(setq org-notify-audible t
      ;; org-notify-parse-file
      ;; org-notify-window-buffer-name
      ;; org-notify-actions '("show" "show" "done" "done" "hour" "one hour later" "day" "one day later" "week" "one week later")
      )

;;; ---------------------------------------------------------
;; List of possible parameters:
;;
;;   :time      Time distance to deadline, when this type of notification shall
;;              start.  It's a string: an integral value (positive or negative)
;;              followed by a unit (s, m, h, d, w, M).
;;   :actions   A function or a list of functions to be called to notify the
;;              user.  Instead of a function name, you can also supply a suffix
;;              of one of the various predefined `org-notify-action-xxx'
;;              functions.
;;   :period    Optional: can be used to repeat the actions periodically.
;;              Same format as :time.
;;   :duration  Some actions use this parameter to specify the duration of the
;;              notification.  It's an integral number in seconds.
;;   :audible   Overwrite the value of `org-notify-audible' for this action.
;;
;;   :actions -ding, -notify, -window, -notify/window, -message, -email,

(org-notify-add 'default
                '(:time "1h" :period "2h" :duration 80
                        :actions (-notify/window -ding))
                ;; '(:time "1h" :period "50m" :duration 100
                ;;         :actions (-notify/window -ding))
                ;; '(:time "3d" :period "4h" :duration 40
                ;;         :actions -notify/window)
                ;; '(:time "5d" :period "6d" :duration 20
                ;;         :actions -notify)
                )

;; (org-notify-add 'appt
;;                 '(:time "-1s" :period "20s" :duration 10
;;                   :actions (-message -ding))
;;                 '(:time "15m" :period "2m" :duration 100
;;                   :actions -notify)
;;                 '(:time "2h" :period "5m" :actions -message)
;;                 ;; '(:time "3d" :actions -email)
;;                 )
;;
;; This means for todo-items with `notify' property set to `appt': 3 days
;; before deadline, send a reminder-email, 2 hours before deadline, start to
;; send messages every 5 minutes, then 15 minutes before deadline, start to
;; pop up notification windows every 2 minutes.  The timeout of the window is
;; set to 100 seconds.  Finally, when deadline is overdue, send messages and
;; make noise."



(org-notify-start 60)

;;;_*, appt
;;
;; (require 'appt) ; appointment
;;
;; (setq appt-audible t
;;       appt-display-diary t
;;       appt-display-interval 3 ; interval in minutes.
;;       appt-message-warning-time 15     ;; warn 15 mins in advance
;;       appt-display-mode-line t     ;; show in the modeline
;;       appt-display-format 'window  ;; use our func
;;       appt-display-duration 10 ; the number of seconds an appointment message is displayed.
;;       )
;;
;; (appt-activate 1)                  ;; active appt (appointment notification)
;; (display-time)                     ;; time display is required for this...
;;
;; update appt each time agenda opened
;; FIXME after fix the problem of repeated timestamp caused over size of 'appt-check'.
;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
;;
;; our little façade-function for djcb-popup
;; TODO
;; (defun my-appt-display (min-to-app new-time msg)
;;   (my-func-notify-send (format "Appointment in %s minute(s)" min-to-app) msg
;;                        "~/Pictures/Icons/Hardcore.png"
;;                        ;; FIXME
;;                        "~/Music/Sounds/Hacking\ Game/voice-please-confirm.wav"))
;; ;; TODO remove this old format setting ?
;; (setq appt-disp-window-function 'my-appt-display) ; 'org-notify,
;;
;; (setq appt-disp-window-function 'sr-org-handler-func) ; for Sauron.
;;
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             ;; (turn-on-font-lock)
;;             ;; turn off fill adapt, make org can write long length sentence.
;;             ;; (turn-off-filladapt-mode)
;;             ))
;;

;;;_* org-bbdb

;;; Usage:
;; - [C-c C-l] + `bbdb:'

;; - (org-bbdb-anniversaries)
;;   put `%%(org-bbdb-anniversaries)' in one of my agenda files. and set headline with property (:CATEGORY: Anniv)
;;   - [C-c C-x p] to set property
;;   - select CATEGORY property, value is "`Anniv'".
;;   - put this line into agenda file below the headline. %%(org-bbdb-anniversaries).

;; TODO:
;; (setq org-bbdb-anniversary-field 'anniversary)
;; (setq org-bbdb-default-anniversary-format "birthday")
;; (setq org-bbdb-anniversary-format-alist
;;       '(("birthday" lambda
;;          (name years suffix)
;;          (concat "Birthday: [[bbdb:" name "][" name " ("
;;                  (format "%s" years)
;;                  suffix ")]]"))
;;         ("wedding" lambda
;;          (name years suffix)
;;          (concat "[[bbdb:" name "][" name "'s "
;;                  (format "%s" years)
;;                  suffix " wedding anniversary]]"))))


;;;_* org-contacts

;; FIXME: (require 'org-contacts)

;; FIXME:

;;;_* org-screenshot

;;; screenshots integrated with emacs org mode attachments.

;;; Usage:
;;
;; - While in an org mode buffer, use the org-screenshot command to take a
;;   screenshot and have it inserted at the current point.

(require 'org-screenshot)

(setq org-screenshot-command-line "scrot -d 5 -s %f" ; "import %f",
      org-screenshot-relative-links t
      org-screenshot-image-directory "./images/"
      org-screenshot-file-name-format "screenshot-%2.2d.png"
      )

(define-key my-org-prefix-map (kbd "s") 'org-screenshot-take)

;; TODO: Emacs need to prompt user to get the screenshot filename to describe this screenshot.
;; 1. improve org-screenshot source code.
;; 2. add custom function. or defadvice.

;;;_* org-pomodoro

;;; This adds very basic support for Pomodoro technique in Emacs' org-mode.

;;; Usage:
;;
;; - Move point to a task as you would do with `org-clock-in'. Call `org-pomodoro' the task will be clocked-in.
;; - When there's time for break, the task will be org-clock-out'ed.
;; - If you call `org-pomodoro' during a pomodoro, you'll be asked to reset a pomodoro.
;; - If you call `org-pomodoro' outside org-mode, you'll be presented with list of recent tasks, as C-u org-clock-in would.


(require 'alert)
(require 'org-pomodoro)

(define-key my-org-prefix-map (kbd "g") 'org-pomodoro)

;;;_* org-doing

;;; Inspired by doing, a set of functions for keeping track of what you're doing right now.

;;; Usage:
;;
;; - [M-x org-doing-log] :: Log what you're doing now.
;; - [C-u M-x org-doing-log] :: Log what you're doing later.
;; - [M-x org-doing-done] :: Log something you've already done.
;; - [M-x org-doing-done (don't enter anything and press Enter)] :: Mark your most recent TODO as DONE.
;; - Org-Doing Omni Function
;;   - The omni function lets you enter short-hands at the beginning, making it very quick to use org-doing:
;;     [M-x org-doing]
;;     (now reviewing email, getting ready for a nap. seriously)

;;;_* org-magit

;; (require 'org-magit)

;;;_* org-protocol

;; (setq org-protocol-project-alist ; (module-name :property value property: value ...)
;;       '(("http://orgmode.org/worg/"
;;           :online-suffix ".php"
;;           :working-suffix ".org"
;;           :base-url "http://orgmode.org/worg/"
;;           :working-directory "/home/stardiviner/Org/Worg/")
;;          ("http://localhost/org-notes/"
;;           :online-suffix ".html"
;;           :working-suffix ".org"
;;           :base-url "http://localhost/org/"
;;           :working-directory "/home/user/org/"
;;           :rewrites (("org/?$" . "index.php")))))


;;;_* Custom Functions

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

;;;_* Org frame settings

;; (setq org-indirect-dedicated-frame nil)


;;;_* Pairs

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (paredit-mode -1)
;;             (autopair-mode -1)))


;;;_* custom keybindings

(define-key my-org-prefix-map (kbd "a") 'org-agenda)
;; [C-u C-c o t] -- prompt for a keyword for todo list.
(define-key my-org-prefix-map (kbd "t") 'org-todo-list)
(if (featurep 'helm)
    (define-key my-org-prefix-map (kbd "c") 'helm-org-capture-templates)
  (define-key my-org-prefix-map (kbd "c") 'org-capture))
(define-key my-org-prefix-map (kbd "l") 'org-insert-link-global)
(define-key my-org-prefix-map (kbd "u") 'org-open-at-point-global)

(define-key org-mode-map (kbd "C-c o T") 'org-timeline) ; Show a time-sorted view of the entries in the current org file.

(define-key my-org-prefix-map (kbd "o")
  (defun my-open-org-agenda ()
    (interactive)
    ;; TODO:
    ;; (if (memq (current-buffer) (frame-bufs-buffer-list (selected-frame)))
    ;;     (switch to that window)
    ;;   (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t))
    (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t)
    ))

(define-key my-org-prefix-map (kbd "e")
  (defun my-org-element-at-point ()
    (interactive)
    ;; FIXME: minibuffer does not show result.
    (org-element-at-point)))


;;;_* custom functions

;; TODO:
;; (defun my-wrap-source-code-with-org-src ()
;;   "Wrap source code with Org-mode source code format."
;;   (interactive)
;;   (if (region-active-p)
;;       (let ((where (cons (region-beginning) (region-end))))
;;         (???))))
;;
;; (define-key org-mode-map (kbd "C-c k s") 'my-wrap-source-code-with-org-src)

;;;_* outorg -- Convert source-code buffers temporarily to Org-mode for comment editing.

;;; Usage:
;;
;; - [C-c '] (outorg-edit-as-org) :: main command
;; - [M-# #] (or M-x outorg-edit-as-org) ::
;; - [M-#] (outorg-copy-edits-and-exit) ::
;; - [C-x C-s] (outorg-save-edits-to-tmp-file) ::

(require 'outorg)

;; Outorg (like outshine) assumes that you set `outline-minor-mode-prefix' in your init-file to 'M-#':
;; NOTE: must be set before outline is loaded
(defvar outline-minor-mode-prefix "\M-#")

(global-set-key (kbd "C-c '") 'outorg-edit-as-org)

;;_* [ poporg ] -- Editing program comments or strings in text mode.
;;
;;; Usage:
;; - [poporg-dwim] :: [C-c ;]
;; - [poporg-edit-and-exit] :: [C-x C-s] in opened buffer.

(autoload 'poporg-dwim "poporg" nil t)

(setq poporg-adjust-fill-column t
      poporg-delete-trailing-whitespace t)

(global-set-key (kbd "C-c ;") 'poporg-dwim)

;;;_* org-trello

;;;_ + Usage:
;;
;; - [M-x org-trello-mode] :: activate org-trello
;;
;; - install key and token (~/.trello/config.el)
;;
;; (setq *consumer-key* "900d9b889d79bd46b6ade7d2d5eb9996")
;; (setq *access-token* "6d52ea5c38c78dd14120a28689849a6bd5825826c415516bd674522f6a8e16f1")
;;
;; org-trello now tries to enforce `symmetry', mainly regarding the sync
;; actions. So by default, an action pushes to trello. Using `C-u', the
;; symmetric action pulls from trello.
;;
;; Workflow:
;;
;; - setup ::
;;
;;   C-c o i or M-x org-trello/install-key-and-token
;;
;; - Connect org-mode buffer to board ::
;;
;;   C-c o I or M-x org-trello/install-board-and-lists-ids
;;
;;   For each org-mode file, you need to connect your org-mode file with a
;;   trello board.
;;
;;   Note This will present you with a list of your actual boards. Select the
;;   one you want and hit enter. This will edit your org-mode file to add the
;;   properties needed.
;;
;; - Create a board
;;
;;   You can avoid the previous step and create directly a board from your
;;   org-mode file.
;;
;;   Note: This will create the list from the keywords you use in your org-mode
;;   (cf. org-todo-keywords).
;;
;; - Migrations
;;
;;   org-trello now tries to enforce symmetry, mainly regarding the sync
;;   actions. So by default, an action pushes to trello. Using `C-u', the
;;   symmetric action pulls from trello.
;;
;;   Bindings 	       | Action
;; --------------------+---------------------------------------------------
;;   C-c o c 	       | sync the entity TO trello without its structure
;;   C-u C-c o c 	   | sync the entity FROM trello without its structure
;;   C-c o C 	       | sync the entity and its structure TO trello
;;   C-u C-c o C 	   | sync the entity and its structure FROM trello
;;   C-c o s 	       | sync the buffer TO trello
;;   C-u C-c o s 	   | sync the buffer FROM trello
;;   C-c o a 	       | assign yourself to the card
;;   C-u C-c o a 	   | unassign yourself from the card
;;
;; - Proxy
;;
;;   If you are using a proxy, as org-trello runs a local web server on port
;;   9876 which serves http request, you need to allow this to bypass your
;;   proxy. A solution to this in emacs would be for example (your mileage may
;;   vary):
;;
;;   (setenv "no_proxy" "localhost,127.0.0.0/8")


;; (require 'org-trello)

;; to have org-trello activated for each org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode in an org buffer to activate org-trello

;; TODO: enable org-trello-mode on `org-agenda-files'.

;; (define-key my-org-prefix-map (kbd "t") 'org-trello-mode)

;; (add-hook 'org-trello-mode-hook
;;           (lambda ()
;;             ;; 1. built-in provide function
;;             (org-trello/install-local-prefix-mode-keybinding! "C-c o t")
;;             ))

;; (org-trello/install-local-prefix-mode-keybinding! "C-c o t")

;; 2. my custom prefix map
;; (unless (boundp 'my-org-trello-map)
;;   (define-prefix-command 'my-org-trello-map))
;; (define-key my-org-prefix-map (kbd "t") 'my-org-trello-map)

;; ;; enable org-trello
;; (define-key my-org-trello-map (kbd "t") 'org-trello-mode)

;; ;; Install the keys and the access-token            
;; (define-key my-org-trello-map (kbd "I") 'org-trello/install-key-and-token)
;; ;; Connect buffer to board
;; (define-key my-org-trello-map (kbd "i") 'org-trello/install-board-and-lists-ids)
;; ;; Sync org file to board. With C-u prefix, sync org file from trello board
;; (define-key my-org-trello-map (kbd "s") 'org-trello/sync-buffer)
;; ;; Sync an entity to trello. With C-u prefix, sync an entity from trello
;; (define-key my-org-trello-map (kbd "c") 'org-trello/sync-entity)
;; ;; Sync an entity and its structure to trello. With C-u prefix, sync an entity and its structure from trello
;; (define-key my-org-trello-map (kbd "C") 'org-trello/sync-full-entity)
;; ;; Assign yourself to the card. With C-u prefix, unassign yourself from the card
;; (define-key my-org-trello-map (kbd "a") 'org-trello/assign-me)
;; ;; Check that the setup is ok
;; (define-key my-org-trello-map (kbd "d") 'org-trello/check-setup)
;; ;; Clean the org buffer from all org-trello information
;; (define-key my-org-trello-map (kbd "D") 'org-trello/delete-setup)
;; ;; Create a board and attach the org-mode file to it
;; (define-key my-org-trello-map (kbd "b") 'org-trello/create-board)
;; ;; Kill the entity from the board/org buffer. With C-u prefix, kill all entities.
;; (define-key my-org-trello-map (kbd "k") 'org-trello/kill-entity)
;; ;; Kill all entities from the board/org buffer
;; (define-key my-org-trello-map (kbd "K") 'org-trello/kill-all-entities)
;; ;; Jump to current trello card. With C-u prefix, jump to trello board.
;; (define-key my-org-trello-map (kbd "j") 'org-trello/jump-to-card)
;; ;; Jump to current trello board
;; (define-key my-org-trello-map (kbd "J") 'org-trello/jump-to-trello-board)
;; ;; Show the card's comments
;; (define-key my-org-trello-map (kbd "o") 'org-trello/show-card-comments)
;; ;; Show the card's labels
;; (define-key my-org-trello-map (kbd "l") 'org-trello/show-board-labels)
;; ;; Add a comment to the card
;; (define-key my-org-trello-map (kbd "A") 'org-trello/add-card-comments)
;; ;; Update the org buffer with trello board metadata
;; (define-key my-org-trello-map (kbd "u") 'org-trello/update-board-metadata)
;; ;; This help message
;; (define-key my-org-trello-map (kbd "h") 'org-trello/help-describing-bindings)


;;; [ org-passwords ]

;;; Usage:
;; - [M-x org-passwords] ::
;;   - [C-x C-q] :: switch read-only mode. for editing file.
;; - [M-x org-passwords-copy-password] ::
;; - [M-x org-passwords-open-url] ::
;; - [M-x org-passwords-generate-password] ::

(require 'org-passwords)

(setq org-passwords-file "~/Git/dotfiles/passwords.gpg"
      ;; org-passwords-default-password-size "20"
      ;; org-passwords-random-words-dictionary "/etc/dictionaries-common/words"
      ;; org-passwords-random-words-substitutions '(("for" . "4") ("s" . "5"))
      ;; org-passwords-password-property "PASSWORD"
      ;; org-passwords-username-property "USERNAME"
      ;; org-passwords-url-property "URL"
      org-passwords-time-opened "1 min"
      )

(eval-after-load "org-passwords"
  '(progn
     (define-key org-passwords-mode-map
       (kbd "C-c u")
       'org-passwords-copy-username)
     (define-key org-passwords-mode-map
       (kbd "C-c p")
       'org-passwords-copy-password)
     (define-key org-passwords-mode-map
       (kbd "C-c o")
       'org-passwords-open-url)))

;;; Making new entries in the database
;;; To enter new passwords, you can use 'org-capture' and a minimal template like:
;;
;; ("p" "password" entry (file "~/documents/passwords.gpg")
;;  "* %^{Title}\n  %^{URL}p %^{USERNAME}p %^{PASSWORD}p %^{TAGS}p")

;; When asked for the password you can then call either
;; 'org-passwords-generate-password' or 'org-passwords-random-words'.
;; Be sure to enable recursive minibuffers to call those functions from the minibuffer:
(setq enable-recursive-minibuffers t)

(defun my-org-passwords-search ()
  "Search entry in org-passwords."
  (interactive)
  (org-passwords)
  (switch-to-buffer "passwords.gpg")
  (if (boundp 'vr/isearch-forward) ; TODO: add thing-at-point support here.
      (vr/isearch-forward)
    (isearch-forward-regexp)))

;; (define-key my-org-prefix-map (kbd "p") 'org-passwords)
(define-key my-org-prefix-map (kbd "p") 'my-org-passwords-search)



;;;_*
(provide 'init-my-tool-org-mode)

;;; init-my-tool-org-mode.el ends here
