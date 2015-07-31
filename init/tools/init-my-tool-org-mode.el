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
(require 'org-compat)
(require 'org-table)
(require 'org-habit)
(require 'org-timer)
(require 'org-clock)
(require 'org-notify)
(require 'org-pcomplete)

(require 'org-plot)
;;; org-protocol need server start.
(unless (server-running-p)
  (server-start))
(require 'org-protocol)

(require 'ox-beamer)
(require 'ox-latex)
;; (require 'ox-bibtex)
(require 'ox-odt)
(require 'ox-html)
(require 'ox-publish)

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
      org-startup-with-inline-images nil ; `org-toggle-inline-images' [C-c C-x C-v]
      ;; FIXME: error: Can't preview LaTeX fragment in a non-file buffer.
      org-startup-with-latex-preview nil ; `org-toggle-latex-fragment' [C-c C-x C-l]
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
      org-emphasis-alist '(("*" bold)
                           ("/" italic)
                           ("_" underline)
                           ("=" org-verbatim verbatim)
                           ("~" org-code verbatim)
                           ("+" (:strike-through t))
                           ;; FIXME: this does not work, seems function only take one character.
                           ;; ("<kbd>" org-code verbatim)
                           )
      org-hide-block-startup nil ; don't fold block.
      ;; org-hide-block-overlays t ; overlays hiding blocks.
      )

(setq org-fontify-emphasized-text t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t)

;; 'auto, t, nil. ((heading . auto) (plain-list-item . auto))
(setq org-blank-before-new-entry
      '((heading . auto)
        (plain-list-item . auto)))
(setq org-list-empty-line-terminates-plain-lists t)
;; internal link (in same file) open style
(setq org-display-internal-link-with-indirect-buffer nil)
(setq org-indirect-buffer-display 'current-window)


;;;_* org-bullets

;; (load-file "~/.emacs.d/my-init/extensions/org-bullets.el")

(require 'org-bullets nil t)

;;; You Can copy symbols from Desktop Utils "Character Maps".
;; ("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
;; ("⒈" "⒉" "⒊" "⒋" "⒌" "⒍" "⒎" "⒏" "⒐" "⒑" "⒒" "⒓" "⒔" "⒕" "⒖" "⒗" "⒘" "⒙" "⒚" "⒛")
;; ("⑴" "⑵" "⑶" "⑷" "⑸" "⑹" "⑺")

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-bullets-bullet-list
                  '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
                  ;; '("❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽" "❾" "❿")
                  ;; '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩")
                  ;; '("㊀" "㊁" "㊂" "㊃" "㊄" "㊅" "㊆" "㊇" "㊈" "㊉")
                  ;; '("㈠" "㈡" "㈢" "㈣" "㈤" "㈥" "㈦" "㈧" "㈨" "㈩")
                  )

            ;; TODO: (setq org-bullets-face-name 'org-level-1)

            (org-bullets-mode 1)
            ))

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

(add-hook 'org-mode-hook
          (lambda ()
            (when (org-bullets-mode)
              (org-indent-mode 1))
            ))

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
                    :foreground "gray" :background (color-darken-name (face-background 'default) 5)
                    :box '(:color "black" :line-width 1 :style nil)
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
;; Daily entry (holidays)
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
                    :foreground "white"
                    :background "dark red"
                    :box '(:color "red" :line-width -1)
                    :bold nil)
;; (setq org-priority-faces '(:foreground "cyan"
;;                                        :background nil
;;                                        :box t
;;                                        :inverse-video nil
;;                                        :inherit nil))

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
                    :bold nil
                    :underline nil)
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
                    :foreground "dark cyan"
                    :background (color-darken-name (face-background 'default) 3)
                    :weight 'normal :slant 'normal
                    :box '(:color "black" :line-width 1)
                    )
(set-face-attribute 'org-block-end-line nil
                    :foreground "dark cyan"
                    :background (color-darken-name (face-background 'default) 3)
                    :weight 'normal :slant 'normal
                    :box '(:color "black" :line-width 1)
                    )
;; seems face `org-block-background' is removed from commit: f8b42e8ebeeecdef59a8a7cbc4324264a5162197 , because it is slower for fontify.
;; (set-face-attribute 'org-block-background nil
;;                     :foreground nil :background "#222222"
;;                     :foreground nil :background "#004A5D"
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

;; inline babel call
;; ... call_<name>[<inside header arguments>](<arguments>)[<end header arguments>] ...
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\(\\[.*\\]\\)\\((.*)\\)\\(\\[.*\\]\\)"
    (1 '(:foreground "dim gray" :height 60)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic)) ; <name>
    (3 '(:foreground "gray" :height 60)) ; [<inside header arguments>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    (5 '(:foreground "gray" :height 60)) ; [<end header arguments>]
    )))
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\((.*)\\)"
    (1 '(:foreground "dim gray" :height 60)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic)) ; <name>
    (3 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
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

;; TODO: configure org latex preview style
(setq org-latex-create-formula-image-program 'dvipng
      org-latex-preview-ltxpng-directory "ltxpng/"
      org-format-latex-options (plist-put
                                org-format-latex-options :scale 3.0)
      org-format-latex-options (plist-put
                                org-format-latex-options :html-scale 2.0))

;; (setq org-latex-default-packages-alist
;;       org-latex-packages-alist)
;;
;; So, let's assume that you like minted over listings and have decided to use
;; it for latex export. You read the documentation for the relevant variable,
;; org-latex-listings, and set up things as the documentation suggests:
;;
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;;
;; To exclude minted from latex preview then, all we have to do is change what
;; we add to org-latex-packages-alist:
;;
;; (add-to-list 'org-latex-packages-alist '("" "minted" nil))

;; The same problem as you describe in the original-post exists for
;; latex-preview. But, for that case there is a better solution:
;;
;; (require 'face-remap)
;; (defadvice preview-inherited-face-attribute (after preview-inherit-local-face nil activate)
;;   "Scale preview images with respect to buffer-local face"
;;   (when (and text-scale-mode (eq attribute :height))
;;     (setq ad-return-value (* (expt text-scale-mode-step text-scale-mode-amount) ad-return-value))))

;; @@html:<kbd>...</kbd>@@, <kbd> </kbd>
(defface org-html-kbd-tag
  '((nil (:foreground "cyan" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1)
                    ;; :weight 'bold
                    )))
  "Face for highlight Org-mode html tag @<kbd>...@</kbd> or @@html:<kbd>...</kbd>@@."
  :group 'org-faces)

;; @@html:<kbd>C-h h</kbd>@@
(font-lock-add-keywords
 'org-mode
 '(("@@html:<kbd>\\([^<]*\\)</kbd>@@"
    (1 'org-html-kbd-tag))))

;; @<kbd>C-h h@</kbd>
(font-lock-add-keywords
 'org-mode
 '(("@<kbd>\\([^@]*\\)@</kbd>"
    (1 'org-html-kbd-tag))))


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

;;; Inserting the kbd tag in interactively
(eval-after-load 'ox-html
  ;; If you prefer to use ~ for <code> tags. Replace "code" with
  ;; "verbatim" here, and replace "~" with "=" below.
  '(push '(code . "<kbd>%s</kbd>") org-html-text-markup-alist))

(defun my/insert-key (key)
  "Ask for a KEY then insert its description.
Will work on both `org-mode' and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp
                  ;; "~%s~"
                  "=[%s]="
                ;; "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\C-m"))
        (insert
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if orgp -2 -6)))))

(define-key org-mode-map (kbd "C-c K") 'my/insert-kbd)
(define-key org-mode-map (kbd "C-c k") 'my/org-insert-key)
;; TODO:
;; (eval-after-load 'web-mode
;;   (define-key web-mode-map (kbd "C-c k") 'my/insert-key))

;;; headline faces
;;; the ahead stars face when org indentation. (org-hide)
(set-face-attribute 'org-hide nil
                    :foreground "#002B36" :background "#002B36")
(set-face-attribute 'org-level-1 nil
                    :family "DejaVu Sans Mono"
                    :weight 'normal :height 105
                    :foreground "#FF3870"
                    :background (color-darken-name (face-background 'default) 4)
                    :box '(:color "black" :line-width -1 :style nil)
                    ;; :overline "#555555"
                    )
(set-face-attribute 'org-level-2 nil
                    :inherit 'org-level-1
                    :foreground "#C8C800"
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
(set-face-attribute 'org-headline-done nil
                    :foreground "#444444"
                    :background nil)
;;; tags
(set-face-attribute 'org-tag nil
                    :foreground "cyan"
                    :underline nil :weight 'normal :slant 'normal
                    :box '(:color "dark green" :line-width 2)
                    :height 80)
;; meta lines
(set-face-attribute 'org-meta-line nil
                    :foreground "yellow"
                    :background (color-darken-name (face-background 'default) 5)
                    )
;;; checkbox faces
;; - [ ], - [X]
(set-face-attribute 'org-checkbox nil
                    :bold 'normal
                    :box '(:line-width 1 :color "black" :style nil)
                    :foreground "dark gray"
                    :background nil)
;; * headline [7%] -> checkbox statistics face.
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :box '(:color "black" :line-width -1)
                    :foreground "green yellow"
                    :background (color-darken-name (face-background 'default) 5)
                    )
(set-face-attribute 'org-checkbox-statistics-done nil
                    :background "#444444" :foreground "black"
                    :box '(:color "black" :line-width -1)
                    :strike-through t)
;;; list definition terms
(set-face-attribute 'org-list-dt nil
                    :foreground "green yellow")
;;; link face
(set-face-attribute 'org-link nil
                    :foreground "cyan"
                    :background (color-darken-name (face-background 'default) 5)
                    :underline "dark cyan"
                    ;; :box '(:color "black")
                    )
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

;; special keywords
(set-face-attribute 'org-special-keyword nil
                    :foreground "dark orange"
                    :background (color-darken-name (face-background 'default) 5)
                    )
(set-face-attribute 'org-property-value nil
                    :foreground "orange"
                    :slant 'italic)

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

(if (featurep 'ido-vertical-mode)
    (setq org-completion-use-ido t)
  (setq org-completion-use-ido nil)
  (setq org-completion-use-iswitchb nil)
  (if (featurep 'helm)
      (setq org-completion-fallback-command 'helm)
    (setq org-completion-fallback-command 'hippie-expand))
  )

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


;; TODO: iimage-mode.
;; -----------------------------------------------------------------------------
;; (if (not (featurep 'iimage))
;;     (require 'iimage))
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

(setq org-footnote-auto-label 'confirm
      org-footnote-auto-adjust t
      org-footnote-define-inline nil ; define foot-note inline, instead of separate section.
      ;; org-footnote-fill-after-inline-note-extraction
      ;; org-footnote-section
      ;; org-footnote-tag-for-non-org-mode-files
      )

;;;_*, hyperlinks

(setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s") ; xdg-open, kde-open, gnome-open.

;; open IRC link with Emacs ERC.
(setq org-irc-client 'erc)


(setq org-file-apps
      '(;; default
        ;; (auto-mode . emacs)
        ;; Web Pages
        ("\.x?html\'" . default)
        ("\(?:xhtml\|html\)" . "firefox %s")
        ;; PDF
        ("\\.pdf\\'" . auto-mode)
        ("\\.pdf::\\([[:digit:]]+\\)\\'" . auto-mode)
        ;; NOTE: disable this, to use `doc-view' for PDF.
        ;; ("\\.pdf\\'" . "okular %s")
        ;; ("\\.pdf::\\([[:digit:]]+\\)\\'" . "okular -p %1 %s")
        ;; CHM
        ("\\.chm\\'" . "kchmviewer %s")
        ;; EPUB
        ("\\.epub\\'" . "ebook-viewer %s")
        ;; AZW3
        ("\\.azw3\\'" . "ebook-viewer %s")
        ;; mobi
        ("\\.mobi\\'" . "ebook-viewer %s")
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

;;; [ Link abbreviations ]
;; TODO add more link abbrev into this variable.
;; NOTE: you can not contain chinese string in "link name". Org-mode does not support it.
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
        ("Baidu_BaiKe" . "http://baike.baidu.com/search/none?word=%s")
        ;; Q & A
        ("Quora" . "https://www.quora.com/search?q=%s")
        ("ZhiHu" . "http://www.zhihu.com/search?q=%s&type=question")
        ("Baidu_ZhiDao" . "http://zhidao.baidu.com/search?word=%s")
        ("Baidu_JingYan" . "http://jingyan.baidu.com/search?word=%s")
        ;; Maps
        ("Google_Maps" . "http://maps.google.com/maps?q=%s")
        ;; Social Networkings
        ("Twitter" . "https://twitter.com/%s")
        ("Facebook" . "https://www.facebook.com/%s")
        ;; Programming
        ("Stack_Exchange_Stack_Overflow" . "http://stackoverflow.com/search?q=%s")
        ("Stack_Exchange_Programmers" . "http://programmers.stackexchange.com/search?q=%s")
        ;; Emacs
        ("Emacs_Wiki" . "www.emacswiki.org/emacs?search=%s")
        ("Stack_Exchange_Emacs" . "http://emacs.stackexchange.com/search?q=%s")
        ;; Document Search
        ("Mozilla_Developer" . "https://developer.mozilla.org/en-US/search?q=%s")
        ;; API Search
        ("{API}Search_apis.io" . "http://apis.io/?search=%s")
        ;; Code Search
        ("search_code" . "http://searchcode.com/?q=%s")
        ("GitHub" . "https://github.com/search?q=%s")
        ("Bitbucket" . "https://bitbucket.org/repo/all?name=%s")
        ("Google_Code" . "https://code.google.com/query/#q=%s")
        ("Launchpad" . "https://launchpad.net/+search?field.text=%s")
        ("Code_Project" . "http://www.codeproject.com/search.aspx?q=%s")
        ("CodePlex" . "https://www.codeplex.com/site/search?query=%s")
        ("Gitorious" . "https://gitorious.org/search?q=%s")
        ("SourceForge" . "https://sourceforge.net/directory/?q=%s")
        ("Freecode" . "http://freecode.com/search?q=%s")
        ("Active_State" . "http://code.activestate.com/search/#q=%s")
        ("Ohloh_Code" . "http://code.ohloh.net/search?s=%s")
        ("Snipplr" . "http://snipplr.com/search.php?q=%s")
        ;; chinese code search
        ("GitCafe" . "https://gitcafe.com/search?keyword=%s")
        ("Coding" . "https://coding.net/search?q=%s")
        ("Geakit" . "https://geakit.com/search?q=%s")
        ("Git_OSC_Open_Source_China" . "https://git.oschina.net/search?search=%s")
        ;; Lisp
        ("lispdoc" . "http://lispdoc.com/?q=%s")
        ;; Ruby
        ("Ruby-Doc" . "http://ruby-doc.com/search.html?q=%s")
        ;; Python
        ("Python_3_Documentation" . "http://docs.python.org/3/search.html?q=%s")
        ;; Perl
        ("Perl_CPAN" . "http://search.cpan.org/search?mode=all&query=%s")
        ;; PHP
        ("PHP_online_documentation" . "http://cn2.php.net/results.php?q=%s&p=manual")
        ;; JavaScript
        ("JavaScript_Mozilla" . "https://developer.mozilla.org/en-US/search?q=%s")
        ;; HTML
        ;; CSS
        ;; Bug
        ("Bugzilla" . "http://bugzilla/show_bug.cgi?id=%s")
        ;; Book
        ("DouBan_Books" . "http://book.douban.com/subject_search?search_text=%s")
        ;; Movie
        ("DouBan_Movies" . "http://movie.douban.com/subject_search?search_text=%s")
        ;; The Pirate Bay
        ("The_Pirate_Bay" . "http://thepiratebay.se/search/%s")
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

;; TODO: auto create link to filename smartly when not link on word/region.
;; [C-c C-o] :: auto create filename on word or region if no link.
;; (defadvice org-open-at-point (before create-file-when-org-link-invalid activate)
;;   ())
;;

;; NOTE: Seems Org-mode's link detect is smart now, whether deprecated this function?
;; [C-u C-c C-o] :: open in external browser
;; (defun my-org-open-at-point (&optional arg)
;;   (interactive)
;;   (if (not arg)
;;       (org-open-at-point)
;;     (let ((browse-url-browser-function #'browse-url-firefox))
;;       (org-open-at-point))))
;; (if (functionp 'my-org-open-at-point)
;;     (define-key org-mode-map (kbd "C-c C-o") 'my-org-open-at-point))
;;
;; (setq org-return-follows-link nil) ; to follow links with [RET], rather 2 key combo.

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

;;;_* org-linkany -- Insert link using anything.el/helm.el on org-mode.

;;; Usage:
;;
;; - persistent-action in helm to view http/https/ftp links.

;; (require 'org-linkany)

;; (add-to-list org-linkany/url-source-collection ')
;;
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

;;;_* org-timer

(eval-after-load "org"
  (lambda ()
    (require 'org-timer)
    (add-to-list 'org-modules 'org-timer)
    ))

(setq org-timer-default-timer 25)       ; Pomodoro time management technique.
(setq org-timer-display 'mode-line)

;;;_* Clock

(require 'org-clock)

(setq org-clock-persist t ; nil, t, 'clock, 'history
      org-clock-persist-query-save t
      org-clock-persist-query-resume t
      org-clock-persist-file "~/.emacs.d/org-clock-save.el"
      org-clock-in-resume t    ; resume when clock in.
      org-clock-continuously nil ; don't continue on last clock out.
      org-clock-in-switch-to-state "STARTED"
      org-clock-out-when-done t         ; clock will stop when task marked DONE.
      org-clock-into-drawer t  ; Save clock data and notes in the :LOGBOOK: drawer
      org-clock-out-remove-zero-time-clocks t ; Removes clocked tasks with 0:00 duration
      org-clock-sound "~/.emacs.d/resources/audio/Ingress/Speech/speech_hacking.wav"
      org-clock-clocked-in-display 'mode-line ; 'mode-line, 'frame-title, 'both, nil.
      ;; org-clock-mode-line-entry t
      org-clock-mode-line-total 'auto
      ;; org-clock-clocktable-language-setup
      ;; org-clock-leftover-time
      ;; org-clock-task-overrun
      ;; org-clock-task-overrun-text
      ;; org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)
      org-clock-report-include-clocking-task t
      ;; org-agenda-clockreport-mode
      ;; org-agenda-start-with-clockreport-mode t
      org-clock-goto-may-find-recent-task t
      ;; org-clock-total-time-cell-format "*%s*"
      org-clock-idle-time nil             ; t
      ;; org-clock-auto-clock-resolution 'when-no-clock-is-running
      ;; org-clock-resolve-expert t
      )

;;; To save the clock history across Emacs sessions, use
;; (setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; to add an effort estimate "on the fly".
(add-hook 'org-clock-in-prepare-hook 'org-clock-modify-effort-estimate)
;; (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer) ; `org-clock-out-remove-zero-time-clocks'

(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)

;; Modify the org-clock-in so that a timer is started with the default value
;; except if a timer is already started:
;;
;; (add-hook 'org-clock-in-hook
;;           '(lambda ()
;;              (if (not org-timer-current-timer) ; FIXME: this variable seems not part of `org-timer'.
;;                  (org-timer-set-timer '(16)))))

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

;;;_* time-stamps

;; (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
;;       org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>") ; toggle by `org-toggle-time-stamp-overlays' [C-c C-x C-t].
;;       ;; org-time-stamp-rounding-minutes '(0 5)
;;       )

;;;_* Agenda Views

(require 'org-agenda)

;; agenda view
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (search . " %i %-12:c")
        (tags . " %i %-12:c")))
(setq org-agenda-remove-times-when-in-prefix t ; remove duplicate time specifications in agenda items.
      ;; org-agenda-remove-tags t
      org-agenda-remove-tags-when-in-prefix t ; remove tags when prefix contains a `%T' specifier.
      ;; org-agenda-remove-timeranges-from-blocks ; remove time ranges specifications in agenda
      )

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

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-window-frame-fractions '(0.5 . 0.75)) ; the min and max height of the agenda window as a fraction of frame height.
(setq org-agenda-span 'week)
;; speedup Org Agenda
(setq org-agenda-dim-blocked-tasks nil
      org-agenda-inhibit-startup nil
      org-agenda-use-tag-inheritance nil)


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
        ;; used to filter out fragment time tasks.
        ("f" "Fragment time tasks"
         ((tags "fragment")))
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

;; How to make icon with GIMP?
;;
;; 1. "scale" image to [16x16] pixels.
;; 2. "select by color" -> "color to alpha"
;; 3. export to .xpm format. (or .png)
(setq org-agenda-category-icon-alist
      '(("Org" "~/.emacs.d/resources/icon/Org.xpm" nil nil :ascent center)
        ("Emacs" "~/.emacs.d/resources/icon/Emacs.xpm" nil nil :ascent center)
        ("Code" "~/.emacs.d/resources/icon/Code.xpm" nil nil :ascent center)
        ("Programming" "~/.emacs.d/resources/icon/Code.xpm" nil nil :ascent center)
        ("Bug" "~/.emacs.d/resources/icon/Bug.xpm" nil nil :ascent center)
        ("Issue" "~/.emacs.d/resources/icon/GitHub Octocat.xpm" nil nil :ascent center)
        ("Feature" "~/.emacs.d/resources/icon/GitHub Octocat.xpm" nil nil :ascent center)
        ("Idea" "~/.emacs.d/resources/icon/Idea.xpm" nil nil :ascent center)
        ("Project"  "~/.emacs.d/resources/icon/Code.xpm" nil nil :ascent center)
        ("Startup"  "~/.emacs.d/resources/icon/Startup.xpm" nil nil :ascent center)
        ("Hack"  "~/.emacs.d/resources/icon/Hack.xpm" nil nil :ascent center)
        ("Daily" '(space . (:width (16)))) ; to display a 16px horizontal space
        ("Learning"  "~/.emacs.d/resources/icon/Org.xpm" nil nil :ascent center)
        ("Linux" "~/.emacs.d/resources/icon/Linux.xpm" nil nil :ascent center)
        ("GNU" "~/.emacs.d/resources/icon/GNU.xpm" nil nil :ascent center)
        ("Arch" "~/.emacs.d/resources/icon/Arch.xpm" nil nil :ascent center)
        ("Ubuntu" "~/.emacs.d/resources/icon/Ubuntu.xpm" nil nil :ascent center)
        ("Kali" "~/.emacs.d/resources/icon/Kali.xpm" nil nil :ascent center)
        ("BSD" "~/.emacs.d/resources/icon/BSD.xpm" nil nil :ascent center)
        ("Android" "~/.emacs.d/resources/icon/Android.xpm" nil nil :ascent center)
        ("Lisp" "~/.emacs.d/resources/icon/common-lisp.xpm" nil nil :ascent center)
        ("Scheme" "~/.emacs.d/resources/icon/Scheme.xpm" nil nil :ascent center)
        ("Clojure" "~/.emacs.d/resources/icon/Clojure.xpm" nil nil :ascent center)
        ("Ruby" "~/.emacs.d/resources/icon/Ruby.xpm" nil nil :ascent center)
        ("Rails" "~/.emacs.d/resources/icon/Rails.xpm" nil nil :ascent center)
        ("Python" "~/.emacs.d/resources/icon/Python.xpm" nil nil :ascent center)
        ("Perl" "~/.emacs.d/resources/icon/Perl.xpm" nil nil :ascent center)
        ("Shell" "~/.emacs.d/resources/icon/Shell.xpm" nil nil :ascent center)
        ("PHP" "~/.emacs.d/resources/icon/PHP.xpm" nil nil :ascent center)
        ("Haskell" "~/.emacs.d/resources/icon/Haskell.xpm" nil nil :ascent center)
        ("Erlang" "~/.emacs.d/resources/icon/Erlang.xpm" nil nil :ascent center)
        ("Prolog" "~/.emacs.d/resources/icon/Prolog.xpm" nil nil :ascent center)
        ("Assembly" "~/.emacs.d/resources/icon/Assembly.xpm" nil nil :ascent center)
        ("C" "~/.emacs.d/resources/icon/C.xpm" nil nil :ascent center)
        ("C++" "~/.emacs.d/resources/icon/C++.xpm" nil nil :ascent center)
        ("D" "~/.emacs.d/resources/icon/D.xpm" nil nil :ascent center)
        ("Go" "~/.emacs.d/resources/icon/Go.xpm" nil nil :ascent center)
        ("Swift" "~/.emacs.d/resources/icon/Swift.xpm" nil nil :ascent center)
        ("Rust" "~/.emacs.d/resources/icon/Rust.xpm" nil nil :ascent center)
        ("Scala" "~/.emacs.d/resources/icon/Scala.xpm" nil nil :ascent center)
        ("Java" "~/.emacs.d/resources/icon/Java.xpm" nil nil :ascent center)
        ("HTML5" "~/.emacs.d/resources/icon/HTML5.xpm" nil nil :ascent center)
        ("CSS3" "~/.emacs.d/resources/icon/CSS3.xpm" nil nil :ascent center)
        ("JavaScript" "~/.emacs.d/resources/icon/JavaScript.xpm" nil nil :ascent center)
        ("SQL" "~/.emacs.d/resources/icon/SQL.xpm" nil nil :ascent center)
        ("NoSQL" "~/.emacs.d/resources/icon/NoSQL.xpm" nil nil :ascent center)
        ("NewSQL" "~/.emacs.d/resources/icon/NewSQL.xpm" nil nil :ascent center)
        ("R" "~/.emacs.d/resources/icon/R.xpm" nil nil :ascent center)
        ("Julia" "~/.emacs.d/resources/icon/Julia.xpm" nil nil :ascent center)
        ("Quipper" "~/.emacs.d/resources/icon/Quipper.xpm" nil nil :ascent center)
        ("TeX" "~/.emacs.d/resources/icon/TeX.xpm" nil nil :ascent center)
        ("GitHub" "~/.emacs.d/resources/icon/GitHub Octocat.xpm" nil nil :ascent center)
        ("GFW" "~/.emacs.d/resources/icon/GFW.xpm" nil nil :ascent center)
        (".*" '(space . (:width (16))))
        ))

;;;_* about TODO tasks

;;; TODOs status
;; `|` separate finished and unfinished two statuses, will add timestamp when finished.
;; `(t)` set shortcut
;; `(d!)` add timestamp
;; `(d@)` need add note declaration
;; `(d@/!)` add timestamp and note
(setq org-todo-keywords
      '(;; Clock
        (sequence "STARTED(!)" "|" "DONE(d@/!)")
        ;; habits
        (sequence "HABIT(h)" "|" "DONE(d)")
        ;; Status
        (sequence "Urgent(u!)" "Doing(g!)" "TODO(t@/!)" "Later(l!)" "SOMEDAY(s!)" "FAILED(x@/!)" "CANCELLED(C@/!)" "|" "DONE(d@/!)")
        ;; Types
        ;; use (@/!) to record/log info reference source link URL and timestamp.
        (type "code(c@/!)" "project(p@/!)" "Org(o@/!)" "|" "DONE(d@/!)")
        ;; Code
        (sequence "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "|" "DONE(d@/!)")
        ;; Work
        (type "Work(w@/!)" "Meeting(m@/!)" "|" "DONE(d@/!)")
        ;; Learn
        (type "Learn(n!)" "Review(r!)" "|" "DONE(d@/!)")
        ;; Life
        (type "SEX(X@/!)" "|" "DONE(d@/!)")
        ;; org-trello
        ;; (type "TODO" "Doing" "|" "DONE")
        ))
(setq org-todo-keyword-faces
      (quote (;;; todos
              ("TODO" :foreground "orange" :weight bold
               :box '(:color "black" :line-width -1))
              ("Urgent" :foreground "red" :background "black"
               :weight bold
               ;; :overline "red"
               :box '(:color "black" :line-width -1 :style nil)
               )
              ("STARTED" :foreground "green" :weight bold
               :box '(:color "red" :line-width -1))
              ("HABIT" :foreground "cyan" :background "black" :weight bold
               :box '(:color "green" :line-width -1))
              ("SOMEDAY" :foreground "dim gray" :weight bold
               :box '(:color "black" :line-width -1))
              ("Doing" :foreground "cyan" :weight bold
               :box '(:color "black" :line-width -1))
              ("Later" :foreground "orange" :background "black" :weight bold
               :box '(:color "dark red" :line-width -1))
              ("DONE" :foreground "black" :background nil :weight bold :strike-through t
               :box '(:color "black" :line-width -1))
              ("FAILED" :foreground "#444444" :background "orange" :weight bold :underline "dark red"
               :box '(:color "black" :line-width -1))
              ("CANCELLED" :foreground "#444444" :background "orange" :weight bold :strike-through t
               :box '(:color "black" :line-width -1))
              ;;; fixme
              ("BUG" :foreground "red" :background nil :weight bold
               :box '(:color "red" :line-width -1 :style nil))
              ("ISSUE" :foreground "red" :background nil :weight bold
               :box '(:color "dark red" :line-width -1 :style nil))
              ("ERROR" :foreground "red" :weight bold
               :box '(:color "red" :line-width -1 :style nil))
              ("FIXME" :foreground "black" :background "red" :weight bold
               :box '(:color "dark red" :line-width -1 :style nil))
              ("FEATURE" :foreground "cyan" :weight bold
               :box '(:color "cyan" :line-width -1 :style nil))
              ;;; types
              ("Org" :foreground "cyan" :backgrund "#004A5D" :weight bold
               :box '(:color "cyan" :line-width -1 :style nil))
              ("code" :foreground "white" :background "#004A5D"
               :box '(:color "cyan" :line-width -1 :style nil))
              ("project" :foreground "white" :background "#004A5D"
               :box '(:color "cyan" :line-width -1 :style nil))
              ;; life
              ("SEX" :foreground "deep pink" :weight bold
               :box '(:color "deep pink" :line-width -1 :style nil))
              ("Outside" :foreground "yellow"
               :box '(:color "yellow" :line-width -1 :style nil))
              ;; work
              ("Work" :foreground "orange"
               :box '(:color "black" :line-width -1 :style nil))
              ("Meeting" :foreground "cornflower blue"
               :box '(:color "cyan" :line-width -1 :style nil))
              ;; learn
              ("Learn" :foreground "dark orange" :background nil
               :box '(:color "black" :line-width -1))
              ("Learning" :foreground "green yellow" :background nil
               :box '(:color "black" :line-width -1))
              ("Review" :foreground "yellow" :background nil
               :box '(:color "black" :line-width -1))
              )))

;;;_*, custom functions

;;; bind key [C-l] to locate to current time now ----- in Org-Agenda buffer.
(defun my-org-agenda-jump-to-current-time ()
  "Jump to current time now."
  (interactive)
  (goto-char (text-property-any (point-min) (point-max) 'face 'org-agenda-current-time))
  (recenter-top-bottom)
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
        ;; fragment time tasks
        (:startgroup . nil)
        ("fragment" . nil)
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
      org-export-babel-evaluate t ; code evaluation during export. nil, 'inline-only,
      ;; org-export-headline-levels 6
      org-export-with-smart-quotes t
      org-export-with-emphasize t
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

;; (setq org-publish-use-timestamps-flag nil)

;; Publishing to a local directory is also much faster than to a remote one, so
;; that you can afford more easily to republish entire projects. If you set
;; `org-publish-use-timestamps-flag' to nil, you gain the main benefit of
;; re-including any changed external files such as source example files you
;; might include with #+INCLUDE:. The timestamp mechanism in Org is not smart
;; enough to detect if included files have been modified.


;;; [ Blog ]


;;; [ o-blog ] -- A stand-alone blog and publication tool for Org-mode.

;;; Usage:
;;
;; Publish:
;;   - Open the ~/.emacs.d/o-blog/example/sample.org file
;;   - type M-x org-publish-blog
;;   - The result site would be published by default in ~/.emacs.d/o-blog/out.

;; (require 'o-blog)



;; (require 'org-blog)


;;; [ org2jekyll ]


;;; [ org-jekyll ]

;; (require 'org-jekyll)




;;;_* org-html5

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      ;; org-html-doctype-alist
      )

;;;_* Babel
;;
;; - [C-c C-v] :: keymap prefix for babel. `org-babel-map'


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
   (processing . t)                     ; Processing
   (ledger . t)                         ; ledger support in Babel
   ;; (sml . t)                            ; from extension ob-sml
   (sass . t)                           ; Sass
   ))

;; Or by using `require' to load.
(require 'ob-processing)

;;; [ ob-julia ]  (require ESS)
;;
;; TODO: read https://github.com/gjkerns/ob-julia/blob/master/ob-julia-doc.org

(setq inferior-julia-program-name "julia")

;; 1.
;; (load "~/.emacs.d/el-get/ob-julia/ob-julia.el")
;; 2.
;; (add-to-list 'load-path "~/.emacs.d/el-get/ob-julia/ob-julia.el")
;; 3.
(my-el-get-require 'ob-julia)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((julia . t)))

;; (setq org-babel-julia-eoe-indicator "print(\"org_babel_julia_eoe\")")
(setq org-babel-default-header-args:julia
      '((:results . "replace output")
        (:padnewline . "yes")))

;; [ ob-mongo ] -- babel for MongoDB

(my-el-get-require 'ob-mongo)

;; [ ob-go ] -- babel for Go
;;
;; Usage:
;;
;; * simple example:
;;
;; #+BEGIN_SRC go :imports "fmt"
;; fmt.Println("Hello, World")
;; #+END_SRC
;;
;; #+results:
;; : Hello, World
;;
;; * multiple imports
;;
;; #+BEGIN_SRC go :imports '("fmt" "time")
;; fmt.Println("Current Time:", time.Now())
;; #+END_SRC
;;
;; #+RESULTS:
;; : Current Time: 2015-05-23 13:02:50.87256801 +0800 CST
;;
;; * concurrent prime sieve
;;
;; #+begin_src go
;;   // A concurrent prime sieve
;;   package main
;;
;;   import "fmt"
;;
;;   // Send the sequence 2, 3, 4, ... to channel 'ch'.
;;   func Generate(ch chan<- int) {
;;           for i := 2; ; i++ {
;;                   ch <- i // Send 'i' to channel 'ch'.
;;           }
;;   }
;;
;;   // Copy the values from channel 'in' to channel 'out',
;;   // removing those divisible by 'prime'.
;;   func Filter(in <-chan int, out chan<- int, prime int) {
;;           for {
;;                   i := <-in // Receive value from 'in'.
;;                   if i%prime != 0 {
;;                           out <- i // Send 'i' to 'out'.
;;                   }
;;           }
;;   }
;;
;;   // The prime sieve: Daisy-chain Filter processes.
;;   func main() {
;;           ch := make(chan int) // Create a new channel.
;;           go Generate(ch)      // Launch Generate goroutine.
;;           for i := 0; i < 10; i++ {
;;                   prime := <-ch
;;                   fmt.Println(prime)
;;                   ch1 := make(chan int)
;;                   go Filter(ch, ch1, prime)
;;                   ch = ch1
;;           }
;;   }
;; #+end_src
;;
;; #+RESULTS:
;; #+begin_example
;;   2
;;   3
;;   5
;;   7
;;   11
;;   13
;;   17
;;   19
;;   23
;;   29
;; #+end_example
;;


(my-el-get-require 'ob-go)

;; [ ob-prolog ] -- babel for Prolog

(my-el-get-require 'ob-prolog)

;; [ ob-http ] -- http request in org-mode babel

(my-el-get-require 'ob-http)

;; [ ob-browser ] -- render HTML in org babel

(my-el-get-require 'ob-browser)


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
(setq org-src-fontify-natively t ; fontify code in code blocks. (highlight code in exported HTML)
      ;; NOTE: fontify-natively (slow-down/suspend on Emacs)
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
                                   ;; ("ocaml" . "ml")
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

;;;_ * Library of Babel

;;; Usage:
;;
;; - `org-babel-lob-ingest' [C-c C-v i]

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

;;;_* Math formula support

(setq org-latex-create-formula-image-program 'dvipng)

;;; MathJax
;;
;; (setq org-html-mathjax-options '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML")
;;                                  (scale "100")
;;                                  (align "center")
;;                                  (font "TeX")
;;                                  (linebreaks "false")
;;                                  (autonumber "AMS")
;;                                  (indent "0em")
;;                                  (multlinewidth "85%")
;;                                  (tagindent ".8em")
;;                                  (tagside "right"))
;;       ;; org-html-mathjax-template
;;       )

;; ;; CDLaTeX minor mode to speed up math input.
;; (autoload 'cdlatex-mode "cdlatex" nil)
;; ;; enable `org-cdlatex-mode' for all org files
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

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
                '(:time "1h" :period "2h" :duration 10
                        :actions (-notify/window -ding))
                '(:time "1d" :period "4h" :duration 10
                        :actions (-notify/window -ding))
                ;; '(:time "3d" :period "4h" :duration 5
                ;;         :actions -notify/window)
                ;; '(:time "5d" :period "6d" :duration 3
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

(require 'org-contacts)

(setq org-contacts-files '("~/Org/Contacts/Contacts.org")
      ;; org-contacts-db
      )

(setq org-contacts-enable-completion t
      ;; org-contacts-complete-functions '(org-contacts-complete-group
      ;;                                   org-contacts-complete-tags-props
      ;;                                   org-contacts-complete-name)
      )

;; (setq org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY")

;; <icon>
(setq org-contacts-icon-size 32
      org-contacts-icon-property "ICON"
      org-contacts-icon-use-gravatar t
      )

(unless (boundp 'my-org-contacts-prefix-map)
  (define-prefix-command 'my-org-contacts-prefix-map))
(define-key my-org-prefix-map (kbd "b") 'my-org-contacts-prefix-map)

(define-key my-org-contacts-prefix-map (kbd "b") 'org-contacts)
(define-key my-org-contacts-prefix-map (kbd "p") 'org-contacts-at-point)

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

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c o s") 'org-screenshot-take)))

;; TODO: Emacs need to prompt user to get the screenshot filename to describe this screenshot.
;; 1. improve org-screenshot source code.
;; 2. add custom function. or defadvice.
;;
;;
;;;_* org-download -- Drag and drop images to Emacs org-mode.
;;
;;; Usage:
;;
;;*This extension facilitates moving images from point A to point B.*
;;
;;* Point A (the source) can be:
;;
;;- An image inside your browser that you can drag to Emacs.
;;- An image on your file system that you can drag to Emacs.
;;- A local or remote image address in ~kill-ring~. Use the ~org-download-yank~ command for this. Remember that you can use "=[0 w]=" in dired to get an address.
;;- An screenshot taken using /gnome-screenshot/ or /scrot/ or /gm/. Use the ~org-download-screenshot~ command for this. Customize the backend with ~org-download-screenshot-method~.
;;
;;* Point B (the target) is an Emacs org-mode buffer where the inline link will be inserted. Several customization options will determine where exactly on the file system the file will be stored.
;;
;;They are: ~org-download-method~:
;;
;;1) 'attach => use org-mode attachment machinery
;;
;;2) 'directory => construct the directory in two stages:
;;
;;   1. first part of the folder name is:
;;      - either "." (current folder)
;;
;;      - or org-download-image-dir (if it's not nil).
;;
;;      - ~org-download-image-dir~ becomes buffer-local when set, so each file can customize this value, e.g with:
;;
;;        #+BEGIN_EXAMPLE
;;        -*- mode: Org; org-download-image-dir: "~/Pictures/foo"; -*-
;;        #+END_EXAMPLE
;;
;;        To set it for all files at once, use this:
;;        
;;        #+BEGIN_SRC emacs-lisp
;;        (setq-default org-download-image-dir "~/Pictures/foo")
;;        #+END_SRC
;;
;;   2. second part is:
;;
;;       - ~org-download-heading-lvl~ is ~nil~ => ""
;;
;;       - ~org-download-heading-lvl~ is ~n~ => the name of current heading with level n.
;;
;;         Level count starts with 0, i.e. * is 0, ** is 1, *** is 2
;;         etc. org-download-heading-lvl becomes buffer-local when set, so each
;;         file can customize this value, e.g with:
;;
;;         #+BEGIN_EXAMPLE
;;         -*- mode: Org; org-download-heading-lvl: nil; -*-
;;         #+END_EXAMPLE
;;
;;
;;
;;~org-download-timestamp~: optionally add a timestamp to the file name.
;;
;;Customize ~org-download-backend~ to choose between ~url-retrieve~ (the /default/) or ~wget~ or ~curl~.
;;

(require 'org-download)

(setq org-download-screenshot-method "scrot -s %s"
      org-download-method 'dictionary ; 'attach, 'dictionary,
      org-download-backend t ; url-retrieve, wget, curl.
      ;; org-download-heading-lvl
      ;; org-download-timestamp "_%Y-%m-%d_%H:%M:%S"
      )

(unless (boundp 'my-org-download-map)
  (define-prefix-command 'my-org-download-map))
(define-key my-org-prefix-map (kbd "d") 'my-org-download-map)

(define-key my-org-download-map (kbd "i") 'org-download-image)
(define-key my-org-download-map (kbd "s") 'org-download-screenshot)
(define-key my-org-download-map (kbd "y") 'org-download-yank)
(define-key my-org-download-map (kbd "d") 'org-download-delete)

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

(setq org-pomodoro-audio-player "/usr/bin/mplayer"
      org-pomodoro-play-sounds t
      org-pomodoro-play-start-sound t
      org-pomodoro-play-ticking-sounds nil
      ;; org-pomodoro-ticking-sound
      org-pomodoro-ticking-sound-args "-volume 50" ; adjust ticking sound volume
      org-pomodoro-format "Pomodoro~%s" ; mode-line string
      )

(define-key my-org-prefix-map (kbd "p") 'org-pomodoro)

;; start another pomodoro automatically upon a break end.
(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (interactive)
            (org-pomodoro '(16)) ; double prefix [C-u C-u]
            ))

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

;;;_* org-mu4e

;;; Usage:
;;
;; - `org-mu4e-open' :: open the mu4e message (for paths starting with 'msgid:')
;;                      or run the query (for paths starting with 'query:').

(if (not (fboundp 'org-mu4e-compose-org-mode))
    (require 'org-mu4e)                 ; this will setup org links.
  )

;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;; (org-add-link-type "msgid" 'org-email-open)
;; (org-add-link-type "query" 'org-email-open)

;;;_* orgit

;; This package defines several Org link types which can be used to
;; link to certain Magit buffers.
;;
;;    orgit:/path/to/repo/           links to a `magit-status' buffer
;;    orgit-log:/path/to/repo/::REV  links to a `magit-log' buffer
;;    orgit-rev:/path/to/repo/::REV  links to a `magit-commit' buffer

;; Such links can be stored from corresponding Magit buffers using
;; the command `org-store-link'.

;; When an Org file containing such links is exported, then the url of
;; the remote configured with `orgit-remote' is used to generate a web
;; url according to `orgit-export-alist'.  That webpage should present
;; Package-Version: 20150525.1140
;; approximately the same information as the Magit buffer would.

;; Both the remote to be considered the public remote, as well as the
;; actual web urls can be defined in individual repositories using Git
;; variables.

;; To use a remote different from `orgit-remote' but still use
;; `orgit-export-alist' to generate the web urls, use:
;;
;;    git config orgit.remote REMOTE-NAME

;; To explicitly define the web urls, use something like:
;;
;;    git config orgit.status http://example.com/repo/overview
;;    git config orgit.log http://example.com/repo/history/%r
;;    git config orgit.rev http://example.com/repo/revision/%r


(require 'orgit)

;; (add-to-list orgit-export-alist ')

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

(if (featurep 'helm)
    (define-key my-org-prefix-map (kbd "c") 'helm-org-capture-templates)
  (define-key my-org-prefix-map (kbd "c") 'org-capture)
  (define-key org-mode-map (kbd "C-c c") 'org-capture))

(define-key my-org-prefix-map (kbd "e")
  (defun my-org-element-at-point ()
    (interactive)
    ;; FIXME: minibuffer does not show result.
    (org-element-at-point)))

(unless (boundp 'my-org-heading-prefix-map)
  (define-prefix-command 'my-org-heading-prefix-map))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'my-org-heading-prefix-map)
            (define-key my-org-heading-prefix-map (kbd "h") 'helm-org-in-buffer-headings)
            (define-key my-org-heading-prefix-map (kbd "a") 'helm-org-agenda-files-headings)
            ))

(unless (boundp 'my-org-agenda-prefix-map)
  (define-prefix-command 'my-org-agenda-prefix-map))
(define-key my-org-prefix-map (kbd "M-a") 'my-org-agenda-prefix-map)

(define-key my-org-prefix-map (kbd "a")
  (defun my-open-org-agenda ()
    (interactive)
    ;; TODO:
    ;; (if (memq (current-buffer) (frame-bufs-buffer-list (selected-frame)))
    ;;     (switch to that window)
    ;;   (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t))
    (my-func/open-and-switch-to-buffer 'org-agenda-list "*Org Agenda*" t)
    ))

(define-key my-org-agenda-prefix-map (kbd "a") 'my-open-org-agenda)
(define-key my-org-agenda-prefix-map (kbd "A") 'org-agenda)
(define-key my-org-agenda-prefix-map (kbd "t") 'org-todo-list) ; prefix [C-u] to prompt keyword for todo list
(define-key org-mode-map (kbd "C-c o M-a T") 'org-timeline) ; Show a time-sorted view of the entries in the current org file.

(unless (boundp 'my-org-link-prefix-map)
  (define-prefix-command 'my-org-link-prefix-map))
(define-key my-org-prefix-map (kbd "M-l") 'my-org-link-prefix-map)

(define-key my-org-link-prefix-map (kbd "L") 'org-insert-link-global)
(define-key my-org-link-prefix-map (kbd "l") 'org-store-link)
(define-key my-org-link-prefix-map (kbd "o") 'org-open-at-point-global)


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


(unless (boundp 'my-org-password-prefix)
  (define-prefix-command 'my-org-password-prefix))
(define-key my-org-prefix-map (kbd "P") 'my-org-password-prefix)


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

(define-key my-org-prefix-map (kbd "P") 'my-org-passwords-search)


;;; [ org-password-manager ]

;;; Usage
;;
;; - [C-u] [C-c C-p u] :: get/query username.
;; - [C-u] [C-c C-p p] :: get/query password.
;; - [C-u] [C-c C-p g] :: generate password.

(require 'org-password-manager)

;; (add-hook 'org-mode-hook 'org-password-manager-key-bindings)

;; (setq org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 25 1")

(define-key my-org-password-prefix (kbd "u") 'org-password-manager-get-username)
(define-key my-org-password-prefix (kbd "p") 'org-password-manager-get-password)
(define-key my-org-password-prefix (kbd "s") 'org-password-manager-get-property)
(define-key my-org-password-prefix (kbd "g") 'org-password-manager-generate-password)



;;;_*
(provide 'init-my-tool-org-mode)

;;; init-my-tool-org-mode.el ends here
