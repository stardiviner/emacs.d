;;; init-my-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; collapse modeline
;; variable: `mode-line-format'
;; use `setq-default` to set it for /all/ modes
;;
;; [C-u C-\ greek RET] then [C-\] to write Greek characters.
;; - [C-h v major-mode] for current buffer major mode.
;; - [C-h v minor-mode-alist] for current buffer minor modes list.

;; load necessary package which will be used later.
(require 'window-number)
(require 'vc)
(require 'vc-git)

;; nyan-mode
;; Nyan Mode is an analog indicator of your position in the buffer. The Cat
;; should go from left to right in your mode-line, as you move your point from
;; 0% to 100%.
(require 'nyan-mode)
(setq nyan-animate-nyancat t
      nyan-wavy-trail nil
      nyan-animation-frame-interval 0.2
      nyan-bar-length 15
      nyan-cat-face-number 1
      )

;; spinner
;; (spinner-start 'vertical-breathing 10)
;; (spinner-start 'minibox)
;; (spinner-start 'moon)
;; (spinner-start 'triangle)


;;; Tips:
;;
;; - (:eval (propertize 'face '(:foreground "green")))
;;
;;   You need to quote those symbols because `:eval' will try to evaluate them.
;;
;; - [C-x C-e] :: evaluate the sexp which in (:eval ...), toggle `debug-on-error' to show errors.

(setq-default
 mode-line-format
 (quote
  (
   ;; (:propertize "%e"
   ;;              face (:foreground "red" :inverse-video nil))

   ;; (:propertize " 暗月:> "
   ;;              face (:foreground "#444444" :background "black")
   ;;              help-echo "九州 ❯ 羽传说 ❯ 向异翅")

   ;; (:propertize " ㊊㊛ "
   ;;              face (:foreground "deep pink" :height 120)
   ;;              help-echo "Female & Lesbian")

   ;; emacsclient indicator
   (:eval (if (frame-parameter nil 'client)
              (propertize "あ"
                          'face '(:foreground "#333333" :background "yellow" :weight 'bold :height 120)
                          'help-echo "emacsclient frame")))
   
   ;; window-number
   (:propertize (:eval (concat "[" (number-to-string (window-number)) "]"))
                face (:foreground "red" :weight 'bold
                                  ))

   ;; anzu
   (:propertize (:eval (anzu--update-mode-line))
                face (:foreground "cyan" :weight 'bold
                                  :box '(:color "deep pink")))
   
   ;; multiple-cursors (mc/)
   (:eval (if (and 'mc/fake-cursor-p (> (mc/num-cursors) 1)) ; (if 'mc/fake-cursor-p ...)
              (propertize (format "[%d]" (mc/num-cursors)) ; `mc/mode-line'
                          'face '(:foreground "firebrick" :background "black")
                          'help-echo "multiple-cursors")))

   ;; input method
   (:eval (if current-input-method-title ; `set-input-method'
              (propertize current-input-method-title
                          'face '(:foreground "dodger blue"))))

   ;; mule info
   (:propertize mode-line-mule-info
                face (:foreground "dark gray"))   ; U:[*--]
   
   ;; Buffer status
   (:eval
    (cond
     (buffer-read-only
      (propertize " ⚑ "
                  'face '(:foreground "red" :weight 'bold)
                  'help-echo "buffer is read-only!!!"))
     ;; (overwrite-mode
     ;;  (propertize "??"))
     ((buffer-modified-p)
      (propertize " ☡ "
                  'face '(:foreground "orange")
                  'help-echo "buffer modified."))
     ;; FIXME:
     ;; (mode-line-remote
     ;;  (propertize " R "
     ;;              'face '(:foreground "dark red")
     ;;              'help-echo "remote buffer")
     ;;  )
     ))

   ;; eldoc
   ;; `eldoc-show-in-mode-line'
   ;; FIXME:
   ;; (:eval (unless eldoc-mode-line-string
   ;;          (propertize eldoc-mode-line-string
   ;;                      ;; 'face '(:foreground "cyan")
   ;;                      )))
   
   ;; rbenv & rvm
   (:eval
    (if (memq (buffer-local-value 'major-mode (current-buffer))
              '(ruby-mode enh-ruby-mode))
        (progn
          (list
           (propertize " ("
                       'face '(:foreground "#444444"))
           (propertize "Ruby: "
                       'face '(:family "Segoe Print"
                                       :height 80
                                       :foreground "red2"))
           (propertize (rbenv--active-ruby-version) ; `rbenv--modestring'
                       'face '(:foreground "cyan" :height 70)
                       'help-echo '(concat "\nCurrent Ruby version: " (rbenv--active-ruby-version)
                                           "\nmouse-1: switch Ruby version menu")
                       ;; TODO: 'local-map
                       )
           (propertize ")"
                       'face '(:foreground "#444444"))
           )
          )
      ))
   

   ;; pyvenv `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
   (:eval
    (if (memq (buffer-local-value 'major-mode (current-buffer))
              '(python-mode))
        (progn
          (list
           (propertize pyvenv-virtual-env-name ; FIXME:
                       'face '(:foreground "orange" :height 70))))
      ;; (propertize "Not Python"
      ;;             'face '(:foreground "orange"))
      ))
   
   ;; VCS - Git, SVN, CVS,
   
   ;; branch
   (:eval
    (if vc-mode
        (progn
          (list
           (propertize " ⭠"
                       'face '(:foreground "sky blue"))
           `(vc-mode vc-mode)
           ))))

   ;; TODO: git repo file status
   ;; - added :: ✚
   ;; - modified :: ∓
   ;; - deleted :: ✖
   ;; - renamed :: ➜
   ;; - unmerged :: ⭠ ⌥
   ;; - untracked :: ⚡
   ;; - branch :: ⭠
   ;;
   ;; (vc-mode (:eval (propertize vc-mode
   ;;                             'face (pcase (vc-state buffer-file-truename)
   ;;                                     (`up-to-date '(:foreground "green"))
   ;;                                     (`edited nil)
   ;;                                     (_ '(:foreground "red"))))))
   ;;
   ;; (vc-mode (:eval (propertize vc-mode 'face
   ;;   (pcase (vc-state buffer-file-truename)
   ;;     (`up-to-date 'mode-line-important)
   ;;     (`edited                      nil)
   ;;     (_             'mode-line-warning)))))

   ;; FIXME:
   ;; git state mark (from `git-emacs')
   ;; (:propertize (:eval (git--state-mark-modeline)))
   ;;
   ;;                 (:propertize (:eval
   ;;                               (git--state-mark-modeline
   ;;                                #("    " 0 4
   ;;                                  (display
   ;;                                   (image :type xpm
   ;;                                          :data
   ;;                                          "/* XPM */
   ;; static char * data[] = {
   ;; \"14 7 3 1\",
   ;; \" 	c None\",
   ;; \"+	c #202020\",
   ;; \".	c %s\",
   ;; \"      +++     \",
   ;; \"     +...+    \",
   ;; \"    +.....+   \",
   ;; \"    +.....+   \",
   ;; \"    +.....+   \",
   ;; \"     +...+    \",
   ;; \"      +++     \"};"
   ;;                                          :ascent center)
   ;;                                   ;; FIXME: help-echo (git--state-mark-tooltip stat)
   ;;                                   )))))
   

   ;; the buffer name; the filename as a tool tip
   ;; mode-line-client
   ;; mode-line-remote
   ;; mode-line-frame-identification
   ;; mode-line-buffer-identification
   (:propertize " ["
                face (:foreground "cyan"))
   (:propertize "%b"
                face (:foreground "white"
                                  :height 75)
                help-echo (buffer-file-name))
   (:propertize "]"
                face (:foreground "cyan"))

   ;; process
   (:eval
    (if mode-line-process
        (progn
          (list
           (propertize "  ◌"
                       'face '(:foreground "cyan" :weight 'bold :height 120)
                       'help-echo "buffer-process")
           ;; (propertize mode-line-process ; FIXME: this does not work, it is a symbol.
           ;;             'face '(:foreground "tomato")
           ;;             'help-echo "buffer-process")
           )
          )
      ))

   ;; (:propertize (:eval (if mode-line-process "  ◌"))
   ;;              face (:foreground "cyan"))
   ;; (:propertize (:eval (if mode-line-process mode-line-process))
   ;;              face (:foreground "tomato"))
   

   ;; notifications
   ;; ERC
   ;; TODO [#e,i]

   ;; mmm-mode
   ;; FIXME:
   ;; (:propertize (:eval (mmm-format-string))

   ;; ;; the major mode of the current buffer.
   ;; (:propertize " 〖"
   ;;              face (:foreground "orange red"))
   ;; (:propertize "%m"
   ;;              face (:foreground "green yellow"
   ;;                                :family "Comic Sans MS" :weight 'bold ; :height 100
   ;;                                )
   ;;              ;; FIXME:
   ;;              help-echo (minor-mode-alist)
   ;;              )
   ;; ;; ;; list of minor modes
   ;; ;; (:propertize "|")
   ;; ;; (:propertize minor-mode-alist
   ;; ;;              face (:foreground "dim gray"))
   ;; (:propertize "〗 "
   ;;              face (:foreground "orange"))

   ;; --------------------------- right align ----------------------------------
   
   ;; Email

   ;; newsticker RSS new feeds.
   ;; (:propertize (:eval (let ((unread (or (newsticker--stat-num-items-total 'new) 0)))
   ;;                       (when (> unread 0)
   ;;                         (format "RSS: %d" unread)
   ;;                         )))
   ;;              face (:foreground "green")
   ;;              help-echo (format "You have %d unread RSS items! [C-c r]" unread)
   ;;              mouse-face 'mode-line-highlight)

   ;; ;; add the time, with the date and the emacs uptime in the tool-tip
   ;; (:propertize (:eval (format-time-string "%H:%M"))
   ;;              face (:foreground "white")
   ;;              help-echo (concat (format-time-string "%c; ")
   ;;                                (emacs-uptime "Uptime: %D, %z%2h:%.2m")))
   
   ;; fill with ' '.
   ;; (:propertize "% ")

   ;; Org-mode clock
   ;; TODO: change into (:eval ...)
   (:eval
    (if '(t org-mode-line-string) ; FIXME:
        (propertize (t org-mode-line-string)
                    'face '(:foreground "cyan" :weight 'bold)
                    'help-echo "Org-mode clock"))
    )

   
   (:propertize mode-line-end-spaces)
   )))

;; update org-clock timer in mode-line after `org-clock-out-hook'.
;; fix org-clock timer does not disappear after clock out.
(add-hook 'org-clock-out-hook
          '(lambda ()
             (setq org-mode-line-string nil)
             (force-mode-line-update)))

;;; mode-line right align (which replace `mode-line-end-spaces' to implement the right alignment.)
;;
(display-time-mode t)
(setq global-mode-string (remove 'display-time-string global-mode-string))
(setq mode-line-end-spaces
      (list (propertize " " 'display '(space :align-to (- right 55)))
            ;;; you can custom here (add right aligned things here)

            ;; TODO: mu4e maildir
            '(:propertize (:eval
                           (let ((my-mu4e-maildir-name (file-name-base mu4e-maildir)))
                             (if (string-match "mu4e-.*" "%b")
                                 (format "[%s]" my-mu4e-maildir-name))))
                          face (:foreground "white" :weight 'bold))

            ;; nyan-mode
            '(:eval (when nyan-mode (list (nyan-create))))

            ;; FIXME: spinner
            ;; Let spinner support to be used in custom mode-line as a function.
            ;; '(:eval (spinner-start 'minibox))
            ;;
            ;; '(:propertize  (:eval (spinner-start 'minibox))   ; 'spinner--mode-line-construct
            ;;                :face (:foreground "dark gray"))

            ;; flycheck
            '(:propertize (:eval (flycheck-mode-line-status-text))
                          face (:foreground "orange" :background nil
                                            :height 80))
            
            ;; line and column number, relative position
            ;; mode-line-position
            ;; '%02' to set to 2 chars at least; prevents flicking
            '(:propertize " (%02l,%02c)_%03p"
                          face (:foreground "dark gray" :height 80)
                          ;; (:eval (if (>= (current-column) 80)
                          ;;            face (:foreground "red" :weight 'bold)))
                          )

            ;; relative position, size of file
            ;; '(:propertize " [%p,%I] ")
            
            '(:propertize " ["
                          face (:foreground "red" :weight 'bold))
            '(:propertize "wg:"
                          face (:foreground "dim gray"))
            ;; workgroups2
            '(:propertize (:eval ; `wg-mode-line-display-on'
                           (wg-mode-line-string))
                          face (:foreground "yellow" :height 75))
            '(:propertize " § "
                          face (:foreground "red"))
            '(:propertize "P:"
                          face (:foreground "dim gray"))
            ;; projectile
            '(:propertize projectile-mode-line
                          face (:foreground "cyan" :height 75))
            
            '(:propertize "] "
                          face (:foreground "red" :weight 'bold))

            ;; the major mode of the current buffer.
            ;; `mode-name', `mode-line-modes', `minor-mode-alist'
            '(:propertize "%m"
                          face (:foreground "green yellow"
                                            :family "Comic Sans MS" :weight 'bold :height 80
                                            )
                          ;; help-echo (minor-mode-alist)
                          )
            
            ;; '(:propertize "{/Emacs/}"
            ;;               face (:foreground "yellow")
            ;;               help-echo "神之编辑器")
            
            ;; '(:propertize " /铁甲依然在 !/ "
            ;;               face (:foreground "cyan")
            ;;               help-echo "我不要这样生，也不要那样死！\n\n 人的一生是如此的的短暂，而你还有很多的事情,很多的梦想 需要曲实现。\n我不想那样生存，也不想就 这样死去，所以，我要拼命去实现我的想法。")
            
            ;; 'display-time-string
            ))


;;; change minor mode name
;; (cl-pushnew '(workgroups-mode " wg") minor-mode-alist :test 'equal)

(defvar mode-line-cleaner-alist
  '((auto-complete-mode . " ⓐ") ; AC (auto-complete)
    (company-mode . "ⓐ") ; company-mode
    (yas-minor-mode . "ⓨ") ; yas (yasnippet)
    (helm-mode . "") ; Helm (helm),
    (paredit-mode . "ⓟ") ; Paredit (paredit)
    (paredit-everywhere-mode . "ⓟ") ; Par-
    (eldoc-mode . "") ; ElDoc (ElDoc)
    ;; minor modes
    (autopair-mode . "") ; pair (auto-pair)
    (auto-indent-mode . "")
    (auto-indent-minor-mode . "") ; AI (auto-indent)
    (indent-guide-mode . "ⓘ")
    (aggressive-indent-mode . "Ⓘ")
    (highlight-indentation-mode . "") ; ||, ▸
    (highlight-indentation-current-column-mode . "") ; |
    (visual-line-mode . "") ; vl (visual-line)
    (global-visual-line-mode . "") ; vl (visual-line)
    (smartparens-mode . "")
    (hs-minor-mode . "") ; hs (hideshow)
    (abbrev-mode . "") ; (abbrev)
    (defining-kbd-macro . " Def:macro") ; Def
    (undo-tree-mode . "") ; Undo-tree (undo-tree)
    (outline-minor-mode . "") ; Outl (outline)
    (global-whitespace-mode . "ⓦ") ; WS (global-whitespace-mode)
    (whitespace-mode . "ⓦ") ; ws (whitespace-mode)
    (global-whitespace-newline-mode . "") ; NL (global-whitespace-newline-mode)
    (whitespace-newline-mode . "") ; nl (whitespace-newline-mode)
    (whitespace-cleanup-mode . "") ; WSC
    (auto-fill-mode . "") ; Fill (auto-fill-mode), A/Fill
    (auto-fill-function . "") ; Fill (auto-fill-function), A/Fill
    (anzu-mode . "") ; Anzu
    (rainbow-mode . "") ; Rbow (rainbow-mode)
    (rainbow-delimiters-mode . "") ; (rainbow-delimiters-mode)
    (auto-capitalize . "") ; Cap, (auto-capitalize), A/Cap
    (ispell-minor-mode . "") ; Spell (ispell-minor-mode)
    (flyspell-mode . "ⓢ") ; Fly (flyspell)
    (flycheck-mode . flycheck-mode-line) ; FlyC
    (flymake-mode . 'flymake-mode-line) ; FlyM
    (hi-lock-mode . "") ; syntax highlight
    (compilation-minor-mode . " Compilation") ; Compilation
    (compilation-shell-minor-mode . " Shell-Compile") ; Shell-Compile
    (compilation-in-progress . " Compiling") ; Compiling
    (next-error-follow-minor-mode . "") ; Fol
    (visible-mode . "") ; Vis (visible-mode)
    (window-number-mode . "") ; window-number
    (workgroups-mode . "") ; wg
    (fic-mode . "") ; FIC (fic-mode)
    ;; Project
    (projectile-mode . projectile-mode-line)        ; " Projectile" (projectile)
    ;; major modes
    ;; Tools
    ;; Org
    (org-mode . "Org") ; Org (org-mode)
    (org-indent-mode . "") ; Ind (org-indent-mode)
    (org-capture-mode . "Org/Capture") ; Rem;
    (org-cdlatex-mode . "Org/OCDL") ; OCDL;
    (orgstruct-mode . "Org/Struct") ; OrgStruct;
    (org-agenda-mode . "*Org Agenda*")
    ;; VCS
    (magit-auto-revert-mode . "") ; MRev
    ;; Programming Languages modes
    (lisp-interaction-mode . "λ:>")
    (lisp-mode . "λ") ; Lisp mode
    (emacs-lisp-mode . "Emacs Lisp") ; Emacs-Lisp (emacs-lisp)
    (elisp-slime-nav-mode . "") ; SliNav
    (sh-mode . "#!") ; shell-script (Bash)
    (ruby-mode . "Ruby")
    (enh-ruby-mode . "EnRuby") ; EnhRuby (Enhanced-Ruby-Mode)
    (ruby-electric-mode . " Ruby/Elec")
    (ruby-block-mode . " Ruby/Block")
    (ruby-test-mode . " Ruby/Test")
    (rspec-mode . " RSpec")
    (robe-mode . "") ; robe
    (rinari-minor-mode . " Rinari") ; Rin
    (ruby-capistrano-minor-mode . " Capstrano") ; capstrano
    (ruby-compilation-minor-mode . " Ruby:comp") ; ruby:comp
    (inf-ruby-minor-mode . "")
    (python-mode . "Python")
    (c-mode . "C")
    (c++-mode . "C++")
    (go-mode . "Go")
    (lua-mode . "Lua")
    (rust-mode . "Rust")
    (clojure-mode . "Clojure")
    (ocaml-mode . "OCaml") ; 🐪
    (scala-mode . "Scala")
    (ess-transcript-minor-mode . "") ; ESStr
    (ess-listing-minor-mode . "") ; ESSlst
    (noweb-mode . "Noweb") ; Noweb
    (noweb-font-lock-mode . "") ; NWFL
    (html-mode . "HTML")
    (zencoding-mode . "Zen") ; " Zen" (zencoding-mode)
    (css-mode . "CSS")
    (js-mode . "JavaScript")
    (js2-mode . "JavaScript(2)")
    (js2-minor-mode . "JavaScript2")
    (js3-mode . "JavaScript(3)")
    (inferior-js-keys-mode . "") ; InfJS
    ;; TODO (haml-mode . "Haml")
    ))

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;; (setq mode-line-in-non-selected-windows t)

;;; 1. color style
;; (set-face-attribute 'mode-line nil
;;                     :inverse-video nil
;;                     :foreground "white" :background "#004A5D"
;;                     :box '(:color "cyan" :line-width 1 :style nil)
;;                     :family "DejaVu Sans Mono"
;;                     :height 90
;;                     )
;; (set-face-attribute 'mode-line-inactive nil
;;                     :inverse-video nil
;;                     :foreground "#444444" :background "black" ; :background "#242424"
;;                     :family "DejaVu Sans Mono"
;;                     :box '(:color "slate blue" :line-width -1 :style nil)
;;                     :height 90
;;                     )

;;; 2. darker style
(set-face-attribute 'mode-line nil
                    :inverse-video nil
                    :foreground "white"
                    ;; :background (color-darken-name (face-background 'default) 3)
                    :background "#333333"
                    :box '(:color "tomato" :line-width 1 :style nil)
                    ;; :box '(:color "slate blue" :line-width 1 :style nil)
                    ;; :box '(:color "forest green" :line-width 1 :style nil)
                    :family "DejaVu Sans Mono"
                    :height 100
                    )
(set-face-attribute 'mode-line-inactive nil
                    :inverse-video nil
                    :foreground "#222222"
                    :background (color-darken-name (face-background 'default) 3)
                    :family "DejaVu Sans Mono"
                    :box '(:color "black" :line-width 1 :style nil)
                    :height 100
                    )


;;; [ display-time ]

;; (display-time-mode 1)

;; (setq display-time-interval 60)
;; (setq display-time-24hr-format nil)
;; (setq display-time-format nil)
;; (setq display-time-day-and-date nil)
;; ;;; event
;; (display-time-event-handler)
;; ;;; Mail
;; (setq display-time-use-mail-icon t)
;; (setq display-time-mail-function nil)
;; (setq display-time-mail-file nil)
;; (setq display-time-mail-directory "~/Mails/INBOX/new/")
;; (display-time-mail-check-directory)
;; ;; (display-time-mail-check-directory)
;; ;;; load-average
;; (setq display-time-default-load-average 0)
;; (setq display-time-load-average-threshold 0.5)



;;; [ diminish ]

;;; Usage:
;; Diminishing Major Modes
;; (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Emacs Lisp"))) ; emacs lisp


;;; [ mode-icons ]

;; (require 'mode-icons)
;;
;; (mode-icons-mode)



(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
