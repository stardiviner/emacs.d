;;; init-helm.el --- init Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] --- (incremental completion and selection narrowing framework)

;;; Usage:
;;;
;;; Helm needs you to remember only a few binding unlike all other Emacs
;;; applications. Thus, all bindings are auto documented.
;;
;; So when helm starts, you have to remember:
;; - [TAB] -- Access to action menu.
;; - [C-j/z] -- Use persistent actions with (open document/and execute persistent action).
;; - [M-<space>] -- Mark candidate.
;;
;; Helm prefix key
;; - [C-x c] :: for "helm version" of other functions.
;;
;; So there are three bindings to remember and they are also documented in
;; mode-line. For more, hitting:
;; - [C-h m]
;; while in helm session will show you all other bindings.
;; NOTE: Some helm commands have a special keymap, you can access infos
;; on these keymap with [C-c ?], it should be documented in the mode-line.
;;
;; get helm help in heml minor mode:
;;  - [C-h m]
;;  - M-x helm-mini
;; wildcard match
;;  - ~/_esk (here `_' is a space)
;;
;; [C-x C-f] -- (helm-find-files)
;;
;; Find Files or url: ~/
;; That show all ~/ directory.
;;
;; Find Files or url: ~/des
;; will show all what begin with "des"
;;
;; Find Files or url: ~/ esk
;; (Notice the space after ~/) will show all what contain esk.
;;
;; Find Files or url: ~/ el$
;; Will show all what finish with el
;;
;; When a row is selected, C-z performs the default action, which is different depending on the context.
;; When you are on a file, C-z will show only this file-name in the helm
;; buffer. On a directory, C-z will step down into this directory to continue
;; searching in it. On a symlink, C-z will expand to the true name of symlink
;; (moving your mouse cursor over a symlink will also show the true name).
;;
;; Take advantage of the second, third and 4th actions in helm. Instead of opening action menu with TAB, just hit:
;; C-e for 2th action, C-j for 3th action

; (require 'helm)
(require 'helm-config)

;; (require 'helm-grep)
(require 'helm-org)

(require 'helm-misc)

(helm-mode 1) ; enable Helm mode initially.
(diminish 'helm-mode)

(setq helm-command-prefix-key "C-x c" ; for `helm-command-prefix'.
      helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-buffer-not-found
                                  ;; helm-source-bookmarks
                                  helm-source-pp-bookmarks
                                  ;; helm-source-my-apps ; my custom source
                                  ;; helm-source-my-locations ; my custom source
                                  ;; helm-source-recentf
                                  ;; helm-source-filter
                                  ;; helm-source-session
                                  ;; helm-source-occur
                                  ;; helm-source-grep
                                  ;; helm-source-regexp
                                  ;; helm-source-gtags-tags
                                  ;; helm-source-time-world
                                  ;; helm-source-tracker-search
                                  ;; helm-source-wikipedia-suggest
                                  ;; helm-source-google-suggest
                                  ;; projectile
                                  ;; helm-source-projectile-buffer
                                  ;; helm-source-projectile-recentf-list
                                  ;; helm-source-projectile-projects
                                  ;; TODO: Org-mode
                                  ;; helm-org-agenda-files-headings
                                  ;; helm-org-in-buffer-headings
                                  ;; helm-source-org-headings-for-files
                                  )
      )

(defvar helm-source-my-apps
  '((name . "My Daily Apps")
    (candidates . (("Agenda" . my-open-org-agenda)
                   ("Mail" . mu4e)
                   ("Blog" . jekyll-draft-post)
                   ("Calendar" . (lambda () (browse-url "https://www.google.com/calendar/render")))
                   ("RSS" . elfeed)
                   ("Web Browser" . eww)))
    (action . (("Open" . (lambda (x) (funcall x)))))
    ))

(defvar helm-source-my-locations
  '((name . "My Frequently File Locations")
    (candidates . (("Linux" . "~/Org/Wiki/Computer/Systems/Linux/Linux.org")
                   ("Emacs" . "~/Org/Wiki/Computer/Programming/Emacs/Emacs.org")
                   ("Programming" . "~/Org/Wiki/Computer/Programming/Programming.org")
                   ("Implement" . "~/Org/Wiki/Computer/Programming/Implements/Implements.org")
                   ("Languages" . "~/Org/Wiki/Computer/Programming/Programming Languages/Programming Languages.org")
                   ("Ruby" . "~/Org/Wiki/Computer/Programming/Programming Languages/Ruby/Ruby.org")
                   ("Lisp" . "~/Org/Wiki/Computer/Programming/Programming Languages/Lisp/Lisp.org")))
    (action . (("Open" . (lambda (x) (find-file x)))))))


;;; work with ido and helm together.
;; If you like ido for some commands and helm for other commands, you should not
;; enable ido-mode, instead customize helm-completing-read-handlers-alist; For
;; example, you want ido-mode for find-file-read-only and helm-mode for
;; find-file: 1) In your config you turn on helm-mode. 2) In customize-group
;; helm-mode add to helm-completing-read-handlers-alist find-file-read-only as
;; key and ido as value. In elisp it looks like this:
;; (find-file-read-only . ido)
;;
;; Now you want find-alternate-file to not use ido and to not use helm, only the
;; vanilla emacs completion: Add an entry to helm-completing-read-handlers-alist
;; like this: (find-alternate-file . nil)

(setq helm-completing-read-handlers-alist
      '((describe-function . helm-completing-read-symbols)
        (describe-variable . helm-completing-read-symbols)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file)
        (tmm-menubar)
        ;; customize
        (org-insert-link . ido)         ; NOTE: temp solution for Helm org-insert-link error.
        ))


;; this global keybinding [M-x] will conflict with {[C-u M-x align-regexp] on select region text.}
;; But you can press [M-x C-u align-regexp RET].
(setq helm-M-x-fuzzy-match nil)
(global-set-key (kbd "M-x") 'helm-M-x)
;; If you prefer the helm version of the file finder, you can bind it to C-x C-f
;; to replace the standard find-file:
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
;; (define-key helm-command-prefix (kbd "o") 'helm-occur)
(global-set-key (kbd "C-h a") 'helm-apropos) ; replace default `apropos-command'.
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-command-prefix (kbd "M-:") 'helm-eval-expression-with-eldoc)
;; Similar to helm-eshell-history, but is used for [M-x shell].
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(define-key helm-map (kbd "<tab>") 'helm-select-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal.
(define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<RET>") 'helm-maybe-exit-minibuffer)

;; NOTE: this cause helm-dash open menu candidate error.
;; (define-key helm-map (kbd "<return>") 'helm-confirm-and-exit-minibuffer)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

;;; `customize-group helm' & `custom-apropos-faces helm'

;;; FIXME: remove this temp fix of using `after-init-hook'.
(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'helm-match nil
                                ;; 1. dim already matched, leave eye on the un-matched strings.
                                :foreground "dim gray" :background (color-darken-name (face-background 'default) 5)
                                ;; 2. highlight/underline already matched
                                ;; :foreground "white"
                                ;; :underline t
                                ;; :weight 'normal
                                )
            (set-face-attribute 'helm-selection nil
                                ;; 1. different font
                                ;; :family "Comic Sans MS" :weight 'normal :height 1.0 :slant 'italic
                                ;; :box nil
                                ;; 2. box selected
                                ;; :background "#004A5D" :foreground "white"
                                ;; :box '(:color "cyan" :line-width -1)
                                ;; :underline nil
                                ;; 3. different highlight color.
                                ;; :background "yellow" :foreground "orange red"
                                ;; :weight 'bold
                                ;; 4. darker background percent 5%
                                :inherit nil
                                :inverse-video nil
                                :background (color-darken-name (face-background 'default) 5)
                                :foreground "cyan"
                                :underline '(:color "dark red")
                                :weight 'bold
                                )))

(set-face-attribute 'helm-header nil
                    :reverse-video nil
                    :foreground "gray" :background "black"
                    :weight 'bold)
(set-face-attribute 'helm-source-header nil
                    :foreground "green" :background "#222222"
                    :overline "yellow" :weight 'bold
                    ;; :family "DejaVu Sans Mono" :height 1.0
                    )
(set-face-attribute 'helm-prefarg nil
                    :foreground "cyan")
(set-face-attribute 'helm-action nil
                    :inverse-video nil
                    :background " " :foreground "orange"
                    :underline nil
                    )
(set-face-attribute 'helm-separator nil
                    :foreground "cyan")
(set-face-attribute 'helm-visible-mark nil
                    :foreground "black" :background "green yellow")
;; FIXME:
;; (set-face-attribute 'helm-lisp-completion-info nil
;;                     :foreground "cyan")

(helm-autoresize-mode t)

(setq helm-full-frame nil ; use current window as popup. helm popup window position.
      helm-always-two-windows t
      helm-autoresize-max-height 25
      helm-autoresize-min-height 10
      ;; split
      helm-split-window-in-side-p t ; force split inside selected window.
      helm-split-window-default-side 'below
      ;; helm-split-window-preferred-function 'helm-split-window-default-fn
      helm-quick-update t ; do not display invisible candidates
      ;; helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-case-fold-search t
      helm-buffers-fuzzy-matching nil
      helm-recentf-fuzzy-match nil
      helm-move-to-line-cycle-in-source nil ; nil: not just cycling current limited candicates section.
      ;; find-file
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
      helm-ff-file-name-history-use-recentf t ; use recentf
      helm-ff-transformer-show-only-basename t ; show basename in find file candidates. toggled by C-].
      ;; helm-sources-using-default-as-input
      )

(setq helm-browse-url-default-browser-alist '(("conkeror" . helm-browse-url-conkeror) ; Conkeror
                                              ("uzbl-browser" . helm-browse-url-uzbl) ; uzbl
                                              ("firefox" . browse-url-firefox) ; Firefox
                                              ("chromium-browser" . helm-browse-url-chromium) ; chromium
                                              ("emacs" . eww-browse-url) ; eww
                                              ("/usr/bin/w3m" . w3m-browse-url) ; w3m
                                              ("gnome-moz-remote" . browse-url-gnome-moz) ; Gnome
                                              ("mozilla" . browse-url-mozilla) ; Mozilla
                                              ("kfmclient" . browse-url-kde) ; KDE
                                              ("galeon" . browse-url-galeon)
                                              ("netscape" . browse-url-netscape) ; netscape
                                              ("xmosaic" . browse-url-mosaic)
                                              ("xterm" . browse-url-text-xterm) ; XTerm
                                              ))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t
        ;; helm-google-suggest-default-browser-function
        ;; helm-google-suggest-default-function 'helm-google-suggest-set-candidates
        ;; helm-google-suggest-search-url "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
        ;; helm-google-suggest-url "http://google.com/complete/search?output=toolbar&q="
        ))


(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Bookmark
;; Helm bookmarks [C-x C-x r b]
;; (helm-highlight-bookmark)

(setq helm-bookmark-show-location t)

;; Firefox bookmarks [C-x C-x]
;; NOTE config your firefox `about:config' to enable:
;; user_pref("browser.bookmarks.autoExportHTML", false);


;; man-women

;;; - helm-man-women ::

;; (setq helm-man-or-woman-function 'Man-getpage-in-background)


;; [ helm-descbinds ]

;; Usage:
;; - [C-h KEY]
;; - [KEY_PREFIX C-h]

;; - when in helm completion buffer:
;;   - press [RET] to select candidate command to execute.
;;   - press [TAB], you can execute action: "execute", "describe function", "find function".
;;   - press [C-z], selected command is described without quiting.

(require 'helm-descbinds)
(helm-descbinds-mode 1)


;;; [ helm-themes ]

;;; Usage:
;;
;; [M-x helm-themes]

;;; Config
;; (require 'helm-themes)


;;; [ helm-gtags ]

(require 'helm-gtags)

(define-key helm-command-prefix (kbd "t") 'helm-gtags-dwim)
(define-key helm-command-prefix (kbd "T") 'helm-top)


;;; [ helm-yaetags ]

(require 'helm-yaetags)

;; (define-key helm-command-prefix (kbd "e") 'helm-etags-select) ; original
(define-key helm-command-prefix (kbd "e") 'helm-yaetags-find-tag)


;; [ helm-projectile ]

(require 'helm-projectile)



;; (defun my-helm ()
;;   "My preconfigured `helm'."
;;   (interactive)
;;   (condition-case nil
;;       (if (projectile-project-root)
;;           (helm-projectile)
;;         ;; otherwise fallback to `helm-mini'
;;         (helm-mini))
;;     ;; fall back to helm mini if an error occurs (usually in `projectile-project-root')
;;     (error (helm-mini))))
;;
;; (global-set-key (kbd "C-x h") 'my-helm)


;; This provides a single command `helm-helm-commands' which will present a helm
;; buffer containing a list of helm commands and short descriptions. You can
;; press C-z on an item to see a longer description of the command, and RET to
;; execute the command.

(defvar helm-helm-commands-source-buffer "*helm source select*")

(defvar helm-source-helm-commands
  `((name . "Helm commands")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for symname in (all-completions "helm-" obarray)
               for sym = (intern symname)
               if (commandp sym) collect
               (cons
                (concat
                 (propertize (format "%s" symname)
                             'face 'font-lock-function-name-face)
                 (propertize (format " %s"
                                     (or (and (documentation sym)
                                              (car (split-string
                                                    (documentation sym) "\n\\|\\.")))
                                         "Not documented"))
                             'face 'font-lock-doc-face))
                sym))))
    (action . (("Execute helm command" .
                (lambda (candidate)
                  (call-interactively candidate)))
               ("Describe command" . describe-command)))
    (persistent-action . describe-command)))

(defun helm-helm-commands nil
  "Select from helm commands to execute."
  (interactive)
  (helm :sources 'helm-source-helm-commands
        :buffer helm-helm-commands-source-buffer))


;;; [ helm-project ]



;;; [ helm-ls-git ] -- Yet another helm to list git file.

;; (require 'helm-ls-git)



;;; [ helm-cmd-t ] -- Helm functions to package directories (SCM controlled or not) as sources.

(require 'helm-cmd-t)

;; (setq helm-cmd-t-repo-types '(("git" ".git" "cd %d && git --no-pager ls-files --full-name")
;;                               ("hg" ".hg" "cd %d && hg manifest")
;;                               ("bzr" ".bzr" "cd %d && bzr ls --versioned")
;;                               ("dir-locals" ".dir-locals.el" helm-cmd-t-get-find)
;;                               ("" "" helm-cmd-t-get-find))
;;       )

(global-set-key (kbd "M-t") 'helm-cmd-t)

;;; additional optional helm settings to make helm more responsive.
;; (setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)

;;; Creating an ad-hoc source
;;; It’s easy to convert any file system directory into a source
;;
;; (setq downloads-source (helm-cmd-t-get-create-source-dir "~/Downloads"))
;; (setq docs-source (helm-cmd-t-get-create-source-dir "~/Documents"))
;;
;; (defun helm-cmd-t-ad-hoc-example ()
;;   "Choose file from test folder."
;;   (interactive)
;;   (helm :sources (list downloads-source docs-source)))



;;; [ helm-c-yasnippet ] -- helm source for yasnippet.el

;;; Usage:
;; - `helm-yas-complete' :: List of yasnippet snippets using `helm' interface.
;; - `helm-yas-create-snippet-on-region' :: Create a snippet from region.

(require 'helm-c-yasnippet)

(setq helm-yas-space-match-any-greedy t ; helm pattern space match anyword greedy.
      helm-yas-not-display-dups t
      helm-yas-display-msg-after-complete t
      helm-yas-display-key-on-candidate t
      )

(global-set-key (kbd "C-c & C-c") 'helm-yas-complete) ; integrate helm with yasnippet.


;;; [ helm-c-moccur ]

;; - [C-x c M-s o] :: `helm-occur'
(define-key helm-command-map (kbd "M-s m") 'helm-multi-occur)


;;; [ helm-gist ]

;;; Usage:
;; - `helm-for-gist'
;; - Helm defined source: `helm-c-source-gist'

(require 'helm-gist)


;;; [ helm-dictionary ]

;;; Usage:
;;
;; - [M-x helm-dictionary] :: start search.

;; (require 'helm-dictionary)
;; alternatively
(autoload 'helm-dictionary "helm-dictionary" "" t)

;; (setq
;;  ;; local dictionary
;;  helm-dictionary-database ""
;;  ;; online dictionary
;;  helm-dictionary-online-dicts '(("translate.reference.com de->eng"
;;                                  . "http://translate.reference.com/translate?query=%s&src=de&dst=en")
;;                                 ("translate.reference.com eng->de"
;;                                  . "http://translate.reference.com/translate?query=%s&src=en&dst=de")
;;                                 ("leo eng<->de"
;;                                  . "http://dict.leo.org/ende?lp=ende&lang=de&search=%s")
;;                                 ("en.wiktionary.org"
;;                                  . "http://en.wiktionary.org/wiki/%s")
;;                                 ("de.wiktionary.org"
;;                                  . "http://de.wiktionary.org/wiki/%s")
;;                                 ("linguee-eng<->de"
;;                                  . "http://www.linguee.de/deutsch-englisch/search?sourceoverride=none&source=auto&query=%s"))

;;  ;; helm-dictionary-browser-function nil
;;  )


;; ;;; [ helm-delicious ]

;; ;; Use:
;; ;; ===
;; ;;
;; ;; M-x helm-delicious
;; ;; That should create a "~/.delicious-cache" file.
;; ;; (you can set that to another value with `helm-c-delicious-cache-file')
;; ;; You can also add `helm-c-source-delicious-tv' to the `helm-sources'.
;; ;;

;; (require 'helm-delicious)

;; ;; after subscribing to http://delicious.com/
;; ;; Setup your login and delicious password:
;; ;;
;; ;; You can set it up in your init file with
;; ;;
;; ;; `helm-delicious-user' and `helm-delicious-password'
;; ;; (use setq)
;; ;;
;; ;; or better:
;; ;;
;; ;; Add a line like this in your .authinfo file:
;; ;;
;; ;; machine api.del.icio.us:443 port https login xxxxx password xxxxx
;; ;;
;; ;; and add to you init file (.emacs):
;; (require 'auth-source)

;; (if (file-exists-p "~/.authinfo.gpg")
;;     (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
;;   (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))

;; ;; Warning:
;; ;;
;; ;; DON'T CALL `helm-delicious-authentify', this will set your login and password
;; ;; globally.


;;; [ helm-source-time-world ]

;; use the Emacs built-in variable: `display-time-world-list'.

(setq helm-time-zone-home-location "Shanghai")

(define-key my-tools-prefix-map (kbd "t") 'helm-world-time)



(provide 'init-helm)

;;; init-helm.el ends here
