;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)
(setq completion-at-point-functions 'auto-complete)


;;; [ pcomplete ] --- programmable completion
(load-library "pcomplete")


;;; Press [TAB] in minibuffer to show completions in popup window buffer.


;;; [ ido ]

(require 'ido)
(require 'ido-ubiquitous)
(ido-mode t)                            ; enable ido mode
(ido-everywhere t)                      ; use ido-mode wherever possible
(ido-ubiquitous-mode t)                 ; enable ido-ubiquitous
(setq ido-enable-flex-matching 't       ; enable fuzzy search
      ido-use-filename-at-point 't      ; look for filename at point
      ido-use-virtual-buffers 't        ; allow me to open closed buffers, even
      ido-auto-merge-work-directories-length 0
      ido-default-buffer-method 'selected-window ; allow buffer to be open in different frames
      )


;;; [ ido-vertical-mode ] -- vertical ido.

;;; Usage:
;; - [M-x]
;; - [C-n/p]

(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)

;; (setq ido-decorations '("{" "}" " | " " | ..." "[" "]"
;;                         " [No match]" " [Matched]" " [Not readable]"
;;                         " [Too big]" " [Confirm]")
;;       ido-vertical-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
;;                                  " [No match]" " [Matched]" " [Not readable]"
;;                                  " [Too big]" " [Confirm]"
;;                                  "\n-> " "")
;;       )



;;; [ Icomplete ]

;; (icomplete-mode 1)



;;; [ Helm ] --- (incremental completion and selection narrowing framework)
;; Customize:
;; - [C-c c] -- for all complete framework prefix.

;; Basic usage:
;;  - M-x helm-mini
;; general helm commands:
;;  - [TAB] -- access to `action' menu with
;;  - [C-z] -- use persistent actions with
;;  - [M-SPACE] -- mark candidate with
;; get helm help in heml minor mode:
;;  - [C-h m]
;; wildcard match
;;  - ~/_esk (here `_' is a space)

;; (require 'helm)
;; (require 'helm-config)

;; (helm-mode 1)

;; (set-face-attribute 'helm-selection nil
;;                     :background "#004A5D" :foreground "white"
;;                     :box '(:color "cyan" :line-width 1)
;; 		    :underline nil)

;; (global-set-key (kbd "C-c h") 'helm-mini)

;; (setq helm-case-fold-search t) ; whether toggle case sensitive search depend on your input has mixture of upcase and downcase.

;;; Bookmark
;; Helm bookmarks[C-x C-x r b]
;; (helm-highlight-bookmark)

;; Firefox bookmarks [C-x C-x]
;; NOTE config your firefox `about:config' to enable:
;; user_pref("browser.bookmarks.autoExportHTML", false);


;;; [ helm-descbinds ]

;;; Usage:
;; - [C-h KEY]
;; - [KEY_PREFIX C-h]
;; - when in helm completion buffer:
;;   - press [RET] to select candidate command to execute.
;;   - press [TAB], you can "execute", "describe function", "find function".
;;   - press [C-z], selected command is described without quiting.

;; (require 'helm-descbinds)
;; (helm-descbinds-mode 1)


;;; auto-complete
;; candidates suffix
;; - nothing  -- means it is buffer string cache complete.
;; - v  -- variable
;; - f  -- function
;; - s  -- symbol
;; - c  -- constant
;; - a  -- abbrev, yasnippet
;; - d  -- dictionary, environment variable (e.g. LC_CTYPE)
;; - m  -- module
;;--------------
;; - r  -- Robe for Ruby
;;
;; Usage:
;; - ["M-["] / [Tab] -- to complete "common" string.
;; - [(C/M)-n/p] -- select next/previous.
;; - [M-h] -- last-quick-help.
;; - [C-c h], [C-c S-h] -- show auto-complete key bindings help.
;; - [C-s] in popup menu -- to search with pattern.
;; M-x auto-complete-mode to start using it manually.
;; * completion by [M-TAB]
;;  - case that only one candidate remains
;;      the candidate will be used to complete
;;  - case that there is common part among candidates
;;      e.g. if all candidates start with "set", so TAB completes "set" at first
;;  - otherwise
;;      otherwise, select candidates in cycle by typing TAB
;; * completion by RET [C-m]
;;  - complete a selected candidate immediately
;;  - execute an action if a selected candidate has the action
;; auto-complete-mode philosophy:
;;  It is not recommended to select candidates. Because it means it has been
;;  failed to guess completion, and also it requires for users to do candidate
;;  selection which is a high cost operation.
;; * candidate selection
;;  - M-n/p
;;  - M-1/2/....
;; * Help
;;  - quick help (appear at the side of completion menu)
;;  - buffer help (display a help in a buffer of other window)
;;      - [M-h] or [C-?] or [F1]
;;      - [C-M-v] or [C-M-S-v] to scroll

(require 'auto-complete)
(require 'auto-complete-config)
(require 'popup)
;; TODO: remove this if not needed any more.
;; (require 'showtip)

;; (ac-config-default)

(global-auto-complete-mode 1) ; use auto-complete globally


(setq ac-auto-start 2) ; auto start auto-complete when has N characters.


;;; dirty fix for having AC everywhere

;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                            (auto-complete-mode))
;;                        ))
;; (real-global-auto-complete-mode t)


;; auto raise popup menu
(setq ac-delay 0.2) ; delay time to start completion in real number seconds
;; (setq ac-show-menu-immediately-on-auto-complete t) ; it is a trade off of responsibility and performance
(setq ac-auto-show-menu 0.3) ;; show popup menu after how many seconds
(setq ac-menu-height 10) ; smaller ac-menu is more cute. big ac-menu is not necessary.
;; NOTE: small menu is helpful for small computer screen, because split window has small height, this cause quick help popup is shown in hidden position.


(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-dwim nil) ; to get pop-ups with docs even if a word is uniquely completed.


;; trigger key [TAB]
;; (ac-set-trigger-key "TAB") ; usualy this, <tab> has higher priority than TAB.
;; (ac-set-trigger-key "<tab>")
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;; ac-menu-map keymap only map for menu is available, not break default.
(setq ac-use-menu-map t)       ; nil: to disable default ac-menu-map key bindings.
;; disable [<tab>] [C-n/p] -> ac-next in ac-menu.
(define-key ac-menu-map (kbd "<tab>") nil)
(define-key ac-menu-map (kbd "<S-tab>") nil) ; "S-TAB". "?\\s-\\t"
(define-key ac-menu-map (kbd "C-n") nil)
(define-key ac-menu-map (kbd "C-p") nil)

(define-key global-map (kbd "M-TAB") 'ac-fuzzy-complete) ; fuzzy complete.
;; ISSUE: when I typed the whole candidate string, then press [SPC] will insert two spaces.
(define-key ac-menu-map (kbd "SPC")
  (defun ac-complete-with-space ()
    "Select the candidate and append a space. save your time for typing space."
    (interactive)
    (ac-complete)
    (insert " ")
    ))
(define-key ac-menu-map (kbd "M-SPC") 'ac-complete) ; select current candidate.
(define-key ac-menu-map (kbd "M-j") 'ac-complete) ; select current candidate.
(define-key ac-menu-map (kbd "M-n") 'ac-next) ; next candidate.
(define-key ac-menu-map (kbd "M-p") 'ac-previous) ; previous candidate.
(define-key ac-menu-map (kbd "M-i") 'ac-expand) ; for expand snippet, abbrev etc.
(define-key ac-menu-map (kbd "C-s") 'ac-isearch)
(define-key ac-menu-map (kbd "M-s") 'ac-isearch) ; isearch in popup menu.
(define-key ac-menu-map (kbd "C-i") 'ac-expand-common) ; complete common string.
(define-key ac-menu-map (kbd "C-h") 'ac-stop) ; close the auto complete popup menu.
(define-key ac-menu-map (kbd "<return>") 'newline) ; go to new line.


;;; [ Help ]
;; ac-help: [M-h], [C-?], [F1]
;; [C-M-n/p] : ac-quick-help-scroll-[down/up]
;; [C-up] : ac-quick-help-scroll-up
;; [C-down] : ac-quick-help-scroll-down
;; ac-persist-help: [M-S-h], [C-M-?] , [M-F1] (show help persistently)
;; ac-last-help & ac-last-quick-help
;; [C-?] :: ac-help
;; [C-M-?] :: ac-persist-help
;; quick-help
(setq ac-use-quick-help t) ; nil to disable auto popup quick help.
(setq ac-quick-help-delay 1.3)
(setq ac-quick-help-height 20)
;; buffer help
(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)


;; show help beautifully with extension "pos-tip.el"

(require 'pos-tip)


;;; [ fuzzy completion ]
;; (setq ac-use-fuzzy t)


;;; [ Filter ]
;; - [C-s] -- to filter, the cursor color will change.
;; - [C-s] (again) -- the filtering string will be restored
;; - [DEL/C-h] -- delete the filter string


;; [ case sensitive ]
;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)


;;; Faces

;; black, CornflowerBlue, DarkSlateBlue, DarkRed, DeepPink, HotPink, DarkMagenta, DarkSlateGray,
;; GreenYellow, DimGray, IndianRed, LawnGreen, LightBlue, LightSkyBlue, pink, salmon, SkyBlue, maroon,


;; color of candidates
(set-face-attribute 'ac-candidate-face nil
                    :foreground "black" :background "white"
                    :inherit nil
                    )
;; color of selection
(set-face-attribute 'ac-selection-face nil
                    :foreground "white" :background "black"
                    :inherit nil
                    )
;; foreground color of inline completion
(set-face-foreground 'ac-completion-face "green yellow")
(set-face-background 'ac-candidate-mouse-face "orange")

(setq ac-fuzzy-cursor-color "orange red")

;;; Popup faces:
;;; in my-init/Emacs/init-my-emacs-popup.el file.


;;; auto-complete UI
(setq ac-max-width nil) ; 'nil, 25, 0.5


;;; [ AI ] (learning your operations)

(setq ac-comphist t) ; comphist internally


;; [ completion by dictionary ]
;;  - user defined dictionary
;;  - major mode dictionary
;;  - extension dictionary
;;  NOTE: after editing and adding dictionary, you should do:
;;      M-x ac-clear-dictionary-cache
;;      to apply changes.

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")


;;; [ auto-complete sources ]
;; source is a concept that insures a extensibility of auto-complete-mode.
;; Simply saying, source is a description about:
;;  - how to generate completion candidates
;;  - how to complete
;;  - how to show
;; Usually a name of source start with "ac-source-", so you can list up sources
;; with apropos:
;;  (M-x apropos RET ^ac-source-)
;; you can see the setting by evaluating `ac-sources` in *scratch* buffer.
;; "ac-sources" is a buffer local variable.

;; "M-/" dabbrev-expand
;; (unless (package-installed-p 'ac-dabbrev)
;;   (package-install 'ac-dabbrev))
;; (require 'ac-dabbrev)
;; (unless (package-installed-p 'ac-ispell)
;;   (package-install 'ac-ispell))
;; (require 'ac-ispell)

;; (unless (package-installed-p 'ac-math)
;;   (package-install 'ac-math))
;; (require 'ac-math)

(require 'auto-complete-pcmp)
(require 'auto-complete-etags)

;;; chunk for dot.separated.words
;; (require 'auto-complete-chunk)

;; (require 'auto-complete-yasnippet)


;;; ac-etags --- https://github.com/syohex/emacs-ac-etags

;;; ac-complete candidate suffix symbol is [s]

;; 1. generate tag file
;; - etags *.c *.h
;; - ctags -e *.c *.h
;; 2. set path of TAGS
;; - [M-x visit-tags-table]

;; (require 'ac-etags)
;;
;; (eval-after-load "etags"
;;   (progn
;;     (ac-etags-setup)))
;;
;; (setq ac-etags-requires 3)


;;; [ semantic ]

;; (semantic-mode 1)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-semantic)))


;;; set default auto-complete source
(setq-default ac-sources
              '(;; snippet
                ac-source-yasnippet
                ;; template
                ;; ac-source-template
                ;; filename
                ac-source-filename
                ac-source-files-in-current-dir
                ;; programming
                ;; ac-source-semantic
                ;; ac-source-semantic-raw
                ;; tags
                ;; ac-source-etags
                ;; ac-source-gtags
                ;; ac-source-functions
                ;; ac-source-variables
                ;; ac-source-symbols
                ;; abbrev
                ac-source-abbrev
                ;; ac-source-dabbrev
                ;; chunk
                ;; ac-source-chunk-list
                ;; buffer
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ;; spell
                ;; ac-source-ispell
                ;; dictionary
                ;; ac-source-dictionary
                ;; ac-source-dictionary-chunk
                ;; ac-source-entity
                ;; features
                ac-source-features ; for emacs-lisp features
                ))


;;; other sources faces

;;; ac-dabbrev
;; (set-face-attribute 'ac-dabbrev-menu-face nil
;;                     :foreground "dark magenta"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-dabbrev-selection-face nil
;;                     )
;;; ac-etags
;; (set-face-attribute 'ac-etags-candidate-face nil
;;                     :foreground "sea green"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-etags-selection-face nil
;;                     )
;;; ac-gtags
;; (set-face-attribute 'ac-gtags-candidate-face nil
;;                     :foreground "dark green"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-gtags-selection-face nil
;;                     )
;;; ac-yasnippet
(set-face-attribute 'ac-yasnippet-candidate-face nil
                    :background "deep pink"
                    :weight 'normal)
(set-face-attribute 'ac-yasnippet-selection-face nil
                    :background "deep pink"
                    :underline t
                    )
;;; auto-complete-clang
;; (set-face-attribute 'ac-clang-candidate-face nil
;;                     :foreground "sky blue"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-clang-selection-face nil
;;                      )



;; TODO set auto-complete source priority.
;; (defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
;;   (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))


;;; add other modes into ac modes
(setq ac-modes
      (append ac-modes
              '(prog-mode               ; programming modes
                text-mode org-mode markdown-mode
                change-log-mode
                ;; objc-mode
                ;; sql-mode js3-mode
                makefile-mode makefile-gmake-mode makefile-automake-mode
                autoconf-mode
                snippet-mode)))


(ac-flyspell-workaround) ; known bug, a way of delaying process of flyspell-mode disables auto-complete.


;;; avoid auto-complete popup lines(candidates) get wrapped.
;; check out here. https://github.com/auto-complete/auto-complete/issues/199
;; (ac-linum-workaround)


;;; [ Company Mode ]

;; (require 'company)

;;; To use company-mode in all buffers, add the following line to your init file:
;; (add-hook 'after-init-hook 'global-company-mode)

;; (setq company-minimum-prefix-length 3)

;; (setq company-backends '(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
;;               (company-dabbrev-code company-gtags company-etags company-keywords)
;;               company-oddmuse company-files company-dabbrev))




(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
