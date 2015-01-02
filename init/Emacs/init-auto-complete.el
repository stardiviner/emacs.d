;;; init-auto-complete.el --- init auto-complete.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; [ auto-complete ]

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
;; - h  -- ac-org
;; - t  -- ac-org -> TeX (\S in Org-mode).
;; - k  -- ac-org
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
;;

(require 'auto-complete)
(require 'auto-complete-config)
(require 'popup)
;; (require 'showtip)
(diminish 'auto-complete-mode)

;; (ac-config-default)

;; require other auto-complete sources
(require 'ac-capf)

(global-auto-complete-mode 1) ; use auto-complete globally
;;; or enable for specific modes.
(if (null global-auto-complete-mode)
    (setq ac-modes
          (append ac-modes
                  '(prog-mode               ; programming modes
                    web-mode
                    text-mode markdown-mode
                    change-log-mode
                    ;; org-mode ; speed up org-mode typing by disabling auto-complete.
                    mail-mode mu4e-compose-mode
                    ;; objc-mode
                    ;; sql-mode js3-mode
                    makefile-mode makefile-gmake-mode makefile-automake-mode
                    autoconf-mode
                    snippet-mode))))



;;; dirty fix for having AC everywhere

;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                            (auto-complete-mode))
;;                        ))
;; (real-global-auto-complete-mode t)


;; auto raise popup menu
(setq ac-auto-start 3) ; auto start auto-complete when has N characters.
(setq ac-delay 0.3) ; delay time to start completion in real number seconds
(setq ac-show-menu-immediately-on-auto-complete t) ; it is a trade off of responsibility and performance
(setq ac-auto-show-menu 0.4) ;; show popup menu after how many seconds
(setq ac-menu-height 10) ; smaller ac-menu is more cute. big ac-menu is not necessary.
;; NOTE: small menu is helpful for small computer screen, because split window
;; has small height, this cause quick help popup is shown in hidden position.


(setq-default ac-expand-on-auto-complete t) ; Non-nil means expand whole common part on first time `auto-complete'.
(setq-default ac-dwim t) ; to get pop-ups with docs even if a word is uniquely completed.


(setq ac-delete-dups nil) ; t: auto delete duplicate candidates. nil: different type but same candidates.


;; trigger key [TAB]

;; (ac-set-trigger-key "<tab>") ; <tab> is used for yasnippet.
(ac-set-trigger-key "TAB") ; usualy this, <tab> has higher priority than TAB.
(define-key ac-mode-map (kbd "C-i") 'auto-complete)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; (define-key global-map (kbd "M-TAB") 'ac-fuzzy-complete) ; fuzzy complete.

;;; ac-menu-map keymap only map for menu is available, not break default.
(setq ac-use-menu-map t)       ; nil: to disable default ac-menu-map key bindings.
;; disable [<tab>] [C-n/p] -> ac-next in ac-menu.
(define-key ac-menu-map "\t" nil)
(define-key ac-menu-map [tab] nil)
(define-key ac-menu-map (kbd "<tab>") nil)
(define-key ac-menu-map (kbd "<S-tab>") nil) ; "S-TAB". "?\\s-\\t"
(define-key ac-menu-map "\r" nil)
(define-key ac-menu-map [return] nil)

(define-key ac-menu-map (kbd "C-n") nil)
(define-key ac-menu-map (kbd "C-p") nil)
(define-key ac-menu-map (kbd "C-j") nil)
;;
;; (define-key ac-menu-map (kbd "C-n") 'ac-next)
;; (define-key ac-menu-map (kbd "C-p") 'ac-previous)
;; (define-key ac-menu-map (kbd "C-j") 'ac-complete)


(define-key ac-menu-map (kbd "M-j") 'ac-complete) ; select current candidate.
(define-key ac-menu-map (kbd "M-n") 'ac-next) ; next candidate.
(define-key ac-menu-map (kbd "M-p") 'ac-previous) ; previous candidate.
(define-key ac-menu-map (kbd "M-i") 'ac-expand) ; for expand snippet, abbrev etc.
(define-key ac-menu-map (kbd "C-s") 'ac-isearch)
(define-key ac-menu-map (kbd "M-s") 'ac-isearch) ; isearch in popup menu.
(define-key ac-menu-map (kbd "C-i") 'ac-expand-common) ; complete common string.
(define-key ac-menu-map (kbd "C-h") 'ac-stop) ; close the auto complete popup menu.

(defun my-ac-return ()
  (interactive)
  (ac-stop)
  (newline-and-indent))
(define-key ac-menu-map (kbd "<return>") 'my-ac-return) ; go to new line.
(define-key ac-menu-map [return] 'my-ac-return)
(define-key ac-menu-map "\r" 'my-ac-return)


;;; complete(select ac-candidate and insert "SPACE")

;; ac-completing-map is the parent map of ac-menu-map.

;;; way 1
(defun ac-complete-with-space ()
  (interactive)
  (ac-complete)
  (insert " "))

;; (define-key ac-menu-map (kbd "SPC") 'ac-complete-with-space)
(define-key ac-menu-map (kbd "M-SPC") 'ac-complete-with-space)

(defun my-ac-complete-with-space-for-org-mode ()
  ;; (local-set-minor-mode-key 'ac-completing-map (kbd "SPC") nil)
  (local-set-minor-mode-key 'ac-menu-map (kbd "SPC") nil))

(add-hook 'org-mode-hook #'my-ac-complete-with-space-for-org-mode)


;;; way 2
;; (defun ac-complete-with-space ()
;;   "Select the candidate and append a space. save your time for typing space."
;;   (interactive)
;;   (if (eq major-mode 'org-mode)
;;       (self-insert-command 1)
;;     (ac-complete)
;;     (insert " "))
;;   ;; ISSUE: when I typed the whole candidate string, then press [SPC] will insert two spaces.
;;   ;; (if (pre-char before point is *only* not whitespace)
;;   ;;     (insert " "))
;;   )

;; (define-key ac-menu-map (kbd "SPC") 'ac-complete-with-space)
;; (define-key ac-menu-map (kbd "M-SPC") 'ac-complete-with-space)



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
;; Prefer native tooltip with pos-tip than overlay popup for displaying quick help.
(setq ac-quick-help-prefer-pos-tip t)
(setq ac-quick-help-delay 0.6)
(setq ac-quick-help-timer nil)     ; quick help idle timer. (nil: never disappear)
(setq ac-quick-help-height 20)

(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
(define-key ac-completing-map (kbd "C-M-n") 'ac-quick-help-scroll-down)
(define-key ac-completing-map (kbd "C-M-p") 'ac-quick-help-scroll-up)
(define-key ac-completing-map [C-down] 'ac-quick-help-scroll-down)
(define-key ac-completing-map [C-up] 'ac-quick-help-scroll-up)

;; buffer help
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)


;; show help beautifully with extension "pos-tip.el"

(require 'pos-tip)
(require 'popup-pos-tip)


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

(setq ac-disable-faces '(font-lock-comment-face font-lock-string-face font-lock-doc-face)
      ac-disable-inline nil               ; disable inline completion visibility
      )

;; color of candidates
(set-face-attribute 'ac-candidate-face nil
                    :foreground "black" :background "white"
                    ;; fix for ac candidates face italic/bold on comments etc.
                    :inherit nil :slant 'normal :weight 'normal :height 100
                    )
;; color of selection
(set-face-attribute 'ac-selection-face nil
                    :foreground "white" :background "black"
                    :inherit nil :slant 'normal :weight 'normal :height 100
                    )
;; foreground color of inline completion
(set-face-foreground 'ac-completion-face "green yellow")
(set-face-background 'ac-candidate-mouse-face "orange")

(setq ac-fuzzy-cursor-color "orange red")

;;; Popup faces:
;;; in my-init/Emacs/init-my-emacs-popup.el file.


;;; auto-complete UI
;; - nil : no limit
;; - 25  : character limit
;; - 0.5 : window ratio limit
(setq ac-max-width nil)


;;; [ AI ] (learning your operations)

(setq ac-comphist t) ; comphist internally


;; [ completion by dictionary ]
;;  - user defined dictionary
;;  - major mode dictionary
;;  - extension dictionary
;;  NOTE: after editing and adding dictionary, you should do:
;;      M-x ac-clear-dictionary-cache
;;      to apply changes.

(add-to-list 'ac-dictionary-directories
             (expand-file-name "ac-dict" user-emacs-directory))


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

(require 'auto-complete-pcmp)
(require 'auto-complete-yasnippet)
;; "M-/" dabbrev-expand
;; (unless (package-installed-p 'ac-dabbrev)
;;   (package-install 'ac-dabbrev))
;; (require 'ac-dabbrev)
;;; chunk for dot.separated.words
;; (require 'auto-complete-chunk)

;; (unless (package-installed-p 'ac-ispell)
;;   (package-install 'ac-ispell))
;; (require 'ac-ispell)

;; (unless (package-installed-p 'ac-math)
;;   (package-install 'ac-math))
;; (require 'ac-math)


;;; [ auto-complete-etags ]

;;; ac-complete candidate suffix symbol is [s]
;;;
;; 1. generate tag file
;; - etags *.c *.h
;; - ctags -e *.c *.h
;; 2. set path of TAGS
;; - [M-x visit-tags-table]

(require 'auto-complete-etags)


;; (require 'ac-etags)
;;
;; (eval-after-load 'etags
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
                ;; abbrev
                ac-source-abbrev
                ;; ac-source-dabbrev
                ;; filename
                ac-source-filename
                ac-source-files-in-current-dir
                ;; programming
                ac-source-capf
                ;; ac-source-semantic
                ;; ac-source-semantic-raw
                ;; tags
                ;; ac-source-etags ; NOTE: by default require a TAGS file.
                ;; ac-source-gtags
                ;; dictionary
                ac-source-dictionary
                ;; chunk
                ;; ac-source-chunk-list
                ;; buffer
                ;; ac-source-words-in-buffer ; NOTE: this source will show a lot of useless candidates.
                ac-source-words-in-same-mode-buffers
                ;; ac-source-words-in-all-buffer
                ;; spell
                ;; ac-source-ispell
                ))


;;; other sources faces

;; NOTE: :inherit 'ac-candidate-face + :inherit 'ac-selection-face.

;;; ac-dabbrev
;; (set-face-attribute 'ac-dabbrev-menu-face nil
;;                     :inherit 'ac-candidate-face
;;                     :foreground "dark magenta"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-dabbrev-selection-face nil
;;                     :inherit 'ac-selection-face
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
                    :inherit 'ac-candidate-face
                    :background "deep pink"
                    :weight 'normal)
(set-face-attribute 'ac-yasnippet-selection-face nil
                    :inherit 'ac-selection-face
                    :background "deep pink"
                    :underline t
                    )
;;; auto-complete-clang
;; (set-face-attribute 'ac-clang-candidate-face nil
;;                     :foreground "sky blue"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-clang-selection-face nil
;;                      )



;; TODO: set auto-complete source priority.
;; (defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
;;   (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))



(ac-flyspell-workaround) ; known bug, a way of delaying process of flyspell-mode disables auto-complete.


;;; avoid auto-complete popup lines(candidates) get wrapped.
;; check out here. https://github.com/auto-complete/auto-complete/issues/199
;; (ac-linum-workaround)


;;; [ ac-capf ] -- auto-complete source of completion-at-point

(require 'ac-capf)

;; global
;; (ac-capf-setup)
;; (add-to-list 'ac-sources 'ac-source-capf)
;; or enable in some modes
;; (dolist (hook '(ruby-mode-hook
;;                 enh-ruby-mode-hook
;;                 inf-ruby-mode-hook
;;                 inferior-python-mode-hook
;;                 ))
;;   (add-hook hook 'ac-capf-setup))


;;; [ ac-helm ]

;; (require 'ac-helm)  ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-:") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)


;;; [ ac-company ] -- company-mode source

;; (require 'ac-company)

;; (ac-company-define-source ac-source-capf company-capf
;;                           (symbol . "r") ; "r" or "s"
;;                           ;; FIXME: ac-quick-help error: wrong-type-argument stringp nil.
;;                           ;; check out auto-complete define source.
;;                           (document . company-quickhelp)
;;                           )
;; (add-to-list 'ac-sources 'ac-source-capf)

;;; Example:
;;
;; (ac-company-define-source ac-source-company-elisp company-elisp)
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-company-elisp)))
;;
;; You can overrides attributes. For example, if you want to add symbol to
;; ac-source-company-elisp, put following:
;;
;; (ac-company-define-source ac-source-company-elisp company-elisp
;;                           (symbol . "s"))


(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
