;;; init-my-emacs-minibuffer.el --- init Emacs minibuffer

;;; Commentary:

;;; Code:

;;; [ minibuffer ]
(setq enable-recursive-minibuffers t)   ; enable to use minibuffer recursively.

;;; [ icomplete-mode ] -- incremental minibuffer completion.
;; - [C-h f icomplete-completions]
;; - (...) :: a single prospect is identified and matching is enforced,
;; - [...] :: a single prospect is identified but matching is optional,
;; - {...} - multiple prospects, separated by commas, are indicated, and further input is required to distinguish a single one.
;; Usage:
;; - [C-.] -- forward completion
;; - [C-,] -- backward completion

;; (icomplete-mode +1)


;;; [ ido ]
;;; Usage:
;; - [TAB] --
;; - [M-n/p] / [C-n/p] / [C-,/.] / [C-s/r] -- next/previous candidate
;; - [C-j] -- select inputted text.
;; - [C-p] -- regexp matching.
;; - [C-f] -- fall back to find file (without ido-mode).
;; - [C-b] -- fall back to buffer switch (without ido-mode).
;; - restrict the list after a first filtering:
;;     - type some characters appearing in the buffer/file name(say .cpp)
;;     - type C-SPC (C-@)
;;     - continue as normal with a list containing only the filtered names
;; - using ido programmatically.
;;     (setq mylist (list "red" "blue" "yellow" "clear" "i-dont-know"))
;;     (ido-completing-read "What, ... is your favorite color? " mylist)


(require 'ido)

(ido-mode t)

(setq ido-everywhere t
      ido-enable-regexp nil
      ido-enable-flex-matching t
      ido-enable-prefix nil             ; match if the entered text is an arbitrary substring.
      ido-enable-dot-prefix t           ; to match hidden files and directories even `ido-enable-prefix' is nil.
      ido-enable-last-directory-history t
      ido-enable-tramp-completion t
      ido-use-filename-at-point 'guess
      ido-confirm-unique-completion nil
      ;; max-mini-window-height
      ido-max-window-height nil
      ido-record-commands t
      ido-use-faces t
      )

(set-face-attribute 'ido-first-match nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :weight 'normal)
(set-face-attribute 'ido-only-match nil
                    :foreground "yellow green"
                    :slant 'italic)
(set-face-attribute 'ido-subdir nil
                    :foreground "forest green")
(set-face-attribute 'ido-virtual nil      ; for matching virtual buffer names.
                    :foreground "sky blue")
(set-face-attribute 'ido-indicator nil    ; highlight its indicators.
                    :foreground "yellow")
(set-face-attribute 'ido-incomplete-regexp nil
                    :foreground "dark red" :background "dim gray")


;;; Display Completions Vertically

;; It’s a lot easier to scan long path names if they’re displayed vertically, instead of horizontally. Run this to achieve just that:

;; This can be achieved by installing package ido-vertical-mode, which (presumably), does the following, below.

;;   ;; Display ido results vertically, rather than horizontally
;;   (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;;   (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;;   (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;   (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;     (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;     (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;;   (add-hook 'ido-setup-hook 'ido-define-keys)


;;; find file at point (ffap)


;;; ido-vertical-mode

;;; Usage:
;; - [M-x] :: open minibuffer, init with ido-complete.
;; - [M-n/p] / [C-n/p] :: next/previous match candidate.

(ido-vertical-mode 1)

(setq ido-vertical-define-keys 'C-n-and-C-p-only
      ido-vertical-decorations '("\n⇨ " "" "\n · " "\n   ......"
                                 "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"
                                 "\n〇 " "")
      ido-max-window-height 7 ; ido-vertical-mode height.
      )

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "M-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)))


;; eldoc-eval --- Enable eldoc support when minibuffer is in use. [M-:]

;; Eldoc info is shown by default in mode-line, but you can have eldoc info
;; somewhere else by setting eldoc-in-minibuffer-show-fn to another function
;; (e.g tooltip-show).
(setq eldoc-in-minibuffer-show-fn 'tooltip-show) ; 'eldoc-show-in-mode-line.

;; It also provides a convenient macro to enable eldoc support in your own
;; functions using minibuffer or in your defadvices, that is
;; with-eldoc-in-minibuffer, e.g:
;;
;; (defadvice edebug-eval-expression (around with-eldoc activate)
;;   "This advice enable eldoc support."
;;   (interactive (list (with-eldoc-in-minibuffer
;;                        (read-from-minibuffer
;;                         "Eval: " nil read-expression-map t
;;                         'read-expression-history))))
;;   ad-do-it)

;; Users of own minibuffer frame will have to set
;; `eldoc-in-minibuffer-own-frame-p' to non-nil.

;; You can turn On/Off eldoc support in minibuffer at any time with
;; `eldoc-in-minibuffer-mode'.

;; TODO: this eldoc-eval does not looks great.
;; (autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
;; (eldoc-in-minibuffer-mode 1)





(provide 'init-my-emacs-minibuffer)

;;; init-my-emacs-minibuffer.el ends here
