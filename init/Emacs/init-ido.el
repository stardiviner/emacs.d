;;; init-ido.el --- init ido settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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
(require 'ido-ubiquitous)

(ido-mode t)                            ; enable ido mode
(ido-everywhere t)                      ; use ido-mode wherever possible
(ido-ubiquitous-mode t)                 ; enable ido-ubiquitous

(setq ido-everywhere t
      ido-default-buffer-method 'selected-window ; allow buffer to be open in different frames
      ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
      ido-enable-regexp t
      ido-enable-flex-matching t        ; enable fuzzy search
      ido-enable-prefix nil ; match if the entered text is an arbitrary substring.
      ido-enable-dot-prefix t ; to match hidden files and directories even `ido-enable-prefix' is nil.
      ido-create-new-buffer 'prompt
      ido-use-filename-at-point 'guess  ; look for filename at point
      ido-max-prospects 10
      ido-use-virtual-buffers t         ; allow me to open closed buffers, even
      ido-enable-last-directory-history t
      ido-enable-tramp-completion t
      ido-use-filename-at-point 'guess
      ido-confirm-unique-completion nil
      ido-auto-merge-work-directories-length -1
      ido-max-window-height nil
      ido-record-commands t
      ido-record-ftp-work-directories t
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



;;; [ ido-vertical-mode ] -- vertical ido.

;;; Usage:
;; - [M-x]
;; - [C-n/p]

(eval-after-load 'ido
  (require 'ido-vertical-mode)
  (ido-vertical-mode 1))

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

;; ---------------------------------------------------------------------------------
;; (setq ido-decorations '("{" "}" " | " " | ..." "[" "]"
;;                         " [No match]" " [Matched]" " [Not readable]"
;;                         " [Too big]" " [Confirm]")
;;       ido-vertical-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
;;                                  " [No match]" " [Matched]" " [Not readable]"
;;                                  " [Too big]" " [Confirm]"
;;                                  "\n-> " "")
;;       )
;; ---------------------------------------------------------------------------------


;;; [ flx-ido ]

;; (require 'flx-ido)


;;; [ smex ] --- remember recently and most frequently used commands.

;; work with ido will show recent frequently used commands in [M-x].

(require 'smex)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)




(provide 'init-ido)

;;; init-ido.el ends here
