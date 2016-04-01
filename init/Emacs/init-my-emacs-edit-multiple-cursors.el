;;; init-my-emacs-edit-multiple-cursors.el --- init for multiple cursors
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ [ iedit ] -- Edit multiple regions simultaneously in a buffer or a region
;;;
;;; This package includes Emacs minor modes (iedit-mode and
;;; iedit-rectangle-mode) based on a API library (iedit-lib) and allows you to
;;; edit one occurrence of some text in a buffer (possibly narrowed) or region,
;;; and simultaneously have other occurrences edited in the same way, with
;;; visual feedback as you type.
;;
;; Normal scenario of Iedit mode is like:
;;
;; 1. Highlight certain contents - by press [C-;] All occurrences of a symbol,
;;    string or a rectangle in the buffer or a region may be highlighted
;;    corresponding to current mark, point and prefix argument. Refer to the
;;    document of `iedit-mode’ for details.
;;
;; 2. Edit one of the occurrences The change is applied to other occurrences
;;    simultaneously.
;;
;; 3. Finish - by pressing [C-;] again
;;
;; You can also use Iedit mode as a quick way to temporarily show only the
;; buffer lines that match the current text being edited. This gives you the
;; effect of a temporary `keep-lines’ or `occur’. To get this effect, hit C-’
;; when in Iedit mode - it toggles hiding non-matching lines.
;;
;; Renaming refactoring is convinient in Iedit mode
;; - The symbol under point is selected as occurrence by default and only complete symbols are matched
;; - With digit prefix argument 0, only occurrences in current function are matched
;; - Restricting symbols in current region can be done by pressing C-; again
;; - Last renaming refactoring is remembered and can be applied to other buffers later
;; - Restricting the search area to just the current line can be done by pressing M-I.
;; - Restricting the search area to the lines near the current line can be done
;;   by pressing M-{ and M-}. These will expand the search region one line at a
;;   time from the top and bottom. Add a prefix argument to go the opposite
;;   direction.

;;; Iedit-rectangle-mode provides rectangle support with visible rectangle
;;; highlighting, which is similar with cua mode rectangle support. But it’s
;;; lighter weight and uses iedit mechanisms.

;;; There are also some other facilities you may never think about. Refer to the
;;; document of function `iedit-mode’ (C-h f iedit-mode RET) for more details.

;;; Usage:
;;
;; - [C-h iedit-mode RET] -- to get help of iedit-mode
;; - [M-x iedit-mode]
;;
;; - [C-;] -- highlight certain contents
;; - [C-'] -- toggle unmatched lines visible
;; - [M-;] -- apply global modification
;;
;; - [Tab] -- next occurrence
;; - [S-Tab] -- prev occurrence
;; - [M-<] -- first occurrence
;; - [M->] -- last  occurrence
;;
;; - [M-b] -- toggle buffering
;; - [M-c] -- toggle case sensitive
;;
;; - [M-d] -- restrict function
;;
;; - [M-d] -- delete occurrences
;; - [M-SPC] -- blank occurences
;; - [M-l] -- downcase occurrences
;; - [M-u] -- upcase occurrences
;; - [M-n] -- number occurrences
;; - [M-r] -- replace occurrences
;;
;; --------------------------------
;;
;; - [M-x iedit-rectangle-mode] -- visible rectangle.
;; - [M-k] -- Iedit kill rectangle.
;; Steps:
;; - mark a rectangle like Emacs rectangle with [C-@ / C-SPC].
;; - after marked the rectangle, then press [C-c C-;] to enable iedit rectangle mode, and highlight the rectangle.

(setq iedit-occurrence-face 'isearch) ; 'highlight

(defun iedit-dwim (arg)
  "If ARG, start iedit but use \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

;; (global-set-key (kbd "C-;") 'iedit-mode)
(define-key my-edit-prefix (kbd "e") 'iedit-dwim)


;;;_ [ multiple-cursors ]

;;; Usage:
;;
;; https://github.com/magnars/multiple-cursors.el
;; - [C-c c] -- prefix of mc.
;; - <return> / [C-g] -- get out of multiple-cursors
;; - [C-c e c c] -- `mc/mark-next-like-this'
;; - [C-c e c r] -- `set-rectangular-region-anchor'
;; - [region] + [C-c e c m] -- mark variants (region is used to limit mark area)
;;
;; Unknown commands
;;
;; Multiple-cursors uses two lists of commands to know what to do: the run-once
;; list and the run-for-all list. It comes with a set of defaults, but it would
;; be beyond silly to try and include all the known Emacs commands.
;;
;; So that's why multiple-cursors occasionally asks what to do about a
;; command. It will then remember your choice by saving it in
;;
;;     `~/.emacs.d/.mc-lists.el' You can change the location with:


(use-package multiple-cursors
  :ensure t
  :config

  ;; (setq mc/list-file (expand-file-name ".mc-lists.el" user-emacs-directory))

  ;; (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode)

  ;; (setq mc/keymap "C-c c")

  (define-key mc/keymap (kbd "C-'") 'mc-hide-unmatched-lines-mode)
  
  (unless (boundp 'my-mc-prefix)
    (define-prefix-command 'my-mc-prefix))
  (define-key my-edit-prefix (kbd "c") 'my-mc-prefix)

  (define-key my-mc-prefix (kbd "c") 'mc/mark-all-dwim)
  (define-key my-mc-prefix (kbd "r") 'set-rectangular-region-anchor)
  (define-key my-mc-prefix (kbd "l") 'mc/edit-lines)
  (define-key my-mc-prefix (kbd "a") 'mc/edit-beginnings-of-lines)
  (define-key my-mc-prefix (kbd "e") 'mc/edit-ends-of-lines)
  (define-key my-mc-prefix (kbd "n") 'mc/insert-numbers)
  (define-key my-mc-prefix (kbd "s") 'mc/sort-regions)
  (define-key my-mc-prefix (kbd "R") 'mc/reverse-regions)

  (unless (boundp 'my-mc/mark-prefix)
    (define-prefix-command 'my-mc/mark-prefix))
  (define-key my-mc-prefix (kbd "m") 'my-mc/mark-prefix)

  (define-key my-mc/mark-prefix (kbd "a a") 'mc/mark-all-like-this-dwim)
  (define-key my-mc/mark-prefix (kbd "a l") 'mc/mark-all-like-this)
  (define-key my-mc/mark-prefix (kbd "a w") 'mc/mark-all-words-like-this)
  (define-key my-mc/mark-prefix (kbd "a s") 'mc/mark-all-symbols-like-this)
  (define-key my-mc/mark-prefix (kbd "a r") 'mc/mark-all-in-region)
  (define-key my-mc/mark-prefix (kbd "a f") 'mc/mark-all-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "a F") 'mc/mark-all-words-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "a S") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key my-mc/mark-prefix (kbd "t") 'mc/mark-sgml-tag-pair)

  (define-key my-mc/mark-prefix (kbd "n n") 'mc/mark-next-like-this)
  (define-key my-mc/mark-prefix (kbd "n w") 'mc/mark-next-word-like-this)
  (define-key my-mc/mark-prefix (kbd "n s") 'mc/mark-next-symbol-like-this)
  (define-key my-mc/mark-prefix (kbd "p p") 'mc/mark-previous-like-this)
  (define-key my-mc/mark-prefix (kbd "p w") 'mc/mark-previous-word-like-this)
  (define-key my-mc/mark-prefix (kbd "p s") 'mc/mark-previous-symbol-like-this)

  (if (featurep 'visual-regexp)
      (define-key my-mc/mark-prefix (kbd "v") 'vr/mc-mark))
  ;; `vr/select-mc-mark', `vr/select-replace', `vr/select-query-replace' etc.

  ;; First mark the word, then add more cursors.

  ;; To get out of multiple-cursors-mode, press <return> or C-g. The latter will
  ;; first disable multiple regions before disabling multiple cursors. If you want
  ;; to insert a newline in multiple-cursors-mode, use [C-j].

  ;; (setq mc/mode-line '("mc:"
  ;;                      (:eval
  ;;                       (format
  ;;                        #("%d" 0 2
  ;;                          (face font-lock-warning-face))
  ;;                        (mc/num-cursors)))))

  (set-face-attribute 'mc/cursor-face nil
                      :inverse-video nil
                      :background "dark red")
  (set-face-attribute 'mc/region-face nil
                      :inverse-video nil
                      :background (color-darken-name (face-background 'default) 4))
  )



(provide 'init-my-emacs-edit-multiple-cursors)

;;; init-my-emacs-edit-multiple-cursors.el ends here
