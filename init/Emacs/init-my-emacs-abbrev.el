;;; init-my-emacs-abbrev.el --- abbrev mode init

;;; Commentary:

;;; [ abbrev-mode ]
;;; Usage:
;;; - type abbrev string, then press [Tab] to expand.
;;; - or select in candidates menu framework like auto-complete, then press [Tab] to expand.
;;; - define abbrev
;;;   type the word you want to use as expansion, then type [C-x a g] and the abbreviation for it.
;;; - save abbrevs
;;;   [M-x write-abbrev-file]
;;; - restore abbrevs
;;;   [M-x read-abbrev-file]
;;; - Abbrev-mode
;;;   [M-x abbrev-mode]

;;; Code:

(require 'abbrev)

(setq abbrev-file-name "~/.emacs.d/my-init/abbrevs/abbrev_defs")
(setq save-abbrevs t)   ; save abbrevs when files are saved, nil: stop asking.
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file) ; reads the abbreviations file on startup quietly.
    )

(setq-default abbrev-mode t)            ; turn on abbrev mode globally.

;;; [ dabbrev ]
;; Usage:
;; - [M-/] -- dabbrev-expand
(require 'dabbrev)

(provide 'init-my-emacs-abbrev)

;;; init-my-emacs-abbrev.el ends here
