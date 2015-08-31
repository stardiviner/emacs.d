;;; init-my-prog-complete-ycmd.el --- init for Emacs's ycmd
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-ycmd ] -- YouCompeleteMe server/daemon

;;; Usage:
;;
;;

(require 'ycmd)

;; use ycmd-mode in all supported modes
(ycmd-setup)
;; Or add ycmd-mode to a specific supported mode:
;; (add-hook 'c++-mode-hook 'ycmd-mode)

;; Use the variable ycmd-server-command to specify how to run the server. It
;; will typically be something like:
(set-variable ycmd-server-command '("python" "~/compile/Emacs/ycmd"))

;; If you've got a global ycmd configuration, specify that in your emacs
;; configuration by setting ycmd-global-config:
;;
;; (set-variable 'ycmd-global-config "/path/to/global_config.py")


;;; for company-mode
(if (functionp 'company-mode)
    (lambda ()
      (require 'company-ycmd)
      (company-ycmd-setup)))

;;; flycheck integration

;; flycheck-ycmd.el allows you to use ycmd as a backend for flycheck. With this
;; enabled, whenever ycmd parses a file the results will be passed to flycheck
;; for display. This is a really nice way to quick feedback on problems in your
;; code.

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
;; This will make sure that flycheck sees the parse results, and that the
;; flycheck-ycmd backend is enabled.

;;; If for some reason you want to do this manually, the instructions are like this:
;;
;; (require 'flycheck-ycmd)
;;
;; ;; Make sure the flycheck cache sees the parse results
;; (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
;;
;; ;; Add the ycmd checker to the list of available checkers
;; (add-to-list 'flycheck-checkers 'ycmd)

;;; Disabling ycmd-based flycheck for specific modes
;;
;; (add-hook 'python-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))

;;; `next-error' integration
;;
;; emacs-ycmd reports found errors through emacs buttons; to integrate those
;; with next-error prepend something like (require 'ycmd-next-error) before
;; require'ing ycmd (after adding the contrib directory to your load-path).



(provide 'init-my-prog-complete-ycmd)

;;; init-my-prog-complete-ycmd.el ends here
