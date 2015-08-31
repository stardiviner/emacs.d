;;; init-my-org-complete.el --- init for Org Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(if (featurep 'helm)
    (setq org-completion-fallback-command 'helm)
  (setq org-completion-fallback-command 'hippie-expand))

;; (if (featurep 'ido-vertical-mode)
;;     (setq org-completion-use-ido t)
;;   (setq org-completion-use-ido nil)
;;   (setq org-completion-use-iswitchb nil)
;;   )


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






(provide 'init-my-org-complete)

;;; init-my-org-complete.el ends here
