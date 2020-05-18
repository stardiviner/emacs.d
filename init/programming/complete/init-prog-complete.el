;;; init-prog-complete.el --- init for Programming Completion
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq completion-ignore-case t)

;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(use-package pcomplete
  :defer t
  :init (setq pcomplete-ignore-case t))

;;; [ auto-complete ] -- Auto Completion for GNU Emacs

(use-package auto-complete
  :ensure t
  :defer t
  :commands (auto-complete-mode global-auto-complete-mode)
  ;; :init (global-auto-complete-mode 1) ; use auto-complete globally
  :config
  ;; load `ac-source-yasnippet'
  (require 'auto-complete-config)
  ;; set default auto-complete source
  (setq-default ac-sources
                '(ac-source-yasnippet
                  ac-source-abbrev
                  ;; ac-source-dabbrev
                  ;; ac-source-dictionary
                  ac-source-words-in-same-mode-buffers))
  ;; auto raise popup menu
  (setq ac-auto-show-menu t)
  ;; quick help
  (setq ac-quick-help-delay 0.2)
  (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  (define-key ac-completing-map (kbd "C-M-n") 'ac-quick-help-scroll-down)
  (define-key ac-completing-map (kbd "C-M-p") 'ac-quick-help-scroll-up)

  ;; [ ac-capf ] -- auto-complete source of completion-at-point
  ;; (use-package ac-capf
  ;;   :ensure t
  ;;   :init (ac-capf-setup) ; global
  ;;   :config (add-to-list 'ac-sources 'ac-source-capf))
  )

(require 'init-company-mode)
;; (require 'init-jetbrains)


(provide 'init-prog-complete)

;;; init-prog-complete.el ends here
