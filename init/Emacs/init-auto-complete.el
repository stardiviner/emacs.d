;;; init-auto-complete.el --- init auto-complete.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ auto-complete ]

(use-package auto-complete
  :ensure t
  :defer t
  :commands (auto-complete-mode global-auto-complete-mode)
  ;; :init (global-auto-complete-mode 1) ; use auto-complete globally
  :config
  ;; auto raise popup menu
  (setq ac-auto-show-menu t)

  ;; keybindings
  ;; (ac-set-trigger-key "<tab>") ; <tab> is used for yasnippet.
  ;; (ac-set-trigger-key "TAB") ; usualy this, <tab> has higher priority than TAB.
  ;; (define-key global-map (kbd "M-TAB") 'ac-fuzzy-complete) ; fuzzy complete.

  ;; ac-menu-map keymap only map for menu is available, not break default.
  (setq ac-use-menu-map t)
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
  (define-key ac-menu-map (kbd "RET") 'my-ac-return)

  ;; quick help
  (setq ac-quick-help-delay 0.2)

  (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  (define-key ac-completing-map (kbd "C-M-n") 'ac-quick-help-scroll-down)
  (define-key ac-completing-map (kbd "C-M-p") 'ac-quick-help-scroll-up)
  (define-key ac-completing-map [C-down] 'ac-quick-help-scroll-down)
  (define-key ac-completing-map [C-up] 'ac-quick-help-scroll-up)

  ;; auto-complete default setup
  ;; (ac-config-default)
  
  ;; load `ac-source-yasnippet'
  (require 'auto-complete-config)
  ;; set default auto-complete source
  (setq-default ac-sources
                '(ac-source-yasnippet
                  ac-source-abbrev
                  ;; ac-source-dabbrev
                  ;; ac-source-dictionary
                  ac-source-words-in-same-mode-buffers)))


;;; [ ac-capf ] -- auto-complete source of completion-at-point

;; (use-package ac-capf
;;   :ensure t
;;   :init (ac-capf-setup) ; global
;;   :config (add-to-list 'ac-sources 'ac-source-capf))


(defun my/ac-source-remove (source-removed-list)
  "remove some ac-source from ac-sources."
  (mapc (lambda (x) (setq-local ac-sources (remq x ac-sources)))
        source-removed-list))



(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
