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

  ;; quick help
  (setq ac-quick-help-delay 0.2)
  (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  (define-key ac-completing-map (kbd "C-M-n") 'ac-quick-help-scroll-down)
  (define-key ac-completing-map (kbd "C-M-p") 'ac-quick-help-scroll-up)
  
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
