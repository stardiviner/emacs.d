;;; init-my-prog-lang-javascript.el --- init JavaScript for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; http://www.emacswiki.org/emacs/JavaScript

;;; Code:

;;; [ javascript-mode ]

(eval-after-load 'js-mode
  '(progn
     (add-hook 'js-mode-hook
               (lambda ()
                 (electric-layout-mode -1) ; electric-layout-mode doesn't play nice with js-mode.
                 ))))


;;; [ js2-mode ]

;; (autoload 'js2-mode "js2-mode" nil t)
;;
;; (eval-after-load 'auto-complete
;;   (add-to-list 'ac-modes 'js2-mode))




(provide 'init-my-prog-lang-javascript)

;;; init-my-prog-lang-javascript.el ends here
