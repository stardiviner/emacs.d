;;; init-my-prog-lang-coffeescript.el --- init for CoffeeScript
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ coffee-mode ]

(use-package coffee-mode
  :ensure t
  :defer t
  :config
  ;; (setq coffee-tab-width 4)
  (setq coffee-args-compile '("-c")
        coffee-args-repl '("-i"))

  (with-eval-after-load "coffee-mode"
    (define-key coffee-mode-map [f5] 'coffee-compile-buffer)
    (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent))

  ;; Move to corresponding point in JavaScript file after compiling
  ;; You can archive this with `sourcemap' and following configuration.
  ;; (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
  ;; (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

  ;; ;; If you want to remove sourcemap file after jumping corresponding point
  ;; (defun my/coffee-after-compile-hook (props)
  ;;   (sourcemap-goto-corresponding-point props)
  ;;   (delete-file (plist-get props :sourcemap)))
  ;; (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)
  )


;;; [ ob-coffee ]

(use-package ob-coffee
  :ensure t)


(provide 'init-my-prog-lang-coffeescript)

;;; init-my-prog-lang-coffeescript.el ends here
