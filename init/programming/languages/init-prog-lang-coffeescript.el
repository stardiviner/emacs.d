;;; init-prog-lang-coffeescript.el --- init for CoffeeScript
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ coffee-mode ]

(use-package coffee-mode
  :ensure t
  :defer t
  :init
  (with-eval-after-load "coffee-mode"
    (define-key coffee-mode-map [f5] 'coffee-compile-buffer)
    (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent))
  :config
  ;; (setq coffee-tab-width 4)
  (setq coffee-args-compile '("-c")
        coffee-args-repl '("-i"))

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
  :ensure t
  :defer t
  :commands (org-babel-execute:coffee)
  :config
  (add-to-list 'org-babel-load-languages '(coffee . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("coffee" . "coffee")))


(provide 'init-prog-lang-coffeescript)

;;; init-prog-lang-coffeescript.el ends here
