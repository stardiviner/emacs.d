;;; init-prog-lang-database-redis.el --- init for Redis
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ redis ] -- Redis integration

(use-package redis
  :ensure t
  :ensure-system-package (redis-cli . "sudo pacman -S --noconfirm redis")
  :defer t
  :commands (redis-mode)
  ;; for pseudo redis script file: *.redis.
  :init (add-to-list 'auto-mode-alist '("\\.redis\\'" . redis-mode))
  :config (add-hook 'redis-mode-hook 'sqlup-mode))


;;; [ eredis ] -- a Redis client in emacs lisp

(use-package eredis
  :ensure t
  :defer t)

;;; [ ob-redis ]

(require 'ob-redis)
(add-to-list 'org-babel-load-languages '(redis . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("redis" . "redis"))


(provide 'init-prog-lang-database-redis)

;;; init-prog-lang-database-redis.el ends here
