;;; init-Redis.el --- init for Redis
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ redis ] -- Redis integration

(use-package redis
  :ensure t
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

(use-package ob-redis
  :defer t
  :commands (org-babel-execute:redis)
  :config
  (add-to-list 'org-babel-load-languages '(redis . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("redis" . "redis")))


(provide 'init-Redis)

;;; init-Redis.el ends here
