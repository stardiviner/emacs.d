;;; init-my-prog-lang-database-redis.el --- init for Redis
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ redis ] -- Redis integration

(use-package redis
  :ensure t
  :commands redis-mode
  :init
  ;; for pseudo redis script file: *.redis.
  (add-to-list 'auto-mode-alist '("\\.redis\\'" . redis-mode))
  )


;;; [ eredis ] -- a Redis client in emacs lisp

(use-package eredis
  :ensure t)



(provide 'init-my-prog-lang-database-redis)

;;; init-my-prog-lang-database-redis.el ends here
