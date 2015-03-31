;;; init-my-emacs-highlight.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; highlight-quoted

(load-file (expand-file-name "init/extensions/highlight-quoted.el" user-emacs-directory))

(add-hook 'prog-mode-hook 'highlight-quoted-mode)

;; (setq highlight-quoted-highlight-symbols t)

(eval-after-load 'highlight-quoted
  (progn
    (set-face-attribute 'highlight-quoted-quote nil
                        :inherit 'font-lock-keyword-face)
    (set-face-attribute 'highlight-quoted-symbol nil
                        :inherit 'font-lock-constant-face))
  )


;;; highlight-numbers

(load-file (expand-file-name "init/extensions/highlight-numbers.el" user-emacs-directory))

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; (setq highlight-numbers-modelist)

(eval-after-load 'highlight-numbers
  (set-face-attribute 'highlight-numbers-number nil
                      :inherit 'font-lock-constant-face))




(provide 'init-my-emacs-highlight)

;;; init-my-emacs-highlight.el ends here
