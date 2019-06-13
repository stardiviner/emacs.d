;;; init-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq mode-line-in-non-selected-windows t)

;; (require 'init-custom-mode-line)
;; (require 'init-powerline)

;;; [ mood-line ] -- a minimal mode-line configuration that aims to replicate some of the features of the doom-modeline.

;; (use-package mood-line
;;   :ensure t
;;   :hook (after-init . mood-line-mode))

;;; [ doom-modeline ] -- A minimal and modern mode-line.

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-buffer-file-name-style 'buffer-name
              doom-modeline-icon t ; don't use icon will be faster
              doom-modeline-github nil
              doom-modeline-irc nil)
  :config
  ;; doom-modeline segment replace IRC buffer name to icons.
  ;; https://github.com/seagle0128/doom-modeline/pull/110
  (defun smf/irc-icons (buffer)
    "Given a BUFFER name, return an icon. Else return buffer."
    (cond
     ((string-match "#mercurial" buffer)
      (all-the-icons-faicon "mercury" :v-adjust .05))
     ((string-match "#bitbucket" buffer)
      (all-the-icons-faicon "bitbucket" :v-adjust .05))
     ((string-match "#octobus-hg" buffer)
      ;; this inserts a custom fonticon, in this case, octobus
      (propertize "\xe900"
                  'face '(:family "smf-custom-icons")
                  'rear-nonsticky t
                  'display '(raise -0.1)
                  'font-lock-ignore t))
     (t buffer)))
  (setq doom-modeline-irc-stylize #'smf/irc-icons))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
