;;; init-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ modify Emacs default mode-line ]

(defun mode-line-align (left right)
  "Return a string with LEFT and RIGHT at the edges of the
current window."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left right))

(setq-default mode-line-format
              '(:eval
                (mode-line-align
                 (format-mode-line
                  (list "%e" mode-line-front-space
                        " " mode-line-client mode-line-remote mode-line-mule-info mode-line-modified
                        " " mode-line-buffer-identification
                        " " mode-line-position
                        " " '(vc-mode vc-mode)
                        " " mode-line-process))
                 (format-mode-line
                  (list " " mode-line-misc-info
                        " " mode-line-modes
                        ;; " " mode-name minor-mode-alist
                        " " mode-line-end-spaces)))))

;;; [ powerline ] -- Rewrite of Powerline.

;; (require 'init-powerline)

;;; [ mood-line ] -- a minimal mode-line configuration that aims to replicate some of the features of the doom-modeline.

;; (use-package mood-line
;;   :ensure t
;;   :hook (after-init . mood-line-mode))

;;; [ doom-modeline ] -- A minimal and modern mode-line.

;; (use-package doom-modeline
;;   :ensure t
;;   :custom ((doom-modeline-buffer-file-name-style 'buffer-name)
;;            (doom-modeline-icon t) ; don't use icon will be faster
;;            (doom-modeline-github nil)
;;            ;; Fix the laggy issue, by don't compact font caches during GC.
;;            (inhibit-compacting-font-caches t))
;;   :hook (after-init . doom-modeline-mode))


(provide 'init-emacs-mode-line)

;;; init-emacs-mode-line.el ends here
