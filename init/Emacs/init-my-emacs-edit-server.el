;;; init-my-emacs-edit-server.el --- init for Emacs Edit Server
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;;_ [ edit-server ]

;; A Chrome "clone" of It's All Text for spawning an editor to edit text areas in browsers with Emacs.
;; https://github.com/stsquad/emacs_chrome

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (unless (server-running-p)
    (server-start))
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setq edit-server-new-frame t)
  
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ;; Stack Overflow
          ("stackoverflow\\.com" . markdown-mode)
          ("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" . markdown-mode)
          (".*\\.stackexchange\\.com/.*" . markdown-mode)
          ("segmentfault\\.com" . markdown-mode)
          ;; Jupyter notebooks
          ;; localhost:8888/notebooks/Untitled1.ipynb?kernel_name=clojure
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=clojure" . clojure-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=ruby" . ruby-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=julia" . julia-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=python" . python-mode)
          ))
  )

;;; [ atomic-chrome ] -- with websocket improvement on `edit-server'.

;; (use-package atomic-chrome
;;   :ensure t
;;   :config
;;   (setq atomic-chrome-default-major-mode 'markdown-mode
;;         atomic-chrome-url-major-mode-alist
;;         '(("github\\.com" . markdown-mode)
;;           ;; Stack Overflow
;;           ("stackoverflow\\.com" . markdown-mode)
;;           ("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" . markdown-mode)
;;           (".*\\.stackexchange\\.com/.*" . markdown-mode)
;;           ("segmentfault\\.com" . markdown-mode)
;;           ;; Jupyter notebooks
;;           ;; localhost:8888/notebooks/Untitled1.ipynb?kernel_name=clojure
;;           ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=clojure" . clojure-mode)
;;           ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=ruby" . ruby-mode)
;;           ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=julia" . julia-mode)
;;           ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=python" . python-mode)
;;           ))
;;
;;   (atomic-chrome-start-server)
;;
;;   (define-key atomic-chrome-edit-mode-map (kbd "C-x #") 'atomic-chrome-close-current-buffer)
;;   )


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-edit-server)

;;; init-my-emacs-edit-server.el ends here
