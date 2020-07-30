;;; init-emacs-edit-server.el --- init for Emacs Edit Server
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_ [ edit-server ]

;; A Chrome "clone" of It's All Text for spawning an editor to edit text areas in browsers with Emacs.
;; https://github.com/stsquad/emacs_chrome

(use-package edit-server
  :ensure t
  :ensure edit-server-htmlize
  :if window-system
  :preface (unless (server-running-p)
             (server-start))
  :hook ((after-init . edit-server-start)
         (edit-server-edit-mode . flyspell-mode))
  :config
  (setq edit-server-default-major-mode 'markdown-mode)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ;; Stack Overflow
          ("stackoverflow\\.com" . markdown-mode)
          ("stack\\(exchange\\|overflow\\)\\.com\\.[a-z0-9]+\\.txt" . markdown-mode)
          (".*\\.stackexchange\\.com/.*" . markdown-mode)
          ("segmentfault\\.com" . markdown-mode)
          ("discuss\\.elastic\\.co" . markdown-mode)
          ;; Gmail
          (".*mail\\.google\\.com/.*" . html-mode)
          ;; Jupyter notebooks
          ;; localhost:8888/notebooks/Untitled1.ipynb?kernel_name=clojure
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=clojure" . clojure-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=ruby" . ruby-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=julia" . julia-mode)
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=python" . python-mode)))
  
  ;; set `emacsclient' and `edit-server' new frame transparency.
  (add-to-list 'edit-server-new-frame-alist (cons 'alpha (list 85 70)))
  (add-hook 'edit-server-edit-mode-hook
            (lambda () (set-frame-parameter (selected-frame) 'alpha (list 85 70)))))

;;; [ atomic-chrome ] -- with websocket improvement on `edit-server'.

(use-package atomic-chrome
  :ensure t
  :defer t
  :bind (:map atomic-chrome-edit-mode-map ("C-x #" . atomic-chrome-close-current-buffer))
  :hook ((after-init . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . flyspell-mode))
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist
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
          ("localhost:8888/notebooks/.*\\.ipynb\\?kernel_name=python" . python-mode))))

;;; [ with-editor ]

(use-package with-editor
  :ensure t
  :defer t
  :hook ((shell-mode . with-editor-export-editor)
         (term-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))


(provide 'init-emacs-edit-server)

;;; init-emacs-edit-server.el ends here
