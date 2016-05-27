;;; init-my-prog-lang-rust.el --- init for Rust
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rust-mode ]

(use-package rust-mode
  :ensure t
  :config
  (setq rust-indent-offset 4
        rust-indent-method-chain t
        rust-indent-where-clause t
        rust-match-angle-brackets t
        )
  )


;;; [ flycheck-rust ]

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )


;;; [ racer ]

(use-package racer
  :ensure t
  :config
  ;; config racer in the path.
  (unless (getenv "RUST_SRC_PATH")
    (setenv "RUST_SRC_PATH" (expand-file-name "~/Code/Rust/rust/src")))
  ;; (setq racer-cmd (or (executable-find "racer") "/usr/local/bin/racer"))
  ;; (setq racer-rust-src-path (or (getenv "RUST_SRC_PATH")
  ;;                               "/usr/local/src/rust/src"))
  
  ;; auto start racer for rust-mode.
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  )


;;; [ company-racer ]

;; (use-package company-racer
;;   :ensure t
;;   :config
;;   (add-hook 'rust-mode-hook
;;             (lambda ()
;;               (my-company-add-backend-locally 'company-racer))))


;;; [ cargo ] -- Emacs Cargo client.

(use-package cargo
  :ensure t)


(provide 'init-my-prog-lang-rust)

;;; init-my-prog-lang-rust.el ends here
