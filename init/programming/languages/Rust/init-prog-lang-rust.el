;;; init-prog-lang-rust.el --- init for Rust
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rust-mode ]

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-indent-offset 4
        rust-indent-method-chain t
        rust-indent-where-clause t
        rust-match-angle-brackets t
        )
  (add-hook 'rust-mode-hook #'eldoc-mode)
  )


;;; [ flycheck-rust ]

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )


;;; [ racer ]

(use-package racer
  :ensure t
  :ensure-system-package (racer . "cargo install racer") ; "sudo pacman -S --noconfirm rust-racer"
  :init
  ;; config racer in the path.
  (let ((rust-src-path (getenv "RUST_SRC_PATH")))
    (if (and rust-src-path (file-directory-p rust-src-path))
        (setenv "RUST_SRC_PATH" (expand-file-name "~/Code/Rust/rust/src"))
      (warn (format "Rust SRC path %s does not exist." rust-src-path))))
  ;; (setq racer-cmd (or (executable-find "racer") "/usr/local/bin/racer"))
  ;; (setq racer-rust-src-path (or (getenv "RUST_SRC_PATH")
  ;;                               "/usr/local/src/rust/src"))
  
  ;; auto start racer for rust-mode.
  (add-hook 'rust-mode-hook #'racer-mode)
  (use-package company-racer
    :ensure t
    :init
    (add-hook 'rust-mode-hook
              (lambda () (my-company-add-backend-locally 'company-racer))))
  :config
  (define-key racer-mode-map (kbd "C-c C-d C-d") 'racer-describe)
  (add-to-list 'display-buffer-alist
               '("^\\*Racer Help\\*" . (display-buffer-below-selected)))
  )

;;; [ cargo ] -- Emacs Cargo client.

(use-package cargo
  :ensure t
  :defer t)

;;; [ ob-rust ] -- Org-mode Babel support for Rust.

(use-package ob-rust
  :ensure t
  :defer t
  :init
  (add-to-list 'org-babel-load-languages '(rust . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("rust" . "rs"))
  )

;;; [ lsp-rust ] Rust support for lsp-mdoe.

(use-package lsp-rust
  :ensure t
  :ensure-system-package ((rls . "rustup component add rls-preview rust-analysis rust-src"))
  :after lsp-mode
  :hook (rust-mode . lsp-rust-enable))


(provide 'init-prog-lang-rust)

;;; init-prog-lang-rust.el ends here
