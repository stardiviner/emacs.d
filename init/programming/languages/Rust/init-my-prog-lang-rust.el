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

  (defun my-rust-basic-settings ()
    "Some basic settings for Rust."
    (interactive)
    (eldoc-mode 1)
    (define-key racer-mode-map (kbd "C-c C-d d") 'racer-describe)
    )

  (add-hook 'rust-mode-hook #'my-rust-basic-settings)
  )


;;; [ flycheck-rust ]

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )


;;; [ racer ]

(use-package racer
  :ensure t
  :defer t
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
  :config
  (use-package company-racer
    :ensure t
    :config
    (add-hook 'rust-mode-hook
              (lambda ()
                (my-company-add-backend-locally 'company-racer)))
    )
  )

;;; [ cargo ] -- Emacs Cargo client.

(use-package cargo
  :ensure t
  :defer t)

;;; [ ob-rust ] -- Org-mode Babel support for Rust.

(use-package ob-rust
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(rust . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("rust" . "rs"))
  )


(provide 'init-my-prog-lang-rust)

;;; init-my-prog-lang-rust.el ends here
