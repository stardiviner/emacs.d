;;; init-prog-lang-rust.el --- init for Rust
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rust-mode ]

(use-package rust-mode
  :ensure t
  :defer t
  :init (setq rust-indent-method-chain t
              rust-indent-where-clause t
              rust-match-angle-brackets t))

;;; [ flycheck-rust ]

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :commands (flycheck-rust-setup)
  :hook ((rust-mode . flycheck-mode) (flycheck-mode . flycheck-rust-setup)))

;;; [ racer ] -- code completion, goto-definition and docs browsing for Rust via racer.

(use-package racer
  :ensure t
  :hook (rust-mode . racer-mode)
  :config
  (define-key racer-mode-map (kbd "C-c C-d C-d") 'racer-describe)
  (add-to-list 'display-buffer-alist
               '("^\\*Racer Help\\*" . (display-buffer-below-selected))))

;;; [ cargo ] -- Emacs Cargo client.

(use-package cargo
  :ensure t
  :defer t
  :commands (cargo-minor-mode)
  :hook (rust-mode . cargo-minor-mode))

;;; [ ob-rust ] -- Org-mode Babel support for Rust.

(use-package ob-rust
  :ensure t
  :defer t
  :commands (org-babel-execute:rust)
  :config
  (add-to-list 'org-babel-load-languages '(rust . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("rust" . "rs")))

;;; [ lsp-rust ] Rust support for lsp-mdoe.

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp))

;;; [ ron-mode ] -- Rusty Object Notation mode.

(use-package ron-mode
  :ensure t)


(provide 'init-prog-lang-rust)

;;; init-prog-lang-rust.el ends here
