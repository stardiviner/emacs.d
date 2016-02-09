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


(provide 'init-my-prog-lang-rust)

;;; init-my-prog-lang-rust.el ends here
