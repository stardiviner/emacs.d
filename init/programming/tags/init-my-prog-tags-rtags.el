;;; init-my-prog-tags-rtags.el --- init for rtags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rtags ] -- A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.

(use-package rtags
  :ensure t
  :config
  (setq rtags-path "/usr/bin"
        ;; rtags-socket-file "/run/user/1000/rdm.socket"
        rtags-autostart-diagnostics t
        rtags-completions-enabled t
        rtags-display-current-error-as-message nil
        rtags-display-current-error-as-tooltip t
        ;; rtags-rdm-includes "" ; additional include paths.
        ;; rtags-process-flags "" ; flags for rdm.
        )

  ;; Sets up a ton of standard keybindings under [C-x r] (we try to avoid
  ;; crashing with the register shortcuts).
  (rtags-enable-standard-keybindings c-mode-base-map "C-c C-r")

  ;; start rtags process `rdm'.
  ;; (rtags-start-process-unless-running)
  ;;
  ;; Start the rdm process unless the process is already running. You may create
  ;; hook to automatically call this function upon entering, e.g. c-mode or
  ;; c++mode.
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  )

;;; [ company-rtags ] -- a company-mode backend for rtags.

(use-package company-rtags
  :ensure t
  :config
  (defun my-rtags-settings ()
    (interactive)
    (my-company-add-backend-locally 'company-rtags)
    )
  
  (dolist (hook '(c-mode-hook
                  c++-mode-hook
                  objc-mode-hook))
    (add-hook hook 'my-rtags-settings))
  )

;;; [ flycheck-rtags ] -- RTags Flycheck integration.

(use-package flycheck-rtags
  :ensure t)


(provide 'init-my-prog-tags-rtags)

;;; init-my-prog-tags-rtags.el ends here
