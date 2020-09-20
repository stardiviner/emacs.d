;;; init-tool-irc.el --- init for IRC.

;;; Commentary:



;;; Code:

;;; [ Circe ] -- have sane defaults, and integrates well with the rest of the editor.

(use-package circe
  :ensure t
  :defer t
  :commands (circe)
  :custom ((circe-default-nick "stardiviner")
           (circe-network-options `(("Freenode"
                                     :user "stardiviner"
                                     :pass ,(my/json-read-value my/account-file 'irc)
                                     :use-tls t
                                     :channels ("#emacs"))))
           (circe-channels '("#emacs"))
           (circe-reduce-lurker-spam t)
           (circe-format-say "{nick:-10s} {body}") ; align nick names and messages.
           ;; spelling checking
           (lui-flyspell-p t)
           (lui-flyspell-alist '(("#hamburg" "german8") (".*" "american"))))
  :config
  (define-key circe-mode-map (kbd "C-c SPC") 'tracking-next-buffer)
  ;; words completion
  (defun my/circe-company-setup ()
    (my-company-add-backend-locally 'company-ispell))
  (add-hook 'circe-channel-mode-hook #'my/circe-company-setup)
  ;; auto use paste service for long single line.
  ;; (require 'lui-autopaste)
  ;; (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  ;; Logging
  (require 'lui-logging)
  (enable-lui-logging-globally)
  ;; track bar
  (enable-lui-track-bar))

;;; [ circe-notifications ] -- Add desktop notifications to Circe.

(use-package circe-notifications
  :ensure t
  :defer t
  :hook (circe-server-connected . enable-circe-notifications))


(provide 'init-tool-irc)

;;; init-tool-irc.el ends here
