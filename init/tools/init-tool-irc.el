;;; init-tool-irc.el --- init for IRC.

;;; Commentary:



;;; Code:

;;; ERC is a powerful, modular, and extensible IRC client for Emacs.
;; (require 'init-erc)

;;; [ Circe ] -- have sane defaults, and integrates well with the rest of the editor.

(use-package circe
  :ensure t
  :defer t
  :commands (circe)
  :config
  ;; user and servers info
  (setq circe-default-nick "stardiviner"
        circe-network-options `(("Freenode"
                                 :user "stardiviner"
                                 :pass ,(my/json-read-value my/account-file 'erc)
                                 :use-tls t
                                 :channels ("#emacs" "#clojure"))))
  ;; ignore
  (setq circe-reduce-lurker-spam t)
  ;; view
  (setq circe-format-say "{nick:-10s} {body}") ; align nick names and messages.
  (define-key circe-mode-map (kbd "C-c SPC") 'tracking-next-buffer)
  ;; spelling checking
  (setq lui-flyspell-p t
        lui-flyspell-alist '(("#hamburg" "german8")
                             (".*" "american")))
  ;; auto use paste service for long single line.
  ;; (require 'lui-autopaste)
  ;; (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  ;; Logging
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  ;; track bar
  (enable-lui-track-bar))

(define-key tools-prefix (kbd "i") 'circe)


(provide 'init-tool-irc)

;;; init-tool-irc.el ends here
