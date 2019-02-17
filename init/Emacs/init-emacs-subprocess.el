;;; init-emacs-subprocess.el --- init for Emacs subprocess managing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prodigy ] -- Manage external services from within Emacs.

(use-package prodigy
  :ensure t
  :defer t
  :config
  (setq prodigy-completion-system 'ivy
        prodigy-kill-process-buffer-on-stop nil
        )

  ;; user defined services.
  ;; (prodigy-define-service
  ;;   :name "Clojure REPL (Leiningen)"
  ;;   :command "lein"
  ;;   :args '("repl")
  ;;   :cwd "~/Code/learning/Clojure/clojure-examples/"
  ;;   :tags '(Clojure)
  ;;   :stop-signal 'sigkill
  ;;   :kill-process-buffer-on-stop t)
  )

;;; [ bpr ] -- Emacs Background Process Runner.

(use-package bpr
  :ensure t
  :defer t
  :commands (bpr-spawn bpr-open-last-buffer)
  :config
  (setq bpr-colorize-output t)
  (setq bpr-process-mode #'comint-mode)
  ;; (setq bpr-on-completion #'FUNC)
  )

;;; [ emacs-application-framework (eaf) ]

(use-package eaf
  :load-path "~/Code/Emacs/emacs-application-framework/"
  :defer t
  :commands (eaf-open
             eaf-open-url eaf-dired-open-file
             eaf-show-file-qrcode dired-show-file-qrcode
             eaf-air-share eaf-upload-file))



(provide 'init-emacs-subprocess)

;;; init-emacs-subprocess.el ends here
