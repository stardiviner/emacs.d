;;; init-my-emacs-subprocess.el --- init for Emacs subprocess managing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ deferred ] -- Simple asynchronous functions for Emacs Lisp.

;; (use-package deferred
;;   :ensure t)

;;; [ prodigy ] -- Manage external services from within Emacs.

;; (use-package prodigy
;;   :ensure t
;;   :config
;;   (setq prodigy-completion-system 'ivy
;;         prodigy-kill-process-buffer-on-stop nil
;;         )
;;
;;   ;; user defined services.
;;   ;; (prodigy-define-service
;;   ;;   :name "Clojure REPL (Leiningen)"
;;   ;;   :command "lein"
;;   ;;   :args '("repl")
;;   ;;   :cwd "~/Code/learning/Clojure/clojure-examples/"
;;   ;;   :tags '(Clojure)
;;   ;;   :stop-signal 'sigkill
;;   ;;   :kill-process-buffer-on-stop t)
;;   )

;;; [ bpr ] -- Emacs Background Process Runner.

;; (use-package bpr
;;   :ensure t
;;   :commands (bpr-spawn bpr-open-last-buffer)
;;   )



(provide 'init-my-emacs-subprocess)

;;; init-my-emacs-subprocess.el ends here
