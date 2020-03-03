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
        prodigy-kill-process-buffer-on-stop nil)

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
  (setq bpr-colorize-output t
        bpr-process-mode #'comint-mode
        ;; bpr-on-completion #'FUNC
        ))

;;; [ emacs-application-framework (eaf) ]

(use-package eaf
  :load-path "~/Code/Emacs/emacs-application-framework/"
  :custom (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-setq eaf-camera-save-path "~")
  (setf (alist-get "q" eaf-pdf-viewer-keybinding) 'quit-window)
  (setf (alist-get "q" eaf-image-viewer-keybinding) 'quit-window)
  (setf (alist-get "q" eaf-video-player-keybinding) 'quit-window)
  ;; use EAF as default web browser for Emacs.
  (setq browse-url-browser-function 'eaf-open-browser)
  ;; let `eaf-open-browser' support HiDPI screen
  (eaf-setq eaf-browser-default-zoom  "2")
  ;; set EAF web browser proxy
  (defvar eaf-proxy-enabled nil)
  (defun eaf-proxy-toggle ()
    "Toggle EAF proxy."
    (interactive)
    (if eaf-proxy-enabled
        (progn
          (eaf-setq eaf-proxy-type nil)
          (eaf-setq eaf-proxy-host nil)
          (eaf-setq eaf-proxy-port nil))
      (progn
        (eaf-setq eaf-proxy-type "socks5")
        (eaf-setq eaf-proxy-host "127.0.0.1")
        (eaf-setq eaf-proxy-port "1086"))))
  ;; exclude EAF buffers from `desktop-save-mode'.
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-modes-not-to-save 'eaf-mode)))

;;; [ Threads ]

(defun emacs-thread-asynchronously (key)
  "Read key-sequence, then execute its command on a new thread."
  (interactive (list (read-key-sequence "Key Sequence: ")))
  (let ((l  (local-key-binding key))
        (g (global-key-binding key)))
    (cond
     ((commandp l)
      (make-thread l)
      (message "Running %s on a new thread." l))
     ((commandp g)
      (make-thread g)
      (message "Running %s on a new thread." g))
     (t (error "%s is not bound to a command." key)))))

;; (global-set-key (kbd "C-' a") 'emacs-thread-asynchronously)



(provide 'init-emacs-subprocess)

;;; init-emacs-subprocess.el ends here
