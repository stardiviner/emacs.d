;;; init-eaf.el --- init for emacs-application-framework -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-08-12 13:33:38 stardiviner>

;;; Commentary:



;;; Code:

;;; [ emacs-application-framework (eaf) ]

(use-package eaf
  :load-path "~/Code/Emacs/emacs-application-framework/"
  :diminish eaf-mode
  ;; :init (setq eaf-enable-debug t)
  :config
  (eaf-setq eaf-camera-save-path "~")

  ;; [ Dired ]
  (setq eaf-find-alternate-file-in-dired t)
  (define-key dired-mode-map (kbd "M-RET") 'eaf-open-this-from-dired)

  ;; [ web browser ]
  
  ;; use EAF as default web browser for Emacs.
  ;; (setq browse-url-browser-function 'eaf-open-browser)
  ;; let `eaf-open-browser' support HiDPI screen
  (eaf-setq eaf-browser-default-zoom  "2")

  ;; EAF app follow Emacs theme
  (eaf-setq eaf-browser-dark-mode "follow")
  (eaf-setq eaf-pdf-dark-mode "follow")
  
  ;; set EAF web browser proxy
  (defvar eaf-proxy-enabled nil)
  (defun eaf-toggle-proxy ()
    "Toggle EAF proxy."
    (interactive)
    (if eaf-proxy-enabled
        (progn
          (eaf-setq eaf-proxy-type nil)
          (eaf-setq eaf-proxy-host nil)
          (eaf-setq eaf-proxy-port nil)
          (setq eaf-proxy-enabled nil)
          (message "EAF proxy disabled."))
      (progn
        (eaf-setq eaf-proxy-type "socks5")
        (eaf-setq eaf-proxy-host "127.0.0.1")
        (eaf-setq eaf-proxy-port "1086")
        (setq eaf-proxy-enabled t)
        (message "EAF proxy enabled."))))

  ;; exclude EAF buffers from `desktop-save-mode'.
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-modes-not-to-save 'eaf-mode))

  ;; [ `eaf-org' ] Org Mode integration
  ;; set overriding option before loading `eaf-org' to execute if condition.
  (setq eaf-org-override-pdf-links nil)
  ;; (require 'eaf-org)

  ;; eaf-interleave integration
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
  (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
  (add-hook 'org-mode-hook 'eaf-interleave-mode)
  (setq eaf-interleave-org-notes-dir-list '("~/org/interleave/"))
  (setq eaf-interleave-split-direction 'vertical)
  (setq eaf-interleave-disable-narrowing t)
  (setq eaf-interleave-split-lines 20)
  (define-key eaf-interleave-mode-map (kbd "M-.") 'eaf-interleave-sync-current-note)
  (define-key eaf-interleave-mode-map (kbd "M-p") 'eaf-interleave-sync-previous-note)
  (define-key eaf-interleave-mode-map (kbd "M-n") 'eaf-interleave-sync-next-note)
  (define-key eaf-interleave-app-mode-map (kbd "C-c M-i") 'eaf-interleave-add-note)
  (define-key eaf-interleave-app-mode-map (kbd "C-c M-o") 'eaf-interleave-open-notes-file)
  (define-key eaf-interleave-app-mode-map (kbd "C-c M-q") 'eaf-interleave-quit)
  

  (add-to-list 'display-buffer-alist
               '("\\*eaf pdf outline\\*" . (display-buffer-below-selected))))



(provide 'init-eaf)

;;; init-eaf.el ends here
