;;; init-eaf.el --- init for emacs-application-framework -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-09-29 18:48:44 stardiviner>

;;; Commentary:



;;; Code:

;;; [ emacs-application-framework (eaf) ]

(use-package eaf
  :load-path "~/Code/Emacs/emacs-application-framework/"
  :demand t
  ;; :custom (eaf-enable-debug t)
  ;; [ `eaf-org' ] Org Mode integration
  ;; set overriding option before loading `eaf-org' to execute if condition.
  :config
  (add-to-list 'display-buffer-alist '("\\*eaf pdf outline\\*" . (display-buffer-below-selected)))
  
  (eaf-setq eaf-camera-save-path "~")

  ;; [ Dired ]
  (setq eaf-find-alternate-file-in-dired t)
  (define-key dired-mode-map (kbd "M-RET") 'eaf-open-this-from-dired)

  ;; [ web browser ]
  
  ;; use EAF as default web browser for Emacs.
  (defun eaf-toggle-default-browser ()
    "Toggle overriding default web browser with EAF web browser."
    (interactive)
    (if (eq browse-url-browser-function 'eaf-open-browser)
        (progn
          (setq browse-url-browser-function 'browse-url-firefox)
          (message "Now revert default browser to your default function."))
      (setq browse-url-browser-function 'eaf-open-browser)
      (message "Now setting default browser to EAF Web Browser.")))
  ;; let `eaf-open-browser' support HiDPI screen
  (eaf-setq eaf-browser-default-zoom  "2")

  ;; EAF app follow Emacs theme
  (eaf-setq eaf-browser-dark-mode "follow")
  (eaf-setq eaf-pdf-dark-mode "ignore")
  
  ;; set EAF web browser proxy
  (defun eaf-toggle-proxy (&optional proxy)
    "Toggle proxy for EAF."
    (interactive (if (and (null eaf-proxy-type) (null eaf-proxy-host))
                     (list (completing-read
                            "Select Proxy: "
                            '("socks5:127.0.0.1:1086" "http:127.0.0.1:8118")))
                   nil))
    (if proxy
        (let* ((list (split-string proxy ":"))
               (proxy-type (car list))
               (proxy-host (cadr list))
               (proxy-port (caddr list)))
          (setq eaf-proxy-type proxy-type
                eaf-proxy-host proxy-host
                eaf-proxy-port proxy-port))
      (setq eaf-proxy-type nil
            eaf-proxy-host nil
            eaf-proxy-port nil)))
  
  ;; exclude EAF buffers from `desktop-save-mode'.
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-modes-not-to-save 'eaf-mode))

  ;; eaf-org
  (require 'eaf-org)
  (setq eaf-org-override-pdf-links t)
  
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
  

  )



(provide 'init-eaf)

;;; init-eaf.el ends here
