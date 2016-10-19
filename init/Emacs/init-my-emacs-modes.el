;;; init-my-emacs-modes.el --- init Emacs modes settings
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ auto-mode-alist ]

(add-to-list 'auto-mode-alist '("\\.conkyrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("conkyrc\\'" . conf-mode))

;;; Arch PKGBUILD (pkgbuild-mode)
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD\\'" . pkgbuild-mode)) auto-mode-alist))


;;; [ mmm-mode ] -- Minor mode to allow multiple major modes in one buffer.

(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe) ; t, nil, 'maybe (turn itself on in precisely).

  (setq mmm-submode-mode-line-format "~M > [~m]"
        mmm-primary-mode-display-name t
        ;; mmm-buffer-mode-display-name t
        )

  (setq mmm-submode-decoration-level 3)

  ;; (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)

  (unless (boundp 'my-edit-prefix)
    (define-prefix-command 'my-edit-prefix))
  (define-key my-edit-prefix (kbd "m") 'mmm-mode) ; enable mmm-mode on region.
  
  ;; submode classes
  ;; (mmm-add-classes
  ;;  '((embedded-css
  ;;     :submode css
  ;;     :face mmm-declaration-submode-face
  ;;     :front "<style[^>]*>"
  ;;     :back "</style>")))

  ;; submode groups
  ;; (mmm-add-to-group 'html-js '((js-html
  ;;                               :submode javascript
  ;;                               :face mmm-code-submode-face
  ;;                               :front "%=%"
  ;;                               :back "%=%"
  ;;                               :end-not-begin t)))
  )


;;; [ mumamo-noweb ] -- multiple major modes


;;; [ polymode ] -- Object oriented framework for multiple emacs modes based on indirect buffers.

(use-package polymode
  :ensure t
  :defer t)


(provide 'init-my-emacs-modes)

;;; init-my-emacs-modes.el ends here
