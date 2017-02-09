;;; init-ido.el --- init ido settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ido ]

(require 'ido)
(require 'ido-ubiquitous)

(ido-mode t)                            ; enable ido mode
(ido-everywhere t)                      ; use ido-mode wherever possible
(ido-ubiquitous-mode t)                 ; enable ido-ubiquitous

(setq ido-everywhere t
      ido-default-buffer-method 'selected-window ; allow buffer to be open in different frames
      ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
      ido-enable-regexp t
      ido-enable-flex-matching t        ; enable fuzzy search
      ido-enable-prefix nil ; match if the entered text is an arbitrary substring.
      ido-enable-dot-prefix t ; to match hidden files and directories even `ido-enable-prefix' is nil.
      ido-create-new-buffer 'prompt
      ido-use-filename-at-point 'guess  ; look for filename at point
      ido-max-prospects 10
      ido-use-virtual-buffers t         ; allow me to open closed buffers, even
      ido-enable-last-directory-history t
      ido-enable-tramp-completion t
      ido-use-filename-at-point 'guess
      ido-confirm-unique-completion nil
      ido-auto-merge-work-directories-length -1
      ido-max-window-height nil
      ido-record-commands t
      ido-record-ftp-work-directories t
      ido-use-faces t
      )

(use-package ido-vertical-mode
  :ensure t)

(provide 'init-ido)

;;; init-ido.el ends here
