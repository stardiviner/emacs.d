;;; init-my-tool-browser.el --- init for Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; default browser function

;; - 'browse-url
;; - 'browse-url-default-browser

;; system default browser
;; (setq browse-url-browser-function 'browse-url-default-browser)

;; specify not exist function browser as default web browser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")


;;; [ EWW ] -- The Emacs Web Wowser

;;; Usage:
;;
;; - [M-x eww RET URL] ::

;;; set to "internal" Emacs Web Wowser
;; (setq browse-url-browser-function 'eww-browse-url)

(setq eww-bookmarks-directory "~/.emacs.d/eww/bookmarks/"
      eww-download-directory "~/Downloads/"
      eww-form-checkbox-symbol "[ ]"
      eww-form-checkbox-selected-symbol "[X]"
      eww-header-line-format "%t: %u"   ; title: url.
      ;; - DuckDuckGo :: "https://duckduckgo.com/html/?q="
      ;; - Google :: "http://www.google.com/search?q=%s"
      ;; - Bing :: "http://bing.com/search?q="
      ;; search engine
      eww-search-prefix "https://www.google.com/search?q=%s"
      ;; eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"
      )

(with-eval-after-load "eww"
  (set-face-attribute 'eww-form-checkbox nil
                      :box '(:color "cyan" :line-width 2 :style 'released-button)
                      :foreground "gray" :background "black"
                      )
  
  (define-key eww-mode-map (kbd "o") 'eww) ; prompt for a URL.
  
  (define-key eww-mode-map (kbd "f") 'eww-follow-link)
  (define-key eww-mode-map (kbd "d") 'eww-download)
  
  (define-key eww-mode-map (kbd "C-i") 'eww-back-url)
  (define-key eww-mode-map (kbd "C-o") 'eww-forward-url)
  (define-key eww-mode-map (kbd "r") 'eww-reload)
  
  (define-key eww-mode-map (kbd "<C-tab>") 'eww-buffer-select) ; 'eww-buffer-show, 'eww-list-buffers
  ;; (define-key eww-mode-map (kbd "<C-tab>") 'eww-buffer-show-next)
  ;; (define-key eww-mode-map (kbd "<C-S-iso-lefttab>") 'eww-buffer-show-previous)
  
  (define-key eww-mode-map (kbd "h") 'eww-history-browse) ; 'eww-list-histories
  
  (define-key eww-mode-map (kbd "b") 'eww-list-bookmarks)
  (define-key eww-mode-map (kbd "B") 'eww-add-bookmark)
  (define-key eww-mode-map (kbd "M-n") nil)
  (define-key eww-mode-map (kbd "M-p") nil)
  
  (define-key eww-mode-map (kbd "&") 'eww-browse-with-external-browser)
  
  (define-key eww-mode-map (kbd "v") 'eww-view-source)
  (define-key eww-mode-map (kbd "H") 'eww-parse-headers)
  (define-key eww-mode-map (kbd "R") 'eww-readable)
  
  (define-key eww-mode-map (kbd "<enter>") 'eww-submit)
  )

(define-key my-tools-prefix (kbd "v") 'eww)


;;; [ w3m ]

;;; Usage:
;;
;; - [M-x w3m]

(use-package w3m
  :config
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)

  ;; setup keybindings
  (setq w3m-mode-map (make-sparse-keymap))

  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
  (define-key w3m-mode-map (kbd "q") 'bury-buffer)
  (define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
  (define-key w3m-mode-map [f5] 'w3m-reload-this-page)
  (define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

  (defun w3m-maybe-url ()
    (interactive)
    (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
            (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
        (w3m-view-this-url)))
  )


(provide 'init-my-tool-browser)

;;; init-my-tool-browser.el ends here
