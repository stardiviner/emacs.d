;;; init-my-tool-browser.el --- init for Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; default browser function

;; - 'browse-url
;; - 'browse-url-default-browser
;; - 'helm-browse-url
(setq browse-url-browser-function 'browse-url-default-browser)


;;; [ EWW ] -- The Emacs Web Wowser

;;; Usage:
;;
;; - [M-x eww Enter URL] ::

(require 'eww)

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
      eww-search-prefix "https://www.duckduckgo.com/?q="
      eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"
      )

(eval-after-load "eww"
  (lambda ()
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
  )

(define-key my-tools-prefix-map (kbd "b") 'eww)


(provide 'init-my-tool-browser)

;;; init-my-tool-browser.el ends here
