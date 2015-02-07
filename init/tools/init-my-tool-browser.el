;;; init-my-tool-browser.el --- init for Browser
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ EWW ] -- The Emacs Web Wowser

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
      eww-search-prefix "http://bing.com/search?q="
      eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"
      )

(set-face-attribute 'eww-form-checkbox nil
                    :box '(:color "cyan" :line-width 2 :style 'released-button)
                    :foreground "gray" :background "black"
                    )

(define-key eww-mode-map (kbd "o") 'eww) ; prompt for a URL.
(define-key eww-mode-map (kbd "b") 'eww-list-bookmarks)
(define-key eww-mode-map (kbd "B") 'eww-add-bookmark)

(define-key my-tools-prefix-map (kbd "b") 'eww)


(provide 'init-my-tool-browser)

;;; init-my-tool-browser.el ends here
