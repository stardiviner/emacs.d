;;; init-eww.el --- The Emacs Web Wowser

;;; Commentary:



;;; Code:


(use-package eww
  :ensure t
  :defer t
  :init
  ;; set to "internal" Emacs Web Wowser
  ;; (setq browse-url-browser-function 'eww-browse-url)
  :config
  (setq eww-bookmarks-directory "~/.emacs.d/eww/bookmarks/"
        eww-download-directory "~/Downloads/"
        eww-form-checkbox-symbol "[ ]"
        eww-form-checkbox-selected-symbol "[X]"
        shr-use-fonts nil ; [F] `eww-toggle-fonts' don't use web page variable-pitch font.
        shr-use-colors t
        shr-external-browser 'browse-url-generic
        eww-header-line-format "%t: %u"   ; title: url.
        ;; - DuckDuckGo :: "https://duckduckgo.com/html/?q="
        ;; - Google :: "http://www.google.com/search?q=%s"
        ;; - Bing :: "http://bing.com/search?q="
        ;; search engine
        eww-search-prefix "https://www.google.com/search?q=%s"
        ;; eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"
        )

  ;; keybindings
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
  
  (define-key eww-mode-map (kbd "O") 'eww-browse-with-external-browser)
  
  (define-key eww-mode-map (kbd "C-M-i") 'eww-view-source)
  (define-key eww-mode-map (kbd "C-M-h") 'eww-parse-headers)
  (define-key eww-mode-map (kbd "C-r") 'eww-readable)
  
  (define-key eww-mode-map (kbd "<enter>") 'eww-submit)

  ;; follow mode keybindings support.
  (use-package eww-lnum
    :ensure t
    :bind (:map eww-mode-map
                ("f" . eww-lnum-follow)
                ("U" . eww-lnum-universal)))
  )





(provide 'init-eww)

;;; init-eww.el ends here
