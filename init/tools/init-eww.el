;;; init-eww.el --- The Emacs Web Wowser

;;; Commentary:



;;; Code:


(use-package eww
  :ensure t
  :defer t
  :commands (eww)
  :custom (;; (browse-url-browser-function 'eww-browse-url) ; set to "internal" Emacs Web Wowser
           (eww-bookmarks-directory (expand-file-name "eww/bookmarks/" user-emacs-directory))
           (eww-download-directory "~/Downloads/")
           (eww-form-checkbox-symbol "[ ]")
           (eww-form-checkbox-selected-symbol "[X]")
           (shr-use-fonts t) ; [F] `eww-toggle-fonts' don't use web page variable-pitch font.
           (shr-use-colors nil) ; don't use webpage's color.
           (shr-external-browser 'browse-url-generic)
           (eww-header-line-format "%t: %u")   ; title: url.
           ;; - DuckDuckGo :: "https://duckduckgo.com/html/?q="
           ;; - Google :: "http://www.google.com/search?q=%s"
           ;; - Bing :: "http://bing.com/search?q="
           ;; search engine
           (eww-search-prefix "https://www.google.com/search?q=%s")
           ;; eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\)"
           )
  :init (add-to-list 'display-buffer-alist '("^\\*eww\\*" . (display-buffer-below-selected)))
  :config
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
  
  (define-key eww-mode-map (kbd "<enter>") 'eww-submit))

;;; [ eww-lnum ] -- Conkeror-like functionality for eww.

;; follow mode keybindings support.
(use-package eww-lnum
  :ensure t
  :defer t
  :bind (:map eww-mode-map ("f" . eww-lnum-follow) ("U" . eww-lnum-universal)))

;;; - store URL in EWW for `org-store-link'.
;;; - copy region and convert to Org format on the fly with `org-eww-copy-for-org-mode' [C-c C-x C-w].
(use-package ol-eww
  :after eww)



(provide 'init-eww)

;;; init-eww.el ends here
