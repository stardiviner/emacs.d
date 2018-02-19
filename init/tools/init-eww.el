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

  ;; EWW syntax highlighting
  (use-package language-detection
    :ensure t
    :config
    (require 'cl-lib)

    (defun eww-tag-pre (dom)
      (let ((shr-folding-mode 'none)
            (shr-current-font 'default))
        (shr-ensure-newline)
        (insert (eww-fontify-pre dom))
        (shr-ensure-newline)))

    (defun eww-fontify-pre (dom)
      (with-temp-buffer
        (shr-generic dom)
        (let ((mode (eww-buffer-auto-detect-mode)))
          (when mode
            (eww-fontify-buffer mode)))
        (buffer-string)))

    (defun eww-fontify-buffer (mode)
      (delay-mode-hooks (funcall mode))
      (font-lock-default-function mode)
      (font-lock-default-fontify-region (point-min)
                                        (point-max)
                                        nil))

    (defun eww-buffer-auto-detect-mode ()
      (let* ((map '((ada ada-mode)
                    (awk awk-mode)
                    (c c-mode)
                    (cpp c++-mode)
                    (clojure clojure-mode lisp-mode)
                    (csharp csharp-mode java-mode)
                    (css css-mode)
                    (dart dart-mode)
                    (delphi delphi-mode)
                    (emacslisp emacs-lisp-mode)
                    (erlang erlang-mode)
                    (fortran fortran-mode)
                    (fsharp fsharp-mode)
                    (go go-mode)
                    (groovy groovy-mode)
                    (haskell haskell-mode)
                    (html html-mode)
                    (java java-mode)
                    (javascript javascript-mode)
                    (json json-mode javascript-mode)
                    (latex latex-mode)
                    (lisp lisp-mode)
                    (lua lua-mode)
                    (matlab matlab-mode octave-mode)
                    (objc objc-mode c-mode)
                    (perl perl-mode)
                    (php php-mode)
                    (prolog prolog-mode)
                    (python python-mode)
                    (r r-mode)
                    (ruby ruby-mode)
                    (rust rust-mode)
                    (scala scala-mode)
                    (shell shell-script-mode)
                    (smalltalk smalltalk-mode)
                    (sql sql-mode)
                    (swift swift-mode)
                    (visualbasic visual-basic-mode)
                    (xml sgml-mode)))
             (language (language-detection-string
                        (buffer-substring-no-properties (point-min) (point-max))))
             (modes (cdr (assoc language map)))
             (mode (cl-loop for mode in modes
                            when (fboundp mode)
                            return mode)))
        (message (format "%s" language))
        (when (fboundp mode)
          mode)))
    
    (setq shr-external-rendering-functions
          '((pre . eww-tag-pre)))
    )
  )





(provide 'init-eww)

;;; init-eww.el ends here
