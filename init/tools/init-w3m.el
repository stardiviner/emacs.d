;;; init-w3m.el --- 

;;; Commentary:



;;; Code:

(use-package w3m
  :ensure t
  :defer t
  :commands w3m-goto-url w3m-search
  :config
  ;; (setq browse-url-browser-function 'w3m-browse-url)
  
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  
  (setq w3m-use-cookies t)

  (require 'w3m-search) ; Command `w3m-search'
  (setq w3m-language "English")
  (setq w3m-search-engine-alist '(("google" "https://www.google.com/search?q=%s")
                                  ("duckduckgo" "https://duckduckgo.com/?q=%s")
                                  ("wikipedia" "https://en.wikipedia.org/wiki/Special:Search/%s")
                                  ("emacswiki" "https://www.emacswiki.org/cgi-bin/wiki?search=%s")
                                  ))

  (define-key w3m-mode-map (kbd "&") 'w3m-view-url-with-external-browser)
  
  ;; setup keybindings
  (setq w3m-mode-map (make-sparse-keymap))

  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
  (define-key w3m-mode-map (kbd "q") 'bury-buffer)
  (define-key w3m-mode-map [f5] 'w3m-reload-this-page)
  (define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

  (define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
  
  (defun w3m-maybe-url ()
    (interactive)
    (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
            (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
        (w3m-view-this-url)))
  )



(provide 'init-w3m)

;;; init-w3m.el ends here
