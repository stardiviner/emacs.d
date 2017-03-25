;;; init-my-tool-dictionary.el --- init my Emacs dictionary.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'dictionary-prefix)
  (define-prefix-command 'dictionary-prefix))
(define-key my-tools-prefix (kbd "d") 'dictionary-prefix)

;;; [ sdcv.el ]

(use-package sdcv
  :ensure t
  :bind (:map dictionary-prefix
              ("d" . sdcv-search-pointer+)
              ("C-d" . sdcv-search-input))
  :config
  ;; a simple dictionary list for popup display
  (setq sdcv-dictionary-simple-list
        '(;; "WordNet"
          "牛津英汉双解美化版"
          ))
  ;; a complete dictionary list for buffer display
  (setq sdcv-dictionary-complete-list
        '("WordNet"
          "牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"
          ))
  )

;;; [ synonymous ] -- a thesaurus client that replace with synonym or antonym.

;; (use-package synonymous
;;   :ensure t
;;   )


;;; [ Goldendict ]

(use-package stem-english
  :ensure t)

(defun goldendict-dwim ()
  "Query current symbol/word at point with Goldendict."
  (interactive)
  (save-excursion
    ;;; way: get word with `thing-at-point'
    (let ((word (my-stem-english ; word stemmer to convert plural word into singular.
                 (if (region-active-p)
                     (buffer-substring-no-properties (mark) (point))
                   (thing-at-point 'word)))))
      ;; pass the selection to Emacs shell command goldendict.
      ;; use Goldendict API: "Scan Popup"
      (shell-command (concat "goldendict " word)))
    )
  )

(define-key dictionary-prefix (kbd "d") 'goldendict-dwim)


;;; [ google-translate ]

(use-package google-translate
  :ensure t
  :defer t
  :bind (:map dictionary-prefix
              ("t" . google-translate-smooth-translate)
              ("T" . google-translate-at-point)
              ("C-t" . google-translate-query-translate)
              )
  :config
  (setq google-translate-enable-ido-completion nil
        google-translate-show-phonetic t
        ;; google-translate-listen-program
        google-translate-output-destination nil ; 'echo-area, 'popup
        google-translate-pop-up-buffer-set-focus nil
        )
  )

;;; [ wotd ] -- Show "Word of the Day" from 15 online sources in Emacs.

(use-package wotd
  :ensure t)


(provide 'init-my-tool-dictionary)

;;; init-my-tool-dictionary.el ends here
