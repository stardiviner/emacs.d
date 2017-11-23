;;; init-my-tool-dictionary.el --- init my Emacs dictionary.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'dictionary-prefix)
  (define-prefix-command 'dictionary-prefix))
(define-key tools-prefix (kbd "d") 'dictionary-prefix)

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

;;; [ chinese-yasdcv ] -- Yet another frontend for the StarDict command-line program.

(use-package chinese-yasdcv
  :ensure t
  :commands (yasdcv-translate-at-point)
  :bind (:map dictionary-prefix
              ("s" . yasdcv-translate-at-point)
              ("C-s" . yasdcv-translate-input))
  :config
  (setq yasdcv-sdcv-dicts
        '(("WordNet" "WordNet" nil t)
          ("Oxford English-Chinese" "牛津英汉双解美化版" nil t)
          ("Langdao English-Chinese" "朗道英汉字典5.0" nil t)
          ("Langdao Chinese-English" "朗道汉英字典5.0" nil t)
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
  (let ((word (downcase
               (substring-no-properties
                (if (region-active-p)
                    (buffer-substring-no-properties (mark) (point))
                  ;; way: get word with `thing-at-point'
                  (thing-at-point 'word))))))
    (save-excursion
      ;; pass the selection to Emacs shell command goldendict.
      ;; use Goldendict API: "Scan Popup"
      (shell-command (concat "goldendict " word)))))

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


;;; [ ob-translate ] -- allows you to translate blocks of text within org-mode.

(use-package ob-translate
  :ensure t)


(provide 'init-my-tool-dictionary)

;;; init-my-tool-dictionary.el ends here
