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

;;; [ chinese-yasdcv ] -- yet another sdcv frontend for Emacs.

(use-package chinese-yasdcv
  :ensure t
  :bind (:map dictionary-prefix
              ("M-d" . yasdcv-translate-at-point))
  :config
  (setq yasdcv-sdcv-dicts
        '(("WordNet" "WordNet" nil t)
          ("Oxford English-Chinese" "牛津英汉双解美化版" nil t)
          ("Langdao English-Chinese" "朗道英汉字典5.0" nil t)
          ("Langdao Chinese-English" "朗道汉英字典5.0" nil t)
          ))
  )

;;; [ babel ] -- An Emacs interface to different translation services available on the Internet.

;; (use-package babel
;;   :ensure t
;;   :config
;;   (setq babel-preferred-from-language "English"
;;         babel-preferred-to-language "Chinese"
;;         babel-remember-window-configuration t
;;         babel-max-window-height 30
;;         babel-echo-area t
;;         babel-select-output-window t
;;         )
;;
;;   (defun my-translate-dwim (func-region func-string func-buffer)
;;     "My custom function to use translate functions depend on situations."
;;     (interactive)
;;
;;     (defalias-maybe 'translate-dwim-region func-region)
;;     (defalias-maybe 'translate-dwim-string func-string)
;;     (defalias-maybe 'translate-dwim-buffer func-buffer)
;;
;;     ;; region
;;     (if (region-active-p)
;;         (translate-dwim-region (region-content)))
;;     (if (yes-or-no-p "buffer(n) / interactive input (y)")
;;         ;; interactive input
;;         (let ((msg (read-string "Translate phrase: ")))
;;           (translate-dwim-string msg))
;;       ;; buffer
;;       (translate-dwim-buffer)
;;       )
;;     )
;;
;;   (define-key dictionary-prefix (kbd "t")
;;     (lambda ()
;;       (interactive)
;;       (my-translate-dwim 'babel-region 'babel-as-string 'babel-buffer)))
;;   )

;;; [ synonymous ] -- a thesaurus client that replace with synonym or antonym.

;; (use-package synonymous
;;   :ensure t
;;   )


;;; [ Goldendict ]

(defun goldendict-dwim ()
  "Query current symbol/word at point with Goldendict."
  (interactive)
  (save-excursion
    ;;; way: get word with `thing-at-point'
    (let ((word (if (region-active-p)
                    (buffer-substring-no-properties (mark) (point))
                  (thing-at-point 'word))))
      ;; pass the selection to Emacs shell command goldendict.
      ;; use Goldendict API: "Scan Popup"
      (shell-command (concat "goldendict " word)))
    )
  )

(define-key dictionary-prefix (kbd "d") 'goldendict-dwim)


;;; [ google-translate ]

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-enable-ido-completion nil
        google-translate-show-phonetic t
        ;; google-translate-listen-program
        google-translate-output-destination nil ; 'echo-area, 'popup
        google-translate-pop-up-buffer-set-focus nil
        )

  (set-face-attribute 'google-translate-phonetic-face nil
                      :background "orange" :foreground "black"
                      :slant 'italic)
  (set-face-attribute 'google-translate-listen-button-face nil
                      :background "gray" :foreground "black"
                      :weight 'bold)
  ;; (set-face-attribute 'google-translate-suggestion-face nil
  ;;                     )
  ;; (set-face-attribute 'google-translate-suggestion-label-face nil
  ;;                     )
  (set-face-attribute 'google-translate-translation-face nil
                      :height 1.4
                      )

  (define-key dictionary-prefix (kbd "t") 'google-translate-smooth-translate)
  ;; (define-key dictionary-prefix (kbd "T") 'google-translate-at-point)
  ;; (define-key dictionary-prefix (kbd "C-t") 'google-translate-query-translate)
  )


(provide 'init-my-tool-dictionary)

;;; init-my-tool-dictionary.el ends here
