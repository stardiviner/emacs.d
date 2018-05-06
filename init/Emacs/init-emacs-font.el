;;; init-emacs-font.el --- Emacs default font settings.
;;
;;; Commentary:
;;
;; the value is in 1/10pt, so 100 will give you 10pt, etc
;;
;; - [C-u C-x =]
;; - [M-x describe-font]
;; - [M-x describe-fontset]
;; - from command: $ fc-list

;;; Code:

(defun my-font-settings ()
  "Set Emacs font."
  (interactive)
  ;; set Unicode characters font
  (when (display-graphic-p) ; for `set-fontset-font'
    (when (member "Symbola" (font-family-list))
      (set-fontset-font t 'unicode "Symbola" nil 'prepend))
    
    ;; set CJK font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        ;; (font-spec :family "Noto Sans CJK SC" :size 13)
                        ;; (font-spec :family "Source Han" :size 12)
                        ;; (font-spec :family "Hack" :size 12)
                        (font-spec :family "WenQuanYi Micro Hei" :size 13)
                        ;; (font-spec :family "NSimSun" :size 13)
                        )))
  
  ;; need to modify English font settings to suitable with chinese font.
  ;; the value is in 1/10pt, so 100 will give you 10pt, etc
  ;; (set-frame-font (format "%s:pixelsize=%d" "DejaVu Sans Mono" 12) t)
  (set-frame-font (format "%s:pixelsize=%d" "Hack" 12) t)

  ;; set default font.
  (set-face-attribute 'default nil
		                  :family "Hack"
		                  :foundry "PfEd")
  )

(my-font-settings)
(add-hook 'after-init-hook #'my-font-settings)

;; (add-hook 'circadian-after-load-theme-hook #'my-circadian-font-reset)


(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
