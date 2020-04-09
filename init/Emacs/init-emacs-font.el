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

(let ((emacs-font-size 9)
      emacs-font-name)
  (cond
   ((featurep 'cocoa)
    (setq emacs-font-name "Monaco"))
   ((string-equal system-type "gnu/linux")
    (setq emacs-font-name "DejaVu Sans Mono")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 10")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))


;; set Unicode characters font
(when (display-graphic-p) ; for `set-fontset-font'
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))



(provide 'init-emacs-font)

;;; init-emacs-font.el ends here
