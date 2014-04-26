;;; init-my-prog-lang-octave.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]




;;; [ octave-mod ]

(require 'octave)
(autoload 'octave-mode "octave-mod" nil t)


(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(setq octave-auto-indent t
      octave-auto-newline t
      octave-blink-matching-block t
      octave-block-offset 2
      octave-continuation-offset 4
      octave-continuation-string "..."
      )

(define-key octave-mode-map (kbd "C-h d") 'octave-help)
(define-key inferior-octave-mode-map (kbd "C-h d") 'octave-help)




;;; [ ac-octave ]

;; (after 'octave-inf
;;   (require 'ac-octave)
;;   (autoload 'ac-octave "ac-octave" "Octave auto-complete source" t))

;; (add-hook 'octave-mode-hook
;;           '(lambda ()
;;              (setq ac-sources '(ac-source-octave))))



(provide 'init-my-prog-lang-octave)

;;; init-my-prog-lang-octave.el ends here
