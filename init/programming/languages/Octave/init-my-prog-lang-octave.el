;;; init-my-prog-lang-octave.el --- init for Octave
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]

(use-package octave
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
  
  :config
  (setq octave-auto-indent t
        octave-auto-newline t
        octave-blink-matching-block t
        octave-block-offset 2
        octave-continuation-offset 4
        octave-continuation-string "..."
        octave-send-echo-input t
        octave-send-show-buffer t
        )

  (define-key octave-mode-map (kbd "C-h d") 'octave-help)
  (define-key inferior-octave-mode-map (kbd "C-h d") 'octave-help)
  )


;;; [ ac-octave ]

(use-package ac-octave
  :ensure t
  :defer t
  :init
  (defun ac-octave-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-octave))
  
  (add-hook 'octave-mode-hook #'ac-octave-mode-setup)
  )


(provide 'init-my-prog-lang-octave)

;;; init-my-prog-lang-octave.el ends here
