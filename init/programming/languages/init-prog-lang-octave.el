;;; init-prog-lang-octave.el --- init for Octave
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]

(use-package octave
  :ensure t
  :defer t
  :mode ("\\.m\\'" . octave-mode)
  :commands (run-octave)
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

  (define-key octave-mode-map (kbd "C-c C-d") 'octave-help)
  (define-key inferior-octave-mode-map (kbd "C-c C-d") 'octave-help)
  (add-to-list 'display-buffer-alist
               '("\\*Inferior Octave\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("\\*Octave Help\\*" . (display-buffer-same-window)))
  )

;;; [ ac-octave ] -- auto-complete source for Octave.

(use-package ac-octave
  :ensure t
  :defer t
  :init
  (add-hook 'octave-mode-hook
            #'(lambda ()
                (with-eval-after-load "ac-octave"
                  (ac-octave-init)
                  (add-to-list 'ac-sources 'ac-complete-octave))))
  )

;;; [ ob-octave ]

(use-package ob-octave
  :defer t
  :commands (org-babel-execute:octave)
  :config
  (add-to-list 'org-babel-load-languages '(octave . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("octave" . "m")))



(provide 'init-prog-lang-octave)

;;; init-prog-lang-octave.el ends here
