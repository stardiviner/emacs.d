;;; init-my-prog-lang-octave.el --- init for Octave
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]

(use-package octave
  :ensure t
  :ensure-system-package octave
  :mode ("\\.m\\'" . octave-mode)
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
  )

;;; [ ac-octave ] -- auto-complete source for Octave.

(use-package ac-octave
  :ensure t
  :config
  (add-hook 'octave-mode-hook
            (lambda ()
              (ac-octave-init)
              (add-to-list 'ac-sources 'ac-complete-octave)))
  )

(require 'ob-matlab)
(add-to-list 'org-babel-load-languages '(matlab . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("matlab" . "m"))

(require 'ob-octave)
(add-to-list 'org-babel-load-languages '(octave . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("octave" . "m"))



(provide 'init-my-prog-lang-octave)

;;; init-my-prog-lang-octave.el ends here
