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
  :custom ((octave-auto-indent t)
           (octave-auto-newline t)
           (octave-blink-matching-block t)
           (octave-block-offset 2)
           (octave-continuation-offset 4)
           (octave-continuation-string "...")
           (octave-send-echo-input t)
           (octave-send-show-buffer t))
  :init
  (add-to-list 'display-buffer-alist '("\\*Inferior Octave\\*" . (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist '("\\*Octave Help\\*" . (display-buffer-same-window)))
  :config
  (define-key octave-mode-map (kbd "C-c C-d") 'octave-help)
  (define-key inferior-octave-mode-map (kbd "C-c C-d") 'octave-help))

;;; [ ac-octave ] -- auto-complete source for Octave.

(use-package ac-octave
  :ensure t
  :config
  (defun ac-octave-setup ()
    (ac-octave-init)
    (add-to-list 'ac-sources 'ac-complete-octave))
  (add-hook 'octave-mode-hook #'ac-octave-setup))

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
