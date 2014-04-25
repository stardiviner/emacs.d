;;; init-my-prog-lang-octave.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]




(require 'octave)
(autoload 'octave-mode "octave-mod" nil t)





;;; [ ac-octave ]

;; (after 'octave-inf
;;   (require 'ac-octave)
;;   (autoload 'ac-octave "ac-octave" "Octave auto-complete source" t))

;; (add-hook 'octave-mode-hook
;;           '(lambda ()
;;              (setq ac-sources '(ac-source-octave))))



(provide 'init-my-prog-lang-octave)

;;; init-my-prog-lang-octave.el ends here
