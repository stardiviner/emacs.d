;;; init-my-prog-lang-clojure.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ clojure-mode ]

(require 'clojure-mode)

(eval-after-load 'clojure-mode
  '(progn
     (defun my-clojure-mode-defaults ()
       (clojure-test-mode +1))

     (setq my-clojure-mode-hook 'my-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook
               (lambda ()
                 (run-hooks 'my-clojure-mode-hook)))))



;;; [ ac-nrepl ] --

(require 'ac-nrepl)

(add-hook 'clojure-mode-hook
          (lambda ()
            (after-load 'auto-complete
                        (add-to-list 'ac-sources 'ac-nrepl))))

;;; ac-nrepl
(set-face-attribute 'ac-nrepl-candidate-face nil
                    :foreground "cyan"
                    :bold 'normal)
;; (set-face-attribute 'ac-nrepl-selection-face nil
;;                     )


;;; [ cider ]

(require 'cider)

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun my-cider-repl-mode-defaults ()
       (subword-mode +1))

     (setq my-cider-repl-mode-hook 'my-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook
               (lambda ()
                 (run-hooks 'my-cider-repl-mode-hook)))))



(provide 'init-my-prog-lang-clojure)

;;; init-my-prog-lang-clojure.el ends here
