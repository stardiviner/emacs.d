;;; [ Tab ]

(setq-default indent-tabs-mode nil)

;;; only controls how much space a tab character occupies in the buffer.
(setq standard-indent 4)

;; Tab length
(setq default-tab-width 4)

;;; tab-stop
;; (setq tab-stop-list
;;       ;; '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120) ; 8 tab-stops
;;       ;; '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64
;;       ;;     68 72 76 80 84 88 92 96 100 104 108 112 116 120) ; 4 tab-stops
;;       ;; or have elisp generate the list
;;       (number-sequence 4 120 4)         ; 4 tab-stops
;;       )

(define-key text-mode-map (kbd "<tab>") 'indent-relative) ; 'indent-relative, 'tab-to-tab-stop,



(provide 'init-my-emacs-indent)
