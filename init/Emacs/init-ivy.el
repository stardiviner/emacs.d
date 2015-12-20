;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swiper/ivy-mode ]

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper)
  ;; Ivy-mode
  (setq ivy-use-virtual-buffers t
        ivy-height 6
        ivy-count-format "(%d/%d) "
        ivy-wrap nil
        )
  
  (set-face-attribute 'ivy-confirm-face nil
                      :inherit 'minibuffer-prompt
                      :foreground "green")
  (set-face-attribute 'ivy-current-match nil
                      :background "#004A5D" :foreground "white"
                      :box '(:color "cyan" :line-width -1)
                      )
  (set-face-attribute 'ivy-match-required-face nil
                      :inherit 'minibuffer-prompt
                      :foreground "red" :background "#004A5D"
                      )
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                      :inherit 'swiper-match-face-1)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :inherit 'swiper-match-face-2)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :inherit 'swiper-match-face-3)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :inherit 'swiper-match-face-4)
  
  (ivy-mode 1)
  )


;;; [ counsel ]

(use-package counsel)


(provide 'init-ivy)

;;; init-ivy.el ends here
