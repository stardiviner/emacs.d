;;; init-my-tool-music.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-music-prefix)
  (define-prefix-command 'my-music-prefix))

(define-key my-tools-prefix (kbd "M") 'my-music-prefix)


;;; [ MPC ] -- Emacs built-in Music Player

(use-package mpc
  :ensure t
  :config
  (advice-add 'mpc :after
              (lambda ()
                (switch-to-buffer-other-window "*MPC-Songs*")))
  
  (define-key my-music-prefix (kbd "M") 'mpc)
  
  (defun my-mpc-songs-search ()
    (interactive)
    (call-interactively 'mpc-songs-search)
    (switch-to-buffer-other-window "*MPC-Songs*"))
  
  (define-key mpc-mode-map (kbd "l") 'my-mpc-songs-search)
  (define-key mpc-mode-map (kbd "L") 'mpc-songs-kill-search)

  (define-key mpc-mode-map (kbd "y") 'mpc-toggle-single)
  (define-key mpc-mode-map (kbd "r") 'mpc-toggle-repeat)
  (define-key mpc-mode-map (kbd "s") 'mpc-toggle-shuffle)
  (define-key mpc-mode-map (kbd "t") 'mpc-toggle-play)
  
  (define-key my-music-prefix (kbd "y") 'mpc-toggle-single)
  (define-key my-music-prefix (kbd "r") 'mpc-toggle-repeat)
  (define-key my-music-prefix (kbd "s") 'mpc-toggle-shuffle)
  (define-key my-music-prefix (kbd "t") 'mpc-toggle-play)
  )


;;; [ mingus ] -- MPD client

;;; Usage:
;;
;; - `mingus'
;; - `mingus-stays-home'

(use-package mingus
  ;; :ensure t
  :config
  (autoload 'mingus "mingus-stays-home" nil t)
  ;; (autoload 'mingus "mingus" nil t)

  (define-key my-music-prefix (kbd "M") 'mingus)
  (define-key my-music-prefix (kbd "l") 'mingus-playlist)
  (define-key my-music-prefix (kbd "b") 'mingus-browse)
  (define-key my-music-prefix (kbd "t") 'mingus-toggle)
  (define-key my-music-prefix (kbd "n") 'mingus-next)
  (define-key my-music-prefix (kbd "p") 'mingus-prev)
  (define-key my-music-prefix (kbd "s") 'mingus-search)
  (define-key my-music-prefix (kbd "q") 'mingus-query)
  )



;; (require 'init-my-tool-music-emms)
;; (require 'init-my-tool-music-bongo)



(provide 'init-my-tool-music)

;;; init-my-tool-music.el ends here
