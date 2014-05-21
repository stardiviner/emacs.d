;;; init-my-prog-lang-octave.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ octave-mode ]


;;; [ EOS: Emacs Octave Support ]
;;; {consists of the three files `octave-mod.el', `octave-inf.el', and `octave-hlp.el'.}


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


;;; [ octave-inf ] Running Octave From Within Emacs

;; - [M-x run-octave] ::
;;
;; C-c i l
;;     Send the current line to the inferior Octave process (octave-send-line). With positive prefix argument N, send that many lines. If octave-send-line-auto-forward is non-nil, go to the next unsent code line. 
;; C-c i b
;;     Send the current block to the inferior Octave process (octave-send-block). 
;; C-c i f
;;     Send the current function to the inferior Octave process (octave-send-defun). 
;; C-c i r
;;     Send the region to the inferior Octave process (octave-send-region). 
;; C-c i s
;;     Make sure that `inferior-octave-buffer' is displayed (octave-show-process-buffer). 
;; C-c i h
;;     Delete all windows that display the inferior Octave buffer (octave-hide-process-buffer). 
;; C-c i k
;;     Kill the inferior Octave process and its buffer (octave-kill-process). 


;;; to directly start an inferior Octave process. If Emacs does not know about
;;; this command, add the line
(autoload 'run-octave "octave-inf" nil t)
;;; enable inferior octave after Emacs startup.
;; (add-hook 'emacs-startup-hook 'run-octave)


(setq octave-send-echo-input t
      octave-send-show-buffer t)

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))


;;; Using the Emacs Info Reader for Octave
;; If `gnuserv' is installed, add the lines
;; (autoload 'octave-help "octave-hlp" nil t)
;; (require 'gnuserv)
;; (gnuserv-start)



;;; [ ac-octave ]

(after 'octave-inf
  (require 'ac-octave))

;; FIXME:
;; (defun ac-octave-mode-setup ()
;;   (setq ac-sources '(ac-source-octave)))
;; (add-hook 'octave-mode-hook
;;           '(lambda () (ac-octave-mode-setup)))


(provide 'init-my-prog-lang-octave)

;;; init-my-prog-lang-octave.el ends here
