;;; init-my-emacs-mac.el --- init for Mac OS X
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:



(if *is-mac*

    ;; <key modifiers>
    ;;
    ;; First of all, Meta M- needs to be really easy to hit. On a Mac keyboard,
    ;; that means Command - and not the default Option - since we want the key
    ;; that is right next to Space.
    ;;
    ;; The good news is that now Option is available for Super s-. And even more
    ;; amazing, you can also bind the Function-key to Hyper H- - without losing
    ;; the ability to change the volume or pause/play.
    ;;
    ;; So now I can use crazy keybindings like H-SPC hyperspace.
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          ns-function-modifier 'hyper)
  )



(provide 'init-my-emacs-mac)

;;; init-my-emacs-mac.el ends here
