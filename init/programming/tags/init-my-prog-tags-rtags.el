;;; init-my-prog-tags-rtags.el --- init for rtags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rtags ] -- A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.

(use-package rtags
  :ensure t)

;; Sets up a ton of standard keybindings under C-x r (we try to avoid crashing with the register shortcuts).
;; (rtags-enable-standard-keybindings)


(provide 'init-my-prog-tags-rtags)

;;; init-my-prog-tags-rtags.el ends here
