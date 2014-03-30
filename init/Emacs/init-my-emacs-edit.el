;;; init-my-emacs-edit.el --- init Emacs editing
;;; Commentary:

;;; Code:

;;; [ undo-tree ]
;; [C-x u] -> undo-tree-visualizer-mode
;;    `- [C-p/n]  -- move up/down
;;    `- [C-b/f]  -- move left/right
;;    `- t    -- timestamp
;;    `- q    -- quit

(require 'undo-tree)
(global-undo-tree-mode t)



;;; [ multiple-cursors ]

;;; Usage:
;;; https://github.com/magnars/multiple-cursors.el
;;; - [C-c c] -- prefix of mc.
;;; - [C-c c c] / [C-S-c C-S-c] -- edit-lines
;;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))

(require 'multiple-cursors)

;; TODO 'mc/mark-all-dwim

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-pop)

;; First mark the word, then add more cursors.

;; To get out of multiple-cursors-mode, press <return> or C-g. The latter will
;; first disable multiple regions before disabling multiple cursors. If you want
;; to insert a newline in multiple-cursors-mode, use [C-j].




(provide 'init-my-emacs-edit)

;;; init-my-emacs-edit.el ends here
