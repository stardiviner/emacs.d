;;; init-emacs-edit-collaborate.el --- init Emacs for collaboration editing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ floobits ] -- Floobits client for Emacs.

(use-package floobits
  :ensure t
  :defer t
  :commands (floobits-join-workspace
             floobits-add-to-workspace
             floobits-remove-from-workspace
             floobits-leave-workspace
             floobits-delete-workspace
             floobits-open-workspace-in-browser
             floobits-workspace-settings
             floobits-share-dir-private
             floobits-share-dir-public
             floobits-summon
             floobits-follow-user
             floobits-follow-mode-toggle
             floobits-clear-highlights)
  :init (add-to-list 'display-buffer-alist '("\\*Floobits\\*" . (display-buffer-below-selected))))




(provide 'init-emacs-edit-collaborate)

;;; init-emacs-edit-collaborate.el ends here
