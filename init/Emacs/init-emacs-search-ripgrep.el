;;; init-emacs-search-ripgrep.el --- init for ripgrep
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'rg-prefix)
  (define-prefix-command 'rg-prefix))
(define-key search-prefix (kbd "r") 'rg-prefix)

;;; [ rg ] -- Use ripgrep (grep and ag replacement) like rgrep.

(use-package rg
  :ensure t
  :after projectile
  :defer t
  :custom (rg-keymap-prefix nil)
  :preface (setq rg-keymap-prefix rg-prefix)
  :commands (rg rg-literal rg-dwim rg-dwim-current-dir rg-dwim-project-dir)
  :bind (:map search-prefix ("s" . rg)
              :map rg-prefix
              ("r" . rg-dwim)
              ("d" . rg-dwim-current-dir)
              ("p" . rg-dwim-project-dir)
              ("C-r" . rg)
              ("R" . rg-literal)
              :map projectile-command-map
              ("s r" . rg-project)
              ;; swap `projectile-ag' keybinding.
              ("s s" . rg-project)
              ("s a" . projectile-ag))
  :init
  (rg-enable-default-bindings)
  (if (fboundp 'wgrep-rg-setup)
      (add-hook 'rg-mode-hook #'wgrep-rg-setup))
  (if (null rg-command-line-flags)
      (setq rg-command-line-flags '("-j 4"))
    (add-to-list 'rg-command-line-flags "-j 4"))
  :config
  (setq rg-group-result t
        ;; rg-command-line-flags '("--debug")
        )
  (add-to-list 'display-buffer-alist
               '("^\\*rg\\*" (display-buffer-reuse-window display-buffer-below-selected)))
  )

;;; search multiple words in files.
(defun rg-files-with-matches-beginning (dir file-type word)
  "Construct literal rg comdnam end part for the beginning WORD in DIR with FILE-TYPE."
  (format "rg --files-with-matches --null --type %s -e \"%s\" %s" file-type word dir))

(defun rg-files-with-matches-middle (word)
  "Construct literal rg comdnam end part for the middle WORD."
  (format " | xargs --null rg --files-with-matches --null -e \"%s\"" word))

(defun rg-files-with-matches-end (word)
  "Construct literal rg comdnam end part for the last WORD."
  (format " | xargs --null rg --heading -e \"%s\"" word))

(defun rg-files-construct-command (dir)
  "Construct a literal rg command to search WORDS in DIR."
  (let* ((words (split-string (read-from-minibuffer "Words: ") " "))
         (file-type (completing-read "file type: "
                                     (mapcar 'car (rg-get-type-aliases))
                                     nil nil "org"))
         (file-type-exts (assq file-type (rg-get-type-aliases))))
    (case (length words)
      (1 (format "rg %s" (car words)))
      (2 (concat (rg-files-with-matches-beginning dir file-type (car words))
                 (rg-files-with-matches-end (cdr words))))
      (3 (concat (rg-files-with-matches-beginning dir file-type (car words))
                 (rg-files-with-matches-middle (cadr words))
                 (rg-files-with-matches-end (car (reverse words)))))
      (t (concat (rg-files-with-matches-beginning dir file-type (car words))
                 ;; KLUDGE: Do I have to use (car (mapcan ...))?
                 (car (mapcan
                       (lambda (word) (list (rg-files-with-matches-middle word)))
                       (delq (car (reverse words)) (cdr words))))
                 (rg-files-with-matches-end (car (reverse words))))))))

(defun rg-search-words-by-files (directory)
  "Search multiple words in files of DIRECTORY as unit instead of line.

The literal rg command looks like this:

rg --files-with-matches --null \"foo\" . | \\
xargs --null rg --files-with-matches --null \"bar\" | \\
... | \\
xargs --null rg --heading \"baz\"

That's it.
"
  ;; interactively select Org default directory or current directory.
  (interactive (list (completing-read "Dir: " `(,(expand-file-name org-directory)
                                                ,default-directory))))
  (let* ((command (rg-files-construct-command directory)))
    ;; FIXME rg report 0 match, but the result has matches.
    ;; dive into rg.el source code to figure out.
    ;; use `rg-define-search'
    (compilation-start command 'rg-mode)))

(define-key rg-prefix (kbd "M-o") 'rg-search-words-by-files)
(define-key Org-prefix (kbd "s") 'rg-search-words-by-files)

;;; [ helm-rg ]

(use-package helm-rg
  :ensure t
  :commands (helm-rg)
  :bind (:map rg-prefix ("h" . helm-rg)))


(provide 'init-emacs-search-ripgrep)

;;; init-emacs-search-ripgrep.el ends here
