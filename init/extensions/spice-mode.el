;;; spice-mode.el --- major mode providing a spice mode hook for fontification
;;;* Last edited: Feb 23 12:55 1995 (cvieri)

;; Author: 1994 Carlin J. Vieri, MIT AI Lab <cvieri@ai.mit.edu>
;; Keywords: Spice editing major-mode

;; Copyright (C) 1994, MIT Artificial Intelligence Lab

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; If you have any questions about this mode, feel free to contact me
;; at the following address:  cvieri@ai.mit.edu.  If you make any
;; modifications or bug fixes, I'd like to hear about them.

;; To use spice-mode, add the following to your .emacs file.  This
;; assumes that you will use the .sp extension for your spice source deck:
;; (autoload 'spice-mode "spice-mode" "Spice  Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.sp$"  . spice-mode)
;;            ) auto-mode-alist))
;;
(defvar spice-mode-syntax-table nil
  "Syntax table used in spice-mode buffers.")
(if spice-mode-syntax-table
    ()
  (setq spice-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?* "<" spice-mode-syntax-table)
  (modify-syntax-entry ?$ "<" spice-mode-syntax-table)
  (modify-syntax-entry ?\n ">" spice-mode-syntax-table))

(defvar spice-mode-abbrev-table nil
  "Abbrev table in use in spice-mode buffers.")
(define-abbrev-table 'spice-mode-abbrev-table ())

(defvar spice-mode-map ()
  "Keymap used in spice-mode.")
(if spice-mode-map
    ()
  (setq spice-mode-map (make-sparse-keymap))
  (install-common-language-commands spice-mode-map))

;; ======================================================================
;; spice-mode main entry point
;; ======================================================================
;;;###autoload
(defun spice-mode ()
  "Major mode for editing spice decks. 0.0
No bug report notification is currently available.  No indentation is
implemented; this mode provides a fontification hook.  Common language
commands and key bindings are linked through this command.  Do not use
a -*- Mode -*- line in a spice deck as the first card in the deck is
defined to be the title card.  Rather, autoload spice-mode through
your .emacs file.  turning on Spice mode calls the value of the
variable `spice-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map spice-mode-map)
  (set-syntax-table spice-mode-syntax-table)
  (setq major-mode 'spice-mode
	mode-name "Spice"
	local-abbrev-table verilog-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'comment-start) "* ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (run-hooks 'spice-mode-hook)
)



;;; Hacks to implement the find function menu bar for spice mode subcircuits.
;;; Fortunately spice only provides one means of abstraction so the parsing is
;;; very easy. 
(defconst fume-function-name-regexp-spice
  "^[\.]\\(subckt\\)[ \t]+\\([A-Za-z0-9_+-]*\\)[ \t]*"
  "Expression to parse Spice subcircuit names.")

(defun fume-find-next-spice-function-name (buffer)
  "Searches for the next spice subcircuit name in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;; hook in the spice mode regular expression above into the association list of
;; regexps used by the function menu generator
(defvar fume-function-name-regexp-alist nil)
(setq fume-function-name-regexp-alist
      (purecopy
       (append
        fume-function-name-regexp-alist
        (list
         '(spice-mode . fume-function-name-regexp-spice)))))


;; hook in the search method above into the association list used by the
;; function menu generating code
(defvar fume-find-function-name-method-alist nil)
(setq fume-find-function-name-method-alist
      (purecopy
       (append
        fume-find-function-name-method-alist
        (list '(spice-mode . fume-find-next-spice-function-name)))))

;; this is sometimes useful
(provide 'spice-mode)


;;; spice-mode.el ends here
