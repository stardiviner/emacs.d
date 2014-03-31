;;; mu-cite.el --- yet another citation tool for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, news, citation

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - How to use
;;   1. Bytecompile this file and copy it to the apropriate directory.
;;   2. Put the following lines in your ~/.emacs file:
;;      For EMACS 19 or later and XEmacs
;;		(autoload 'mu-cite-original "mu-cite" nil t)
;;		;; for all but message-mode
;;		(add-hook 'mail-citation-hook (function mu-cite-original))
;;		;; for message-mode only
;;		(setq message-cite-function (function mu-cite-original))
;;      For EMACS 18
;;		;; for all but mh-e
;;		(add-hook 'mail-yank-hooks (function mu-cite-original))
;;		;; for mh-e only
;;		(add-hook 'mh-yank-hooks (function mu-cite-original))

;;; Code:

;; Pickup some macros, e.g. `with-temp-buffer', for old Emacsen.
(require 'poe)

(require 'pcustom)
(require 'std11)
(require 'alist)

(autoload 'mu-cite-get-prefix-method "mu-register")
(autoload 'mu-cite-get-prefix-register-method "mu-register")
(autoload 'mu-cite-get-prefix-register-verbose-method "mu-register")

(autoload 'mu-bbdb-get-prefix-method "mu-bbdb")
(autoload 'mu-bbdb-get-prefix-register-method "mu-bbdb")
(autoload 'mu-bbdb-get-prefix-register-verbose-method "mu-bbdb")


;;; @ version
;;;

(defconst mu-cite-version "8.1")


;;; @ macro
;;;

(defmacro mu-cite-remove-text-properties (string)
  "Remove text properties from STRING which is read from minibuffer."
  (if (or (featurep 'xemacs)
	  (boundp 'minibuffer-allow-text-properties);; Emacs 20.1 or later.
	  (not (fboundp 'set-text-properties)));; under Emacs 19.7.
      string
    (` (let ((obj (copy-sequence (, string))))
	 (set-text-properties 0 (length obj) nil obj)
	 obj))))


;;; @ set up
;;;

(defgroup mu-cite nil
  "Yet another citation tool for GNU Emacs."
  :prefix "mu-cite-"
  :group 'mail
  :group 'news)

(defvar mu-cite-default-methods-alist
  (list (cons 'from
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "From"))))
	(cons 'date
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "Date"))))
	(cons 'message-id
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "Message-Id"))))
	(cons 'subject
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "Subject"))))
	(cons 'ml-name
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "X-Ml-Name"))))
	(cons 'ml-count (function mu-cite-get-ml-count-method))
	(cons 'address-structure
	      (function
	       (lambda ()
		 (car
		  (std11-parse-address-string (mu-cite-get-value 'from))))))
	(cons 'full-name
	      (function
	       (lambda ()
		 (std11-full-name-string
		  (mu-cite-get-value 'address-structure)))))
	(cons 'address
	      (function
	       (lambda ()
		 (std11-address-string
		  (mu-cite-get-value 'address-structure)))))
	(cons 'id
	      (function
	       (lambda ()
		 (let ((ml-name (mu-cite-get-value 'ml-name)))
		   (if ml-name
		       (concat "["
			       ml-name
			       " : No."
			       (mu-cite-get-value 'ml-count)
			       "]")
		     (mu-cite-get-value 'message-id))))))
	(cons 'in-id
	      (function
	       (lambda ()
		 (let ((id (mu-cite-get-value 'id)))
		   (if id
		       (format ">>>>> In %s \n" id)
		     "")))))
	(cons 'x-attribution
	      (function
	       (lambda ()
		 (mu-cite-get-field-value "X-Attribution"))))
	;; mu-register
	(cons 'prefix (function mu-cite-get-prefix-method))
	(cons 'prefix-register
	      (function mu-cite-get-prefix-register-method))
	(cons 'prefix-register-verbose
	      (function mu-cite-get-prefix-register-verbose-method))
	;; mu-bbdb
	(cons 'bbdb-prefix
	      (function mu-bbdb-get-prefix-method))
	(cons 'bbdb-prefix-register
	      (function mu-bbdb-get-prefix-register-method))
	(cons 'bbdb-prefix-register-verbose
	      (function mu-bbdb-get-prefix-register-verbose-method))
	))


;;; @ formats
;;;

(defcustom mu-cite-cited-prefix-regexp
  "\\(^[^ \t\n<>]+>+[ \t]*\\|^[ \t]*$\\)"
  "Regexp to match the citation prefix.
If match, mu-cite doesn't insert citation prefix."
  :type 'regexp
  :group 'mu-cite)

(defcustom mu-cite-prefix-format '(prefix-register-verbose "> ")
  "List to represent citation prefix.
Each elements must be a string or a method name."
  :type (list
	 'repeat
	 (list
	  'group
	  :convert-widget
	  (function
	   (lambda (widget)
	     (list
	      'choice
	      :tag "Method or String"
	      :args
	      (nconc
	       (mapcar
		(function (lambda (elem) (list 'choice-item (car elem))))
		mu-cite-default-methods-alist)
	       '((symbol :tag "Method")
		 (const :tag "-" nil)
		 (choice-item :tag "String: \"> \"" "> ")
		 (string))))))))
  :set (function (lambda (symbol value)
		   (set-default symbol (delq nil value))))
  :group 'mu-cite)

(defcustom mu-cite-top-format '(in-id ">>>>>\t" from " wrote:\n")
  "List to represent top string of citation.
Each elements must be a string or a method name."
  :type (list
	 'repeat
	 (list
	  'group
	  :convert-widget
	  (function
	   (lambda (widget)
	     (list 'choice
		   :tag "Method or String"
		   :args
		   (nconc
		    (mapcar
		     (function (lambda (elem) (list 'choice-item (car elem))))
		     mu-cite-default-methods-alist)
		    '((symbol :tag "Method")
		      (const :tag "-" nil)
		      (choice-item :tag "String: \">>>>>\\t\"" ">>>>>\t")
		      (choice-item :tag "String: \" wrote:\\n\"" " wrote:\n")
		      (string :tag "String"))))))))
  :set (function (lambda (symbol value)
		   (set-default symbol (delq nil value))))
  :group 'mu-cite)


;;; @ hooks
;;;

(defcustom mu-cite-instantiation-hook nil
  "List of functions called just before narrowing to the message."
  :type 'hook
  :group 'mu-cite)

(defcustom mu-cite-pre-cite-hook nil
  "List of functions called before citing a region of text."
  :type 'hook
  :group 'mu-cite)

(defcustom mu-cite-post-cite-hook nil
  "List of functions called after citing a region of text."
  :type 'hook
  :group 'mu-cite)


;;; @ field
;;;

(defvar mu-cite-get-field-value-method-alist nil
  "Alist major-mode vs. function to get field-body of header.")

(defun mu-cite-get-field-value (name)
  "Return the value of the header field NAME.
If the field is not found in the header, a method function which is
registered in variable `mu-cite-get-field-value-method-alist' is called."
  (or (std11-field-body name)
      (let ((method (assq major-mode mu-cite-get-field-value-method-alist)))
	(if method
	    (funcall (cdr method) name)))))


;;; @ item methods
;;;

;;; @@ ML count
;;;

(defcustom mu-cite-ml-count-field-list
  '("X-Ml-Count" "X-Mail-Count" "X-Seqno" "X-Sequence" "Mailinglist-Id")
  "List of header fields which contains a sequence number of the mailing list."
  :type '(repeat (choice :tag "Field Name"
			 (choice-item "X-Ml-Count")
			 (choice-item "X-Mail-Count")
			 (choice-item "X-Seqno")
			 (choice-item "X-Sequence")
			 (choice-item "Mailinglist-Id")
			 (const :tag "-" nil)
			 (string :tag "Other")))
  :set (function (lambda (symbol value)
		   (set-default symbol (delq nil value))))
  :group 'mu-cite)

(defun mu-cite-get-ml-count-method ()
  "A mu-cite method to return a ML-count.
This function searches a field about ML-count, which is specified by
the variable `mu-cite-ml-count-field-list', in a header.
If the field is found, the function returns a number part of the
field.

Notice that please use (mu-cite-get-value 'ml-count)
instead of to call the function directly."
  (let ((field-list mu-cite-ml-count-field-list))
    (catch 'tag
      (while field-list
	(let* ((field (car field-list))
	       (ml-count (mu-cite-get-field-value field)))
	  (if (and ml-count (string-match "[0-9]+" ml-count))
	      (throw 'tag (match-string 0 ml-count)))
	  (setq field-list (cdr field-list)))))))


;;; @ fundamentals
;;;

(defvar mu-cite-methods-alist nil)

(defun mu-cite-make-methods ()
  (setq mu-cite-methods-alist
	(copy-alist mu-cite-default-methods-alist))
  (run-hooks 'mu-cite-instantiation-hook))

(defun mu-cite-get-value (item)
  "Return a current value of ITEM."
  (let ((ret (cdr (assoc item mu-cite-methods-alist))))
    (if (functionp ret)
	(prog1
	    (setq ret (save-excursion (funcall ret)))
	  (set-alist 'mu-cite-methods-alist item ret))
      ret)))

(defun mu-cite-eval-format (list)
  (mapconcat (function
	      (lambda (elt)
		(cond ((stringp elt) elt)
		      ((symbolp elt) (mu-cite-get-value elt)))))
	     list ""))


;;; @ main function
;;;

;;;###autoload
(defun mu-cite-original ()
  "Citing filter function.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard."
  (interactive)
  (mu-cite-make-methods)
  (save-restriction
    (if (< (mark t) (point))
	(exchange-point-and-mark))
    (narrow-to-region (point)(point-max))
    (run-hooks 'mu-cite-pre-cite-hook)
    (let ((last-point (point))
	  (top (mu-cite-eval-format mu-cite-top-format))
	  (prefix (mu-cite-eval-format mu-cite-prefix-format)))
      (if (re-search-forward "^-*$" nil nil)
	  (forward-line 1))
      (widen)
      (delete-region last-point (point))
      (insert top)
      (setq last-point (point))
      (while (< (point)(mark t))
	(or (looking-at mu-cite-cited-prefix-regexp)
	    (insert prefix))
	(forward-line 1))
      (goto-char last-point))
    (run-hooks 'mu-cite-post-cite-hook)))


;;; @ message editing utilities
;;;

(defcustom citation-mark-chars ">}|"
  "String of characters for citation delimiter."
  :type 'string
  :group 'mu-cite)

(defcustom citation-disable-chars "<{"
  "String of characters not allowed as citation-prefix."
  :type 'string
  :group 'mu-cite)

(defun-maybe-cond char-category (character)
  "Return a string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character,
TABLE defaults to the current buffer's category table."
  ((and (subr-fboundp 'char-category-set)
	(subr-fboundp 'category-set-mnemonics))
   (category-set-mnemonics (char-category-set character))
   )
  ((fboundp 'char-category-list)
   (mapconcat (lambda (chr)
		(char-to-string (int-char chr)))
	      (char-category-list character)
	      "")
   )
  ((boundp 'NEMACS)
   (if (< (char-int character) 128)
       "al"
     "j")
   )
  (t
   (if (< (char-int character) 128)
       "al"
     "l")
   ))

(defun detect-paragraph-cited-prefix ()
  (save-excursion
    (goto-char (point-min))
    (let ((i 0)
	  (prefix
	   (buffer-substring (line-beginning-position)
			     (line-end-position))))
      (let ((init prefix)
	    str ret)
	(while (and (= (forward-line) 0)
		    (setq str (buffer-substring
			       (progn (beginning-of-line)(point))
			       (progn (end-of-line)(point))))
		    (setq ret (string-compare-from-top prefix str)))
	  (setq prefix
		(if (stringp ret)
		    ret
		  (car (cdr ret))))
	  (or (string-equal init prefix)
	      (setq i (1+ i)))))
      (cond ((> i 1) prefix)
	    ((> i 0)
	     (goto-char (point-min))
	     (save-restriction
	       (narrow-to-region (point)
				 (+ (point)(length prefix)))
	       (goto-char (point-max))
	       (if (re-search-backward
		    (concat "[" citation-mark-chars "]") nil t)
		   (progn
		     (goto-char (match-end 0))
		     (if (looking-at "[ \t]+")
			 (goto-char (match-end 0)))
		     (buffer-substring (point-min)(point)))
		 prefix)))
	    ((progn
	       (goto-char (point-max))
	       (re-search-backward
		(concat "[" citation-disable-chars "]") nil t)
	       (re-search-backward
		(concat "[" citation-mark-chars "]") nil t))
	     (goto-char (match-end 0))
	     (if (looking-at "[ \t]+")
		 (goto-char (match-end 0)))
	     (buffer-substring (line-beginning-position)(point)))
	    (t "")))))

;;;###autoload
(defun fill-cited-region (beg end)
  "Fill each of the paragraphs in the region as a cited text."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char end)
      (and (search-backward "\n" nil t)
	   (setq end (match-end 0)))
      (narrow-to-region beg end)
      (let* ((fill-prefix (detect-paragraph-cited-prefix))
	     (fill-column (max (+ 1 (current-left-margin)
				  (string-width fill-prefix))
			       (current-fill-column)))
	     (pat (concat fill-prefix "\n"))
	     filladapt-mode)
	(goto-char (point-min))
	(while (search-forward pat nil t)
	  (let ((b (match-beginning 0))
		(e (match-end 0)))
	    (delete-region b e)
	    (if (and (> b (point-min))
		     (let ((cat (char-category
				 (char-before b))))
		       (or (string-match "a" cat)
			   (string-match "l" cat))))
		(insert " "))))
	(goto-char (point-min))
	(fill-region (point-min) (point-max))))))

;;;###autoload
(defun compress-cited-prefix ()
  "Compress nested cited prefixes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
    (while (re-search-forward
	    (concat "^\\([ \t]*[^ \t\n" citation-mark-chars "]*["
		    citation-mark-chars "]\\)+") nil t)
      (let* ((b (match-beginning 0))
	     (e (match-end 0))
	     (prefix (buffer-substring b e))
	     ps pe (s 0)
	     (nest (let ((i 0))
		     (if (string-match "<[^<>]+>" prefix)
			 (setq prefix
			       (substring prefix 0 (match-beginning 0))))
		     (while (string-match
			     (concat "\\([" citation-mark-chars "]+\\)[ \t]*")
			     prefix s)
		       (setq i (+ i (- (match-end 1)(match-beginning 1)))
			     ps s
			     pe (match-beginning 1)
			     s (match-end 0)))
		     i)))
	(if (and ps (< ps pe))
	    (progn
	      (delete-region b e)
	      (insert (concat (substring prefix ps pe)
			      (make-string nest ?>)))))
	))))

(defun replace-top-string (old new)
  (interactive "*sOld string: \nsNew string: ")
  (while (re-search-forward
	  (concat "^" (regexp-quote old)) nil t)
    (replace-match new)))

(defun string-compare-from-top (str1 str2)
  (let* ((len1 (length str1))
	 (len2 (length str2))
	 (len (min len1 len2))
	 (p 0)
	 c1 c2)
    (while (and (< p len)
		(progn
		  (setq c1 (sref str1 p)
			c2 (sref str2 p))
		  (eq c1 c2)))
      (setq p (char-next-index c1 p)))
    (and (> p 0)
	 (let ((matched (substring str1 0 p))
	       (r1 (and (< p len1)(substring str1 p)))
	       (r2 (and (< p len2)(substring str2 p))))
	   (if (eq r1 r2)
	       matched
	     (list 'seq matched (list 'or r1 r2)))))))


;;; @ end
;;;

(provide 'mu-cite)

(run-hooks 'mu-cite-load-hook)

;;; mu-cite.el ends here
