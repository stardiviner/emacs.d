;;; hexl-isearch.el --- Isearch hexl buffers         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: data, matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar-local hexl-isearch-raw-buffer nil
  "Buffer with the dehexlified content of the hexl buffer for hexl-isearch-mode.
This variable is set in the original hexl-mode buffer.")

(defvar-local hexl-isearch-original-buffer nil
  "This variable is set in the buffer with the dehexlified content.
It points to the corresponding hexl buffer.")

(defun hexl-address (position)
  "Return address of hexl buffer POSITION."
  (save-excursion
    (goto-char position)
    (hexl-current-address)))

(defun hexl-isearch-startup ()
  "Prepare hexl buffer for `hexl-isearch'."
  (let ((original-buf (current-buffer)))
    (setq-local hexl-isearch-raw-buffer (generate-new-buffer " hexl"))
    (setq-local isearch-search-fun-function (lambda () #'hexl-isearch-fun))
    (with-current-buffer hexl-isearch-raw-buffer
      (set-buffer-multibyte nil)
      (setq-local hexl-isearch-original-buffer original-buf)
      (insert-buffer-substring original-buf 1 (buffer-size original-buf))
      (dehexlify-buffer))))

(defun hexl-isearch-end ()
  "Cleanup after `hexl-isearch'."
  (let ((isearch-raw-buffer hexl-isearch-raw-buffer))
    (setq-local hexl-isearch-raw-buffer nil)
    (when (buffer-live-p isearch-raw-buffer)
      (kill-buffer isearch-raw-buffer))))

(defun hexl-isearch-incomplete-input (string)
  "If STRING ends with \\x[0-9] it is considered as incomplete.
Thereby the number of leading backslashes must be odd.
Otherwise the backslash is escaped."
  (and (string-match "\\([\\]+\\)\\(?:x[0-9a-fA-F]?\\|[0-7]\\{0,2\\}\\)\\'" string) ;; TODO: Also consider octal and binary sequences.
       (cl-oddp (length (match-string 1 string)))))

(defun hexl-isearch-fun (string &optional bound noerror count)
  "Search for byte sequence of STRING in hexl buffer.
The arguments BOUND and NOERROR work like in `search-forward'."
  (when bound (setq bound (1+ (hexl-address bound))))
  (when (hexl-isearch-incomplete-input string)
    (signal 'invalid-regexp '("Unmatched [ or [^")))
  (setq string (read (concat "\"" string "\"")))
  (let ((point (1+ (hexl-current-address)))
	match-data)
    (with-current-buffer hexl-isearch-raw-buffer
      (goto-char point)
      (setq point (funcall (if isearch-forward #'re-search-forward #'re-search-backward)
			   (if isearch-regexp
			       string
			     (regexp-quote string))
			   bound noerror count))
      (setq match-data (match-data t nil t)))
    (when point
      (prog1
	  (hexl-goto-address (1- point))
	(set-match-data
	 (mapcar (lambda (el)
		   (if (integerp el)
		       (hexl-address-to-marker (1- el))
		     el))
		 match-data))))))

(defvar-local hexl-isearch-original-fun nil
  "")

;;;###autoload
(define-minor-mode hexl-isearch-mode
  "Search for binary string with isearch in hexl buffer."
  :lighter " hi"
  (if hexl-isearch-mode
      (progn
	(setq-local hexl-isearch-original-fun isearch-search-fun-function) ;; Note: hexl-mode sets this variable and maybe other packages set it also.
	(setq-local isearch-search-fun-function #'hexl-isearch-fun)
	(add-hook 'isearch-mode-hook #'hexl-isearch-startup t t)
	(add-hook 'isearch-mode-end-hook #'hexl-isearch-end t t))
    (if (functionp hexl-isearch-original-fun)
	(setq-local isearch-search-fun-function hexl-isearch-original-fun)
      (setq-local isearch-search-fun-function #'isearch-search-fun-default)) ;;< This fallback should never be used.
    (remove-hook 'isearch-mode-hook #'hexl-isearch-startup t)
    (remove-hook 'isearch-mode-end-hook #'hexl-isearch-end t)))

(defvar hexl-mode-hook) ;; Defined in hexl.
;; hexl-isearch-mode is only available when hexl is already loaded.

;;;###autoload
(add-hook 'hexl-mode-hook
	  (lambda ()
	    (easy-menu-add-item hexl-mode-map '(menu-bar Hexl)
				["Hexl Isearch Mode" (if hexl-isearch-mode (hexl-isearch-mode -1) (hexl-isearch-mode)) :style toggle :selected hexl-isearch-mode] "Go to address")))

(provide 'hexl-isearch)
;;; hexl-isearch.el ends here
