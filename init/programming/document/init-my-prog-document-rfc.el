;;; init-my-prog-document-rfc.el --- init for RFC
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ irfc ]

;;; Usage:
;;
;; `irfc-render-toggle'         Toggle render status with RFC buffer.
;; `irfc-quit'                  Quit RFC buffer.
;; `irfc-visit'                 Ask for RFC number and visit document.
;; `irfc-reference-goto'        Ask for RFC reference and jump to it.
;; `irfc-head-goto'             Ask for heading name and jump to it.
;; `irfc-head-number-goto'      Ask for heading number and jump to it.
;; `irfc-follow'                Visit RFC document around point.
;; `irfc-table-jump'            Switch between table and content.
;; `irfc-page-goto'             Goto page.
;; `irfc-page-next'             Jump next page.
;; `irfc-page-prev'             Jump previous page.
;; `irfc-page-first'            Jump first page.
;; `irfc-page-last'             Jump last page.
;; `irfc-page-table'            Jump table page.
;; `irfc-head-next'             Jump next heading.
;; `irfc-head-prev'             Jump previous heading.
;; `irfc-rfc-link-next'         Jump next RFC link.
;; `irfc-rfc-link-prev'         Jump previous RFC link.
;; `irfc-scroll-up-one-line'    Scroll up one line.
;; `irfc-scroll-down-one-line'  Scroll down one line.
;;
;; TAB             rfcview-next-button
;; q               rfcview-quit

;;; View mode
;; H, h, ?	 This message.
;; Digits	provide prefix arguments.
;; -	negative prefix argument.
;; <	move to the beginning of buffer.
;; >	move to the end of buffer.
;; o	scroll so that buffer end is at last line of window.
;; SPC	scroll forward "page size" lines.
;; 	  With prefix scroll forward prefix lines.
;; DEL, S-SPC  scroll backward "page size" lines.
;; 	      With prefix scroll backward prefix lines.
;; z	like  SPC  but with prefix sets "page size" to prefix.
;; w	like  DEL  but with prefix sets "page size" to prefix.
;; d	scroll forward "half page size" lines.  With prefix, sets
;; 	  "half page size" to prefix lines and scrolls forward that much.
;; u	scroll backward "half page size" lines.  With prefix, sets
;; 	  "half page size" to prefix lines and scrolls backward that much.
;; RET, LFD  scroll forward one line.  With prefix scroll forward prefix line(s).
;; y	scroll backward one line.  With prefix scroll backward prefix line(s).
;; F	revert-buffer if necessary and scroll forward.
;; 	  Use this to view a changing file.
;; =	prints the current line number.
;; %	goes prefix argument (default 100) percent into buffer.
;; g	goes to line given by prefix argument (default first line).
;; .	set the mark.
;; x	exchanges point and mark.
;; @	return to mark and pops mark ring.
;; 	  Mark ring is pushed at start of every successful search and when
;; 	  jump to line occurs.  The mark is set on jump to buffer start or end.
;; m	save current position in character register.
;; '	go to position saved in character register.
;; s	do forward incremental search.
;; r	do reverse incremental search.
;; /	searches forward for regular expression, starting after current page.
;; 	  ! and @ have a special meaning at the beginning of the regexp.
;; 	  ! means search for a line with no match for regexp.  @ means start
;; 	  search at beginning (end for backward search) of buffer.
;; \	searches backward for regular expression, starting before current page.
;; n	searches forward for last regular expression.
;; p	searches backward for last regular expression.
;; q	quit View mode, restoring this window and buffer to previous state.
;; 	  q is the normal way to leave view mode.
;; e	exit View mode but stay in current buffer.  Use this if you started
;; 	  viewing a buffer (file) and find out you want to edit it.
;; 	  This command restores the previous read-only status of the buffer.
;; E	exit View mode, and make the current buffer editable
;; 	  even if it was not editable before entry to View mode.
;; Q	quit View mode, restoring all windows to previous state.
;; c	quit View mode and maybe switch buffers, but don't kill this buffer.
;; C	quit View mode, kill current buffer and go back to other buffer.


(require 'irfc)

(setq irfc-directory (concat user-emacs-directory "documentations/RFC")) ; set RFC database storage.
(setq irfc-assoc-mode t)        ; RFC documents are associated with `irfc-mode'.

(setq irfc-highlight-requirement-keywords t
      irfc-requirement-keywords '("MUST" "MUST NOT" "REQUIRED"
                                  "SHALL" "SHALL NOT" "SHOULD" "SHOULD NOT"
                                  "RECOMMENDED" "NOT RECOMMENDED"
                                  "MAY" "OPTIONAL" "NOT")
      irfc-requirement-keyword-overlay nil
      irfc-highlight-references t)

(define-key my-prog-help-document-map (kbd "r") 'irfc-visit)



;;; [ rfcview ]

(require 'rfcview)

(setq rfcview-index-location (concat user-emacs-directory "documentations/RFC/rfc-index.txt")
      rfcview-rfc-location-pattern (concat user-emacs-directory "documentations/RFC/rfc%s.txt")
      rfcview-std-location-pattern (concat user-emacs-directory "documentations/RFC/std%s.txt")
      )

(define-key rfcview-mode-map (kbd "q") 'rfcview-quit)
(define-key rfcview-mode-map (kbd "j") 'rfcview-find-rfc)
(define-key rfcview-mode-map (kbd "i") 'rfcview-textmode)
(define-key rfcview-mode-map (kbd "g") 'rfcview-goto-link)
(define-key rfcview-mode-map (kbd "I") 'rfcview-find-index)
(define-key rfcview-mode-map (kbd "TAB") 'rfcview-next-button)
(define-key rfcview-mode-map (kbd "t") 'rfcview-hyperlink-contents)
(define-key rfcview-mode-map (kbd "s") 'rfcview-find-location-of-rfc)
(define-key rfcview-mode-map (kbd "d") 'rfcview-imenu-index-function)


(provide 'init-my-prog-document-rfc)

;;; init-my-prog-document-rfc.el ends here
