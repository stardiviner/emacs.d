;;; init-my-emacs-pdf.el --- init for PDF.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ doc-view ]

;;; Usage:
;;
;; - [] ::

;; (require 'doc-view)
;;
;; (setq doc-view-continuous t
;;       ;; doc-view--image-type ; 'png, 'tiff
;;       ;; doc-view-doc-type ; Can be `dvi', `pdf', or `ps'.
;;       ;; doc-view-cache-directory "/tmp/docview1000"
;;       doc-view-scale-internally t
;;       doc-view-image-width 850
;;       )


;;; [ pdf-tools ] -- Emacs support library for PDF files.

;;; About
;;
;; PDF Tools is, among other things, a replacement of DocView for PDF files. The
;; key difference is, that pages are not prerendered by e.g. ghostscript and
;; stored in the file-system, but rather created on-demand and stored in memory.
;;
;; This rendering is performed by a special library named, for whatever reason,
;; poppler, running inside a server programm. This programm is called epdfinfo
;; and it’s job is it to successively read requests from Emacs and produce the
;; proper results, i.e. the PNG image of a PDF page.
;;
;; Actually, displaying PDF files is just one part of PDF Tools. Since poppler
;; can provide us with all kinds of informations about a document and is also
;; able to modify it, there is a lot more we can do with it. Watch

;;; Features
;;
;; View
;;
;; View PDF documents in a buffer with DocView-like bindings.
;;
;; Isearch
;;
;; Interactively search PDF documents like any other buffer. (Though there is currently no regexp support.)
;;
;; Follow links
;;
;; Click on highlighted links, moving to some part of a different page, some
;; external file, a website or any other URI. Links may also be followed by
;; keyboard commands.
;;
;; Annotations
;;
;; Display and list text and markup annotations (like underline), edit their
;; contents and attributes (e.g. color), move them around, delete them or create
;; new ones and then save the modifications back to the PDF file.
;;
;; Attachments
;;
;; Save files attached to the PDF-file or list them in a dired buffer.
;;
;; Outline
;;
;; Use imenu or a special buffer to examine and navigate the PDF’s outline.
;;
;; SyncTeX
;;
;; Jump from a position on a page directly to the TeX source and vice-versa.
;;
;; Misc
;;
;;   - Display PDF’s metadata.
;;   - Mark a region and kill the text from the PDF.
;;   - Search for occurrences of a string.
;;   - Keep track of visited pages via a history.

;; (require 'pdf-tools)


(provide 'init-my-emacs-pdf)

;;; init-my-emacs-pdf.el ends here
