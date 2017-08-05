;;; init-my-org-image.el --- init for Org Image
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; inline images
;;
;; [C-c C-x C-v] - `org-toggle-inline-images'.
;; [C-c C-x C-M-v] - `org-redisplay-inline-images'

(setq org-startup-with-inline-images nil ; disable starup preview inline image to improve org-mode pferformance.
      org-image-actual-width nil ; inline image scale width.
      ;; you need to use:
      ;; - #+ATTR_ORG: :width 400
      ;; - #+ATTR_LATEX: :width 5in
      ;; - #+ATTR_HTML: :width 200px
      ;; org-latex-inline-image-rules
      org-latex-image-default-width "2.0\\linewidth"
      ;; org-latex-image-default-height
      )


;;; set default Org-mode inline image background color.

(defcustom org-inline-image-background nil
  "The color used as the default background for inline images.
  When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(setq org-inline-image-background "#FFFFFF")

;;modified version of original `org-display-inline-images'.
;; append `:background' in `create-image'.
(defun org-display-inline-images (&optional include-linked refresh beg end)
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((case-fold-search t)
           (file-extension-re (org-image-file-name-regexp)))
       (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
         (let ((link (save-match-data (org-element-context))))
           ;; Check if we're at an inline image.
           (when (and (equal (org-element-property :type link) "file")
                      (or include-linked
                          (not (org-element-property :contents-begin link)))
                      (let ((parent (org-element-property :parent link)))
                        (or (not (eq (org-element-type parent) 'link))
                            (not (cdr (org-element-contents parent)))))
                      (org-string-match-p file-extension-re
                                          (org-element-property :path link)))
             (let ((file (expand-file-name
                          (org-link-unescape
                           (org-element-property :path link)))))
               (when (file-exists-p file)
                 (let ((width
                        ;; Apply `org-image-actual-width' specifications.
                        (cond
                         ((not (image-type-available-p 'imagemagick)) nil)
                         ((eq org-image-actual-width t) nil)
                         ((listp org-image-actual-width)
                          (or
                           ;; First try to find a width among
                           ;; attributes associated to the paragraph
                           ;; containing link.
                           (let ((paragraph
                                  (let ((e link))
                                    (while (and (setq e (org-element-property
                                                         :parent e))
                                                (not (eq (org-element-type e)
                                                         'paragraph))))
                                    e)))
                             (when paragraph
                               (save-excursion
                                 (goto-char (org-element-property :begin paragraph))
                                 (when
                                     (re-search-forward
                                      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                                      (org-element-property
                                       :post-affiliated paragraph)
                                      t)
                                   (string-to-number (match-string 1))))))
                           ;; Otherwise, fall-back to provided number.
                           (car org-image-actual-width)))
                         ((numberp org-image-actual-width)
                          org-image-actual-width)))
                       (old (get-char-property-and-overlay
                             (org-element-property :begin link)
                             'org-image-overlay)))
                   (if (and (car-safe old) refresh)
                       (image-refresh (overlay-get (cdr old) 'display))
                     (let ((image (create-image file
                                                (and width 'imagemagick)
                                                nil
                                                :width width
                                                :background org-inline-image-background)))
                       (when image
                         (let* ((link
                                 ;; If inline image is the description
                                 ;; of another link, be sure to
                                 ;; consider the latter as the one to
                                 ;; apply the overlay on.
                                 (let ((parent
                                        (org-element-property :parent link)))
                                   (if (eq (org-element-type parent) 'link)
                                       parent
                                     link)))
                                (ov (make-overlay
                                     (org-element-property :begin link)
                                     (progn
                                       (goto-char
                                        (org-element-property :end link))
                                       (skip-chars-backward " \t")
                                       (point)))))
                           (overlay-put ov 'display image)
                           (overlay-put ov 'face 'default)
                           (overlay-put ov 'org-image-overlay t)
                           (overlay-put
                            ov 'modification-hooks
                            (list 'org-display-inline-remove-overlay))
                           (push ov org-inline-image-overlays)))))))))))))))



;;; A workflow to quickly insert inline images to Org-mode.
;; http://pragmaticemacs.com/emacs/a-workflow-to-quickly-add-photos-to-org-mode-notes/

;; required libraries
(require 'dash)
(require 'swiper)
(require 's)

;; start directory
(defvar my/org-mode-images-dir (expand-file-name "~/Downloads"))

(defun my-insert-org-mode-inline-image ()
  "Insert image from conference directory, rename and add link in current file.

The file is taken from a start directory set by
`my/org-mode-images-dir' and moved to the current directory,
renamed and embedded at the point as an `org-mode' link. The user
is presented with a list of files in the start directory, from
which to select the file to move, sorted by most recent first."
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full file-ext end-file end-file-base end-file-full file-number)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes my/org-mode-images-dir)))

    ;; get target directory
    (setq target-dir (file-name-directory (buffer-file-name)))

    ;; sort list by most recent
    ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    ;; use ivy to select start-file
    (setq start-file (ivy-read
                      (concat "Move selected file to " target-dir ":")
                      file-list-sorted
                      :re-builder #'ivy--regex
                      :sort nil
                      :initial-input nil))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file my/org-mode-images-dir))
    ;; generate target file name from current org section
    (setq file-ext (file-name-extension start-file t))

    ;; get section heading and clean it up
    (setq end-file-base (s-downcase (s-dashed-words (nth 4 (org-heading-components)))))
    ;; shorten to first 40 chars to avoid long file names
    (setq end-file-base (s-left 40 end-file-base))
    ;; number to append to ensure unique name
    (setq file-number 1)
    (setq end-file (concat
                    end-file-base
                    (format "-%s" file-number)
                    file-ext))

    ;; increment number at end of name if file exists
    (while (file-exists-p end-file)
      ;; increment
      (setq file-number (+ file-number 1))
      (setq end-file (concat
                      end-file-base
                      (format "-%s" file-number)
                      file-ext))
      )

    ;; final file name including path
    (setq end-file-full
          (expand-file-name end-file target-dir))
    ;; rename file
    (rename-file start-file-full end-file-full)
    (message "moved %s to %s" start-file-full end-file)
    ;; insert link
    (insert (org-make-link-string (format "file:%s" end-file)))
    ;; insert empty line.
    (insert "\n\n")
    ;; re-display inline images.
    (org-display-inline-images t t)))

(define-key org-mode-map (kbd "C-c C-x C-m") 'my-insert-org-mode-inline-image)



(provide 'init-my-org-image)

;;; init-my-org-image.el ends here
