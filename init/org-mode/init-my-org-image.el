;;; init-my-org-image.el --- init for Org Image
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ inline images ]

;; (setq org-startup-with-inline-images t)  ; disable starup preview inline image to improve org-mode pferformance.

;;; manually specify inline image size.
;; you need to use:
;; - #+ATTR_ORG: :width 400
;; - #+ATTR_LATEX: :width 5in
;; - #+ATTR_HTML: :width 200px
(use-package org
  :ensure-system-package (convert . "sudo pacman -S --noconfirm imagemagick")
  :config
  (setq org-image-actual-width nil) ; inline image scale width.
  )


;;; set default Org-mode inline image background color based on color-theme background color.
(defun org-display-inline-images--with-color-theme-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-background 'default))
            props)))

(advice-add 'create-image :filter-args
            #'org-display-inline-images--with-color-theme-background-color)


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

(define-key org-link-prefix (kbd "i") 'my-insert-org-mode-inline-image)



(provide 'init-my-org-image)

;;; init-my-org-image.el ends here
