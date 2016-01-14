;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ swiper/ivy-mode ]

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper)
  ;; Ivy-mode
  (setq ivy-use-virtual-buffers t
        ivy-height 5
        ivy-count-format "(%d/%d) "
        ivy-wrap nil
        )
  
  (set-face-attribute 'ivy-confirm-face nil
                      :inherit nil
                      :foreground "black" :background "khaki"
                      :weight 'bold)
  (set-face-attribute 'ivy-current-match nil
                      ;; :foreground "white" :background "#004A5D"
                      ;; :weight 'normal :box nil
                      ;; Sci-Fi style
                      :foreground "white" :background "#004A5D"
                      :box '(:color "cyan" :line-width -1)
                      )
  (set-face-attribute 'ivy-match-required-face nil
                      :inherit 'minibuffer-prompt
                      :foreground "dark red" :background " "
                      :weight 'bold :box nil
                      )
  ;; the string between matches
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                      :inherit nil
                      :foreground " " :background " "
                      :weight 'normal :box nil
                      )
  ;; first match part.
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "green"
                      :weight 'normal :box nil
                      )
  ;; second match part.
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "orange"
                      :weight 'normal :box nil
                      )
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :inherit 'ivy-minibuffer-match-face-1
                      :foreground "dodger blue"
                      :weight 'normal :box nil
                      )
  
  ;; Custom Functions

  ;; {Imenu}
  (defun ivy-imenu-get-candidates-from (alist  &optional prefix)
    (cl-loop for elm in alist
             nconc (if (imenu--subalist-p elm)
                       (ivy-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        (concat prefix (if prefix ".") (car elm)))
                     (and (cdr elm) ; bug in imenu, should not be needed.
                          (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                          (list (cons (concat prefix (if prefix ".") (car elm))
                                      (copy-marker (cdr elm))))))))

  (defun ivy-imenu-goto ()
    "Go to buffer position"
    (interactive)
    (let ((imenu-auto-rescan t) items)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (setq items (imenu--make-index-alist t))
      (ivy-read "imenu items:"
                (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
                :action (lambda (k) (goto-char k)))))

  ;; {Bookmarks}
  (defun ivy-bookmark-goto ()
    "Open ANY bookmark"
    (interactive)
    (let (bookmarks filename)
      ;; load bookmarks
      (unless (featurep 'bookmark)
        (require 'bookmark))
      (bookmark-maybe-load-default-file)
      (setq bookmarks (and (boundp 'bookmark-alist) bookmark-alist))

      ;; do the real thing
      (ivy-read "bookmarks:"
                (delq nil (mapcar (lambda (bookmark)
                                    (let (key)
                                      ;; build key which will be displayed
                                      ;; (cond
                                      ;;  ((and (assoc 'filename bookmark) (cdr (assoc 'filename bookmark)))
                                      ;;   (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'filename bookmark)))))
                                      ;;  ((and (assoc 'location bookmark) (cdr (assoc 'location bookmark)))
                                      ;;   ;; bmkp-jump-w3m is from bookmark+
                                      ;;   (unless (featurep 'bookmark+)
                                      ;;     (require 'bookmark+))
                                      ;;   (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'location bookmark)))))
                                      ;;  (t
                                      ;;   (setq key (car bookmark))))
                                      ;; re-shape the data so full bookmark be passed to ivy-read:action

                                      (setq key (car bookmark))
                                      
                                      (cons key bookmark)))
                                  bookmarks))
                :action (lambda (bookmark)
                          (bookmark-jump bookmark)))
      ))

  (global-set-key [remap bookmark-jump] 'ivy-bookmark-goto)
  
  ;; (ivy-mode 1)
  )


;;; [ counsel ]

(use-package counsel)


(provide 'init-ivy)

;;; init-ivy.el ends here
