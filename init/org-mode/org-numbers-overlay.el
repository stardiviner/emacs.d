;;; org-numbers-overlay.el --- 


;;; Commentary:




;;; Code:

;; (define-minor-mode org-numbers-overlay-mode
;;   "Add overlays to org headings which number them"
;;   nil " *1." nil
;;
;;   (let ((hooks '(after-save-hook
;;                  org-insert-heading-hook))
;;         (funcs '(org-promote
;;                                         ;                 org-cycle-level
;;                  org-promote-subtree
;;                  org-demote
;;                  org-demote-subtree
;;                  org-move-subtree-up
;;                  org-move-subtree-down
;;                  org-move-item-down
;;                  org-move-item-up
;;                  org-cut-subtree
;;                  org-insert-todo-heading
;;                  org-insert-todo-subheading
;;                  org-meta-return
;;                  org-set-property)))
;;     (if org-numbers-overlay-mode
;;         (progn
;;           (org-numbers-overlay-update)
;;           (dolist (fn funcs)
;;             (advice-add fn :after #'org-numbers-overlay-update))
;;           (dolist (hook hooks)
;;             (add-hook hook #'org-numbers-overlay-update)))
;;
;;       (progn
;;         (dolist (fn funcs)
;;           (advice-add fn :after #'org-numbers-overlay-update))
;;         (dolist (hook hooks)
;;           (remove-hook hook #'org-numbers-overlay-update))
;;
;;         (remove-overlays (point-min) (point-max) 'type 'org-number)))))

(defun org-numbers-overlay-update (&rest args)
  (when org-numbers-overlay-mode
    (let ((continue t)
          (levels (make-vector 10 0))
          (any-unnumbered (member "UNNUMBERED" (org-buffer-property-keys))))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (or (outline-on-heading-p)
            (outline-next-heading))

        (overlay-recenter (point-max))
        (remove-overlays (point-min) (point-max) 'type 'org-number)

        (while continue
          (let* ((detail (org-heading-components))
                 (level (- (car detail) 1)))

            (when (or (not any-unnumbered)
                      (org-entry-get (point) "UNNUMBERED" 'selective))
              (let* ((lcounter (1+ (aref levels level)))
                     text)

                (aset levels level lcounter)

                (loop for i from (1+ level) to 9
                      do (aset levels i 0))

                (loop for i across levels
                      until (zerop i)
                      do (setf text (if text
                                        (format "%s.%d" text i)
                                      (format " %d" i))))

                (let  ((o (make-overlay (point) (+ (point) (car detail)) nil t t)))
                  (overlay-put o 'type 'org-number)
                  (overlay-put o 'evaporate t)
                  (overlay-put o 'after-string text)))))
          (setq continue (outline-next-heading))

          )))))



(provide 'org-numbers-overlay)


;;; org-numbers-overlay.el ends here
