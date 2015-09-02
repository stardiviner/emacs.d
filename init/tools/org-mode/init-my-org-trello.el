;;; init-my-org-trello.el --- init for Org-Trello
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;;_* org-trello

;;;_ + Usage:
;;
;; - [M-x org-trello-mode] :: activate org-trello
;; - [C-o h] / `org-trello/help-describing-bindings' :: get org-trello keybindings popup.
;;
;; - install key and token (~/.trello/config.el)
;;
;; (setq *consumer-key* "900d9b889d79bd46b6ade7d2d5eb9996")
;; (setq *access-token* "6d52ea5c38c78dd14120a28689849a6bd5825826c415516bd674522f6a8e16f1")
;;
;; org-trello now tries to enforce `symmetry', mainly regarding the sync
;; actions. So by default, an action pushes to trello. Using `C-u', the
;; symmetric action pulls from trello.
;;
;; Workflow:
;;
;; - setup ::
;;
;;   C-c o i or M-x org-trello/install-key-and-token
;;
;; - Connect org-mode buffer to board ::
;;
;;   C-c o I or M-x org-trello/install-board-and-lists-ids
;;
;;   For each org-mode file, you need to connect your org-mode file with a
;;   trello board.
;;
;;   Note This will present you with a list of your actual boards. Select the
;;   one you want and hit enter. This will edit your org-mode file to add the
;;   properties needed.
;;
;; - Create a board ::
;;
;;   C-c o b
;;
;;   You can avoid the previous step and create directly a board from your
;;   org-mode file.
;;
;;   Note: This will create the list from the keywords you use in your org-mode
;;   (cf. org-todo-keywords).
;;
;; - Migrations
;;
;;   org-trello now tries to enforce symmetry, mainly regarding the sync
;;   actions. So by default, an action pushes to trello. Using `C-u', the
;;   symmetric action pulls from trello.
;;
;;   Bindings 	       | Action
;; --------------------+---------------------------------------------------
;;   C-c o c 	       | sync the entity TO trello without its structure
;;   C-u C-c o c 	   | sync the entity FROM trello without its structure
;;   C-c o C 	       | sync the entity and its structure TO trello
;;   C-u C-c o C 	   | sync the entity and its structure FROM trello
;;   C-c o s 	       | sync the buffer TO trello
;;   C-u C-c o s 	   | sync the buffer FROM trello
;;   C-c o a 	       | assign yourself to the card
;;   C-u C-c o a 	   | unassign yourself from the card
;;
;; - Proxy
;;
;;   If you are using a proxy, as org-trello runs a local web server on port
;;   9876 which serves http request, you need to allow this to bypass your
;;   proxy. A solution to this in emacs would be for example (your mileage may
;;   vary):
;;
;;   (setenv "no_proxy" "localhost,127.0.0.0/8")


;; (require 'org-trello)

;; to have org-trello activated for each org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode in an org buffer to activate org-trello

;; TODO: enable org-trello-mode on `org-agenda-files'.

;; (define-key my-org-prefix (kbd "t") 'org-trello-mode)

;; (add-hook 'org-trello-mode-hook
;;           (lambda ()
;;             ;; 1. built-in provide function
;;             (org-trello/install-local-prefix-mode-keybinding! "C-c o t")
;;             ))

;; (org-trello/install-local-prefix-mode-keybinding! "C-c o t")

;; 2. my custom prefix map
;; (unless (boundp 'my-org-trello-map)
;;   (define-prefix-command 'my-org-trello-map))
;; (define-key my-org-prefix (kbd "t") 'my-org-trello-map)

;; ;; enable org-trello
;; (define-key my-org-trello-map (kbd "t") 'org-trello-mode)

;; ;; Install the keys and the access-token            
;; (define-key my-org-trello-map (kbd "I") 'org-trello/install-key-and-token)
;; ;; Connect buffer to board
;; (define-key my-org-trello-map (kbd "i") 'org-trello/install-board-and-lists-ids)
;; ;; Sync org file to board. With C-u prefix, sync org file from trello board
;; (define-key my-org-trello-map (kbd "s") 'org-trello/sync-buffer)
;; ;; Sync an entity to trello. With C-u prefix, sync an entity from trello
;; (define-key my-org-trello-map (kbd "c") 'org-trello/sync-entity)
;; ;; Sync an entity and its structure to trello. With C-u prefix, sync an entity and its structure from trello
;; (define-key my-org-trello-map (kbd "C") 'org-trello/sync-full-entity)
;; ;; Assign yourself to the card. With C-u prefix, unassign yourself from the card
;; (define-key my-org-trello-map (kbd "a") 'org-trello/assign-me)
;; ;; Check that the setup is ok
;; (define-key my-org-trello-map (kbd "d") 'org-trello/check-setup)
;; ;; Clean the org buffer from all org-trello information
;; (define-key my-org-trello-map (kbd "D") 'org-trello/delete-setup)
;; ;; Create a board and attach the org-mode file to it
;; (define-key my-org-trello-map (kbd "b") 'org-trello/create-board)
;; ;; Kill the entity from the board/org buffer. With C-u prefix, kill all entities.
;; (define-key my-org-trello-map (kbd "k") 'org-trello/kill-entity)
;; ;; Kill all entities from the board/org buffer
;; (define-key my-org-trello-map (kbd "K") 'org-trello/kill-all-entities)
;; ;; Jump to current trello card. With C-u prefix, jump to trello board.
;; (define-key my-org-trello-map (kbd "j") 'org-trello/jump-to-card)
;; ;; Jump to current trello board
;; (define-key my-org-trello-map (kbd "J") 'org-trello/jump-to-trello-board)
;; ;; Show the card's comments
;; (define-key my-org-trello-map (kbd "o") 'org-trello/show-card-comments)
;; ;; Show the card's labels
;; (define-key my-org-trello-map (kbd "l") 'org-trello/show-board-labels)
;; ;; Add a comment to the card
;; (define-key my-org-trello-map (kbd "A") 'org-trello/add-card-comments)
;; ;; Update the org buffer with trello board metadata
;; (define-key my-org-trello-map (kbd "u") 'org-trello/update-board-metadata)
;; ;; This help message
;; (define-key my-org-trello-map (kbd "h") 'org-trello/help-describing-bindings)

;;; To activate org-trello only for specific files, there exists a custom
;;; variable dedicated to this:
(setq org-trello-files '("~/Org/Trello/Trello.org" "~/Org/Trello/Startups/Startups.org"))

;; FIXME: local `org-todo-keywords' in org-trello buffers.
;; (add-hook 'org-trello-mode-hook
;;           '(lambda ()
;;              (setq-local org-todo-keywords '((sequence "TODO(@/@)" "Doing(g!)" "|" "DONE(d@/!)")))
;;             
;;              ;; (setq-mode-local org-trello-mode org-todo-keywords
;;              ;;                  '((sequence "TODO(@/@)" "Doing(g!)" "|" "DONE(d@/!)"))
;;              ;;                  )
;;              ))



(provide 'init-my-org-trello)

;;; init-my-org-trello.el ends here
