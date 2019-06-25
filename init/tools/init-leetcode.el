;;; init-leetcode.el --- init for LeetCode

;;; Time-stamp: <2019-06-24 21:10:42 stardiviner>

;;; Commentary:



;;; Code:

;;; [ leetcode.el ] -- An Emacs LeetCode client.

(leaf leetcode
  :package t
  ;; :straight (leetcode :type git :host github :repo "kaiwk/leetcode.el")
  :ensure request-deferred
  :commands (leetcode)
  :config (setq leetcode-account "stardiviner"
                leetcode-password (my/json-read-value my/account-file 'leetcode)
                leetcode-prefer-language "python3") ; `clojure' is not supported by LeetCode yet.
  )



(provide 'init-leetcode)

;;; init-leetcode.el ends here
