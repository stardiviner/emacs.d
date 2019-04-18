;;; init-leetcode.el --- init for LeetCode

;;; Time-stamp: <2019-04-19 07:18:23 stardiviner>

;;; Commentary:



;;; Code:

;;; [ leetcode.el ] -- An Emacs LeetCode client.

(use-package leetcode
  :quelpa (leetcode :fetcher github :repo "kaiwk/leetcode.el")
  :ensure request-deferred
  :defer t
  :commands (leetcode)
  :config (setq leetcode-account "stardiviner"
                leetcode-password (my/json-read-value my/account-file 'leetcode)
                leetcode-prefer-language "python3") ; `clojure' is not supported by LeetCode yet.
  )



(provide 'init-leetcode)

;;; init-leetcode.el ends here
