;;; init-leetcode.el --- init for LeetCode

;;; Time-stamp: <2019-08-09 08:43:13 stardiviner>

;;; Commentary:



;;; Code:

;;; [ leetcode.el ] -- An Emacs LeetCode client.

(leaf leetcode
  :package t
  ;; :el-get (leetcode :url "https://github.com/kaiwk/leetcode.el.git")
  :ensure request-deferred
  :commands (leetcode)
  :config (setq leetcode-account "stardiviner"
                leetcode-password (my/json-read-value my/account-file 'leetcode)
                leetcode-prefer-language "python3") ; `clojure' is not supported by LeetCode yet.
  )



(provide 'init-leetcode)

;;; init-leetcode.el ends here
