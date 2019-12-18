;;; init-leetcode.el --- init for LeetCode

;;; Time-stamp: <2019-12-18 22:47:00 stardiviner>

;;; Commentary:



;;; Code:

;;; [ leetcode.el ] -- An Emacs LeetCode client.

(leaf leetcode
  :package t
  ;; :el-get (leetcode :url "https://github.com/kaiwk/leetcode.el.git")
  :ensure request-deferred
  :commands (leetcode)
  :config
  (setq leetcode-account "stardiviner"
        leetcode-password (my/json-read-value my/account-file 'leetcode)
        ;; `clojure' is not supported by LeetCode yet.
        leetcode-prefer-language "python3"))



(provide 'init-leetcode)

;;; init-leetcode.el ends here
