;;; cpp-hpp.el --- generate cpp and hpp files
;;; Commentary:
;;; Code:

(defun gen-cpp-hpp (name)
  "Generate a cpp and hpp file from filename.  NAME is the name of a file."
  (interactive "sfile name?: ")
  (counsel-find-file (concat name ".cpp"))
  (counsel-find-file (concat name ".hpp")))

(provide 'cpp-hpp)
;;; cpp-hpp.el ends here
