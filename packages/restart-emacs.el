;;; restart-emacs.el --- because I'm lazy
;;; Commentary:
;;; Code

;; restart emacs in emacs
(defun launch-separate-emacs-in-terminal ()
  "Helper for restart-emacs."
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  "Helper for restart-emacs."
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  "Restart Emacs within Emacs."
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(provide 'restart-emacs)
;;; restart-emacs.el ends here
