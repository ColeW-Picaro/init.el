;;; Code:
(package-initialize)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq initial-scratch-message ";; Thank you rms, very cool!")

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/")
  (require 'use-package))
  
;; For melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; backups directory (.backups)
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Default font
(set-default-font "Iosevka-14")

;; themes directory
(add-to-list 'load-path "~/.emacs.d/themes/")

;; load path
(add-to-list 'load-path "~/.emacs.d/packages/")

;; elpa load path
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; restart emacs in emacs
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Generate hpp and cpp files
(defun gen-cpp-file (name)
  "Generate a .cpp and .hpp file from filename"
  (interactive "sfile name?: ")
  (counsel-find-file (concat name ".cpp"))
  (counsel-find-file (concat name ".hpp")))

;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook 
	  '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))

;; Disabled *Messages*
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Hooks
(add-hook 'prog-mode-hook #'auto-complete-mode)

;; bind keys
(require 'bind-key)

;; Use packages
;; (use-package afternoon-theme)
(use-package gruvbox-theme
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.css\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (use-package web-beautify
    :ensure t))

(use-package rainbow-mode
  :ensure t
  :after web-mode
  :diminish rainbow-mode
  :init (add-hook 'web-mode-hook 'rainbow-mode))

(use-package company
  :ensure t
  :defer 2
  :diminish company-mode "complete"
  :config (global-company-mode)
  :bind ("C-\\" . company-complete-common))

;;(use-package ewal)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5))))

(use-package caml
  :ensure t)

(use-package magit
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (bind-key* "C-x C-f" 'counsel-find-file))

(use-package swiper
  :ensure t
  :config
  (bind-key* "C-s" 'swiper))

(use-package ivy
  :ensure t)

(use-package auto-complete
  :ensure t)

(use-package dashboard-hackernews
  :ensure t)

;;(use-package spotify
;;  :load-path "packages/spotify.el/"
;;  :config
;;  (setq spotify-oauth2-client-secret "")
;;  (setq spotify-oauth2-client-id "")
;;  (setq spotify-transport 'connect))

(use-package counsel-spotify
  :ensure t
  :config
  (bind-key "M-n" 'counsel-spotify-next)
  (bind-key "M-p" 'counsel-spotify-previous)
  (bind-key "M-s" 'counsel-spotify-toggle-play-pause)
  )

(use-package zone
  :ensure t
  :config
  (zone-when-idle 60))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
)

;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:background "#3c3836" :foreground "pale goldenrod"))))
 '(linum ((t (:background "#1d2021" :foreground "pale goldenrod"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))


;; global settings
(global-linum-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(auto-complete-mode)
(ivy-mode)
(show-paren-mode 1)
(electric-indent-mode 1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq scroll-step 1)

;; Key Bindings
(bind-key* "C-x C-b" 'ibuffer)
(bind-key* "C-x p" 'previous-multiframe-window)

