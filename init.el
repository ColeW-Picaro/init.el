;;; Init.el --- my emacs config
;;; Commentary:
;;; The editor of the gods
;;; Code:

;; Increase the garbage collection threshold to 100MB for a faster startup time.
(setq-default gc-cons-threshold 100000000
	      gc-cons-percentage 0.6)
;; Restore it to 8MB after initialization is finished.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 8000000
					       gc-cons-percentage 0.1)))
;; Collect all garbage whenever Emacs loses focus.
(add-hook 'focus-out-hook #'garbage-collect)

;; Initialize the package management system (only at compile time).
(eval-when-compile
  ;; Require the package manager.
  (require 'package)
  ;; Enable the MELPA repository.
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  ;; Initialize the package manager.
  (package-initialize)
  (package-refresh-contents)
  ;; Check for use-package. Install it if not already present.
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Memes
(setq initial-scratch-message ";; Thank you rms, very cool!")
(defun shrug ()
  "Header def for shrug ¯\_(ツ)_/¯."
  )
;; Startup things
(setq-default
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 )

;; Editor Preferences
(setq-default
 scroll-step 1
 c-default-style "google"
 )

;; Function definitions
(defun enable-modes ()
  "All the modes."
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (global-hl-line-mode 1)
  (show-paren-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (global-prettify-symbols-mode 1)
  )
(enable-modes)

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun switch-to-minibuffer-window ()
  "Switch to the minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; Init file
(defun get-latest-init ()
  "Get my latest init file."
  (message "Downloading init file")
  (url-copy-file "https://raw.githubusercontent.com/ColeW-Picaro/init.el/master/init.el" (concat user-emacs-directory "init.el"))
  (byte-compile-init-file)
  )

(defun byte-compile-init-file ()
  "Byte compile the init file."
  (interactive)
  (save-restriction
    (message "Byte-compiling init file...")
    (byte-compile-file (concat user-emacs-directory "init.el"))))

;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))

;; Disabled *Messages*
(setq-default message-log-max nil)
(kill-buffer "*Messages*")



;; package requires
(require 'bind-key)

;; File loads
(load "cpp-hpp.el")
(load "shrug.el")

;; use-package decl

;; for ssh
(use-package tramp)

;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode "syntax"
  :config
  (use-package flycheck-rust
    :ensure t
    :config
    (flycheck-rust-setup))
  (global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode . (lambda () (setq flycheck-gcc-language-standard "c++17")))
  :bind
  ("C-c e" . flycheck-list-errors)
  )

;; Make this shit look good
(use-package cherry-blossom-theme
  :ensure t
  :defer)

(use-package gruvbox-theme
  :ensure t
  :defer)

(use-package moe-theme
  :ensure t
  :defer)

;; Journaling nerd
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Documents/journal/")
  :bind
  ("C-x C-j" . org-journal-new-entry)
  :hook
  (org-journal-mode . org-mode)
  )

;; Readability
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Idk I don't use this
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

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5))))

;; caml
(use-package caml
  :ensure t)

;; magit
(use-package magit
  :ensure t)

;; counsel
(use-package counsel
  :ensure t
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil))

;; swiper
(use-package swiper
  :ensure t
  :config
  (bind-key* "C-s" 'swiper))

;; ivy
(use-package ivy
  :ensure t
  :config (ivy-mode))

;; auto-complete
(use-package auto-complete
  :ensure t
  :diminish (auto-complete-mode)
  :config
  (ac-config-default)
  :defer)

;; google's style guidlines
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; rust
(use-package rust-mode
  :ensure t)

;; neotree!!
(use-package neotree
  :ensure t
  :bind
  (([f8] . neotree-toggle)
   ("C-c t" . neotree)))

;; mAKE This shit look good
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  :custom-face
  (mood-line-unimportant-face ((t (:foreground)))))
;; poweruser
(use-package multiple-cursors
  :ensure t)

;; MAKE THIS SHIT LOOK FUCKING GOOD
(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 40.0)
  (setq calendar-longitude -76.3)
  (setq circadian-themes
	'((:sunrise . moe-light)
	  (:sunset . gruvbox-dark-hard)))
  (circadian-setup))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ebdbb2" "#9d0006" "#79740e" "#b57614" "#076678" "#8f3f71" "#427b58" "#3c3836"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default)))
 '(package-selected-packages
   (quote
    (ssh emojify ac-emoji counsel-spotify web-mode web-beautify w3m use-package rainbow-mode rainbow-delimiters monokai-theme markdown-preview-eww markdown-mode magit gruvbox-theme glsl-mode eww-lnum dired-hacks-utils dashboard-hackernews counsel company caml auto-complete afternoon-theme)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mood-line-unimportant-face ((t (:foreground)))))

;; Key Bindings
(bind-keys*
 ("C-x C-b" . ibuffer)
 ("C-x p" . previous-multiframe-window)
 ( "C-c s" . shrug)
 ([f6] . visual-line-mode)
 ([f7] . display-line-numbers-mode)
 ("C-x RET" . eshell)
 ("M-n" . scroll-up-line)
 ("M-p" . scroll-down-line)
 ("C-c i" . windmove-up)
 ("C-c k" . windmove-down)
 ("C-c j" . windmove-left)
 ("C-c l" . windmove-right)
 ("C-c c" . calendar)
 ("C-c m" . switch-to-minibuffer-window)
 )


(provide 'init)
;;; init.el ends here
