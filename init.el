;;; Init.el --- my emacs config
;;; Commentary:
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

(defun enable-modes ()
  "All the modes."
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (global-hl-line-mode 1)
  (show-paren-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  )

(enable-modes)

;; Default font
(set-frame-font "Iosevka-14")

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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
(use-package gruvbox-theme
  :ensure t)

;; Journaling nerd
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Documents/journal/")
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

;; Auto complete
(use-package company
  :ensure t
  :defer 2
  :diminish company-mode "complete"
  :config (global-company-mode)
  :bind ("C-\\" . company-complete-common))

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
  (bind-key* "C-x C-f" 'counsel-find-file))

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
  :diminish (auto-complete-mode))

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


(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode)
  (setq doom-modeline-major-mode-color-icon t))

(use-package multiple-cursors
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(package-selected-packages
   (quote
    (ssh emojify ac-emoji counsel-spotify web-mode web-beautify w3m use-package rainbow-mode rainbow-delimiters monokai-theme markdown-preview-eww markdown-mode magit gruvbox-theme glsl-mode eww-lnum dired-hacks-utils dashboard-hackernews counsel company caml auto-complete afternoon-theme))))

;; Custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:background "#3c3836" :foreground "pale goldenrod"))))
 '(linum ((t (:background "#1d2021" :foreground "pale goldenrod")))))

;; Key Bindings
(bind-keys* ("C-x C-b" . ibuffer)
            ("C-x p" . previous-multiframe-window)
            ( "C-c C-s" . shrug)
            ([f6] . visual-line-mode)
            ([f7] . display-line-numbers-mode)
            ("C-x RET" . eshell)
            ("M-n" . scroll-up-line)
	    ("M-p" . scroll-down-line)
	    ("C-c C-i" . windmove-up)
	    ("C-c C-k" . windmove-down)
	    ("C-c C-j" . windmove-left)
	    ("C-c C-l" . windmove-right)
	    )


(provide 'init)
;;; init.el ends here
