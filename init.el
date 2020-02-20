;;; init.el --- my emacs config
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;; Memes
(setq initial-scratch-message ";; Thank you rms, very cool!")

;; Startup things
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; GUI Preferences
(global-linum-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-step 1)

;; Editor Preferences
(show-paren-mode 1)
(electric-indent-mode 1)

;; set transparency
;;(set-frame-parameter (selected-frame) 'alpha '(95 95))
;;(add-to-list 'default-frame-alist '(alpha 95 95))

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
(load "restart-emacs.el")

;; use-package decl

(use-package tramp)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode "syntax"
  :config
  (global-flycheck-mode))

;; (use-package afternoon-theme)
(use-package gruvbox-theme
  :ensure t)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/Documents/journal/")
  (add-hook 'org-journal-mode-hook 'emojify-mode)
  )

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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2)
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
  :ensure t
  :config (ivy-mode))

(use-package auto-complete
  :ensure t
  :diminish (auto-complete-mode))

(use-package dashboard-hackernews
  :ensure t)

(use-package emojify
  :ensure t)

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

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
(bind-key* "C-x C-b" 'ibuffer)
(bind-key* "C-x p" 'previous-multiframe-window)
(bind-key* "C-c C-s" 'shrug)

(provide 'init)
;;; init.el ends here
