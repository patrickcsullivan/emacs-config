(setq inhibit-startup-message t)

; Basic UI
(tool-bar-mode -1)        ; Enable toolbar.
(tooltip-mode 1)          ; Enable tooltips.
(menu-bar-mode 1)         ; Enable menu bar.
(set-fringe-mode 10)      ; Give some breathing room.
(setq visible-bell t)     ; Use visiual bell instead of audio.
(global-hl-line-mode)     ; Highlight current line.
(delete-selection-mode 1) ; Typing deletes selected text.
(show-paren-mode 1)       ; Highlight matching paren on hover.
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Show line numbers in programming modes.

; Font, theme
(set-face-attribute 'default nil :height 150)
(load-theme 'tango-dark)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs-bak")))

;; ----------

;; PACKAGE INIT

;; Initialize package sources.
(require 'package)
;(add-to-list 'package-archives `("melpa" . "https://melp.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; Refresh package contents on fresh system.
(unless package-archive-contents
  (package-refresh-contents))

; Initialize use-package on non-Linux platforms.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ----------

;; THEME, DASHBOARD

;; Install icons on system.
;; Need to run `M-x all-the-icons-install fonts` once on fresh system.
(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 5)
			    (projects . 5)))
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-footer nil))
  :config
  (dashboard-setup-startup-hook))

;; ----------

;; LAYOUT

(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'left
	centaur-tabs-height 24
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "∙")
  (centaur-tabs-mode t))

;; ----------

;; IDE

;; TODO
;; - Helm: completion window
;; - Flycheck: syntax checking

;; Run using `M-x global-commnad-log-mode`
;; followed by `M-x clm/toggle-command-log-buffer`.
(use-package command-log-mode)

;; Command completion window
;; Double press `Tab` after partial command.
;; Use either ido or helm but not both.
(use-package helm
  :ensure t
  :config (helm-mode 1))

;; Command completion minibuffer
;; Use either ido or helm but not both.
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (ido-mode t)

;; Project management
;; C-x p ... for projectile commands
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

;; Sidebar navigation with extras
;; F8 to toggle nav tree
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs))
  :config
  (progn
    (setq treemacs-is-never-other-window t))
  (treemacs-filewatch-mode t))

;; Code completion
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)) ; TODO: Decrease wait time

;; Intelligent errors (squiggly lines)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Smart text selection
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; ----------

;; ELIXIR

(use-package elixir-mode
  :ensure t)

;; ----------

;; AGDA

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Auto-load agda-mode for .agda and .lagda.md.
;; Emacs uses a Markdown mode for .md by default so we must override.
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; ----------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (centaur-tabs expand-region dashboard helm projectile doom-themes elixir-mode treemacs-magit treemacs-projectile use-package treemacs rainbow-delimiters magit flycheck doom-modeline company command-log-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

