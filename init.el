(setq inhibit-startup-message t)

; Basic UI
(tool-bar-mode 1)     ; Enable toolbar.
(tooltip-mode 1)      ; Enable tooltips.
(menu-bar-mode 1)     ; Enable menu bar.
(set-fringe-mode 10)  ; Give some breathing room.
(setq visible-bell t) ; Use visiual bell instead of audio.

; Font, theme
(set-face-attribute 'default nil :height 150)
(load-theme 'tango-dark)

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

;; THEME

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; ----------

;; IDE

;; TODO
;; - Company: buffer completion
;; - Projectile: projects
;; - Theme
;; - Flycheck: syntax checking

;; Run using `M-x global-commnad-log-mode`
;; Then `M-x clm/toggle-command-log-buffer`
(use-package command-log-mode)

; Sidebar navigation with extras
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs))
  :config
  (progn
    (setq treemacs-is-never-other-window t))
  (treemacs-filewatch-mode t))

; ----------

(use-package elixir-mode
  :ensure t)

; ----------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes elixir-mode treemacs-magit treemacs-projectile use-package treemacs rainbow-delimiters magit flycheck doom-modeline company command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
