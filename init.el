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

;; Initialize package sources.
(require 'package)
(setq package-archives `(("melpa" . "https://melp.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
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

; Run using `M-x global-commnad-log-mode`
; Then `M-x clm/toggle-command-log-buffer`
(use-package command-log-mode)  

