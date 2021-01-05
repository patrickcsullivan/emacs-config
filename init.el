;; --------------------------------------------------------------------
;; ## Set up package management

(require 'package)     ; Bring in package functions.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh package contents on new system loads.
(unless package-archive-contents
  (package-refresh-contents))

;; Import use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package):
(setq use-package-always-ensure t)

;; --------------------------------------------------------------------
;; ## Packages

(use-package command-log-mode)          ; To use:
					; M-x globalcommand-log-mode
					; M-x clm/toggle-command-log-buffer

;; --------------------------------------------------------------------
;; ## User interface

;; Clean up Emacs UI.

(setq inhibit-startup-message nil)
(tool-bar-mode nil)  ; Disable the toolbar.
(tooltip-mode nil)   ; Disable tooltips.

(setq visible-bell t)               ; Turn on visible bell.
(global-display-line-numbers-mode)  ; Always show line numbers.

(setq blink-cursor-mode nil)  ; Don't blink cursor.
(setq cursor-type 'bar)       ; Use bar cursor.

;; Mode line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; --------------------------------------------------------------------
;; Buffer completion framework

;; Helm and Ivy are two main options

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work rig

 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" default)))
 '(idris-interpreter-path "idris2")
 '(package-selected-packages
   (quote
    (company idris-mode spacemacs-theme afternoon-theme dracula-theme)))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Always show line numbers.


