;;; package --- Summary

;;; Commentary:

;;; Code:

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

;; ------------------------------------------------------------------------------
;; ## Packages

(use-package command-log-mode)          ; To use:
					; M-x globalcommand-log-mode
					; M-x clm/toggle-command-log-buffer

;; ------------------------------------------------------------------------------
;; ## User interface

;; ### Clean up Emacs UI.

(setq inhibit-startup-message nil)  ; Hide startup screen.
(tool-bar-mode nil)                 ; Disable the toolbar.
(tooltip-mode nil)                  ; Disable tooltips.
(setq visible-bell t)               ; Turn on visible bell.

;; ### Line and column numbers

(column-number-mode)                ; Show column number in mode line.
(global-display-line-numbers-mode)  ; Always show line numbers.

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ### Cursor

(setq blink-cursor-mode nil)  ; Don't blink cursor.
(setq cursor-type 'bar)       ; Use bar cursor.

;; ### Mode line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ## Programming

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Install company for autocompletion popup.
(use-package company
  :hook (prog-mode . company-mode))

;; Install flycheck for real-time syntax checking (squiggly line errors).
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ## Help

;; Show key-binding completion suggestions in a new buffer.
(use-package which-key
  :init (which-key-mode))

(provide 'init)
;;; init.el

