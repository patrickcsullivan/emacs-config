;; ---------------------------------------------------------

;; ------ Basic Emacs commands

;; C-x C-f
;; Find and load file.

;; C-x C-s
;; Save.

;; C-x k
;; Kill buffer.

;; C-x 4 f
;; Find file in a new split window.

;; ------ Buffer

;; C-x b
;; Change buffer.

;; C-x 4 b
;; Change buffer in a new split window.

;; ------ Movement

;; C-f / C-b
;; Forward / backward one character.

;; M-f / M-b
;; Forward / backward one word.

;; C-n / C-p
;; Next / previous line.

;; C-a / C-e
;; Beginning / end of current line.

;; M-x goto-line <line>
;; Go to line <line>.

;; ------ Editing

;; C-x u
;; Undo.

;; C-x C-;
;; Comment.

;; ------ Commands

;; C-g
;; Abort command.

;; C-h <type> <target>
;; Describe <target>.
;; Eg: C-h f load-theme

;; ------ Shell

;; M-x shell
;; Shell in new buffer.

;; ------ Executing

;; M-x eval-buffer
;; Eval buffer.

;; C-x C-e
;; Eval last s-expression.

;; C-h v <var>
;; Show the value of <var>.

;; Shift-M-:
;; Enter an s-expression to evaluate.

;; ---------------------------------------------------------

;; Global keybindings.

;; Make ESC abort command like C-g.
;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ---------------------------------------------------------

;; Initialize package sources.

;; M-x list-packages runs the built-in package manager. Shows packages available in ELPA.
;; use-package is another package manager that uses MELPA.

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-pacakge on non-Linux platforms.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------------------------------------

;; Clean up Emacs UI, making it more minimal.

(setq inhibit-startup-message t)

;; (scroll-bar-mode -1) ; Disable visible scrollbar.
;; (tool-bar-mode -1) ; Disable the toolbar.
;; (tooltip-mode -1) ; Disable the tooltips.
;; (set-fringe-mode 10) ; Give some breathing room.
;; (menu-bar-mode -1) ; Disable the menu bar.
;; (setq visible-bell t) ; Turn on visual (instead of audio) bell.

(tool-bar-mode -1)              ; Disable toolbar.
(tooltip-mode 1)                ; Enable tooltips.
(menu-bar-mode 1)               ; Enable menu bar.
(set-fringe-mode 10)            ; Give some breathing room.
(setq-default cursor-type 'bar) ; Enable bar cursor.
(setq visible-bell t)           ; Use visiual bell instead of audio.
(global-hl-line-mode)           ; Highlight current line.
(delete-selection-mode 1)       ; Typing deletes selected text.
(show-paren-mode 1)             ; Highlight matching paren on hover.

;; Set up Doom mode line.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(column-number-mode) ; Show column number in mode line.
(global-display-line-numbers-mode t) ; Show line numbers.

;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Scroll one line at a time (less "jumpy" than defaults).
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time.
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling.
(setq mouse-wheel-follow-mouse 't) ;; Scroll window under mouse.
(setq scroll-step 1) ;; Keyboard scroll one line at a time.

;; ---------------------------------------------------------

;; General help and productivity.

;; Autocompletion using Ivy.

;; This will turn on autocompletion for commands such as M-x and possibly other things.
;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;; 	 :map ivy-minibuffer-map
;; 	 ("TAB" . ivy-alt-done)
;; 	 ("C-l" . ivy-alt-done)
;; 	 ("C-j" . ivy-next-line)
;; 	 ("C-k" . ivy-previous-line)
;; 	 :map ivy-switch-buffer-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-l" . ivy-alt-done)
;; 	 ("C-d" . ivy-switch-buffer-kill)
;; 	 :map ivy-reverse-i-search-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))

;; Show pop-up when you start key binding to show what keys are available.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  ; Show available bindings immediately.
  (setq which-key-idle-delay 0))

;; ---------------------------------------------------------

;; Demoing.

;; Command log mode shows command history in a buffer.
;; Run M-x global-command-log-mode to enable it for every buffer.
;; Then run M-x clm/toggle-command-log-buffer
(use-package command-log-mode)

;; ---------------------------------------------------------

;; Theme.

;(load-theme 'wombat)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-palenight t))

(set-face-attribute 'default nil :height 140)

;; ---------------------------------------------------------

;; Global programming mode settings.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ---------------------------------------------------------

;; Agda.

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Auto-load agda-mode for .agda and .lagda.md.
;; Emacs uses a Markdown mode for .md by default so we must override.
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; ---------------------------------------------------------

;; Text editing.

;; Word-wrap in text mode but not in programming modes.
(add-hook 'text-mode-hook '(lambda ()
    (setq truncate-lines nil
          word-wrap t)))
(add-hook 'prog-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))

