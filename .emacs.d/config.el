(add-to-list 'load-path "~/.emacs.d/nano-bindings.el")
(add-to-list 'load-path "~/.emacs.d/nano-modeline.el")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(dolist (mode '(vterm-mode-hook
		dashboard-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Visual bell
(setq visible-bell t)

;;(global-set-key (kbd "C-=") 'text-scale-increase)
;;(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil :font "Source Code Pro" :height 100)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
	     '("org" . "https://orgmode.org/elpa/"))
(package-refresh-contents)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor)

(use-package general
  :config
  (general-evil-setup t))

;; General keybindings
(nvmap :keymaps 'override :prefix "SPC"
  "SPC" '(counsel-M-x :which-key "M-x")
  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload Emacs"))

(use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil))

(setq load-prefer-newer noninteractive)
(setq gc-cons-threshold (* 2 1000 1000))

(use-package doom-themes)
(setq doom-themes-enable-italic t
      doom-themes-enable-bold t)

(use-package nano-theme)

;;(load-theme 'doom-one t)
(load-theme 'nano-dark t)
(nano-mode)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t)

(require 'doom-modeline)
;;(doom-modeline-mode 1)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 15)
(setq-default doom-modeline-height 15)

(setq doom-modeline-bar-width 4)
(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-project-detection 'auto)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count t)
;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)
;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)
;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)
;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)
(setq doom-modeline-modal-icon t)
;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-rust t)
;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-rust-executable "rustc")
;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")
;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(use-package elec-pair
  :ensure t)
(electric-pair-mode 1)

(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(setq ivy-initial-inputs-alist nil)

(use-package smex
    :ensure t)
(smex-initialize)

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
    '((swiper                     . ivy-posframe-display-at-point)
      (complete-symbol            . ivy-posframe-display-at-point)
      (counsel-M-x                . ivy-display-function-fallback)
      (counsel-esh-history        . ivy-posframe-display-at-window-center)
      (counsel-describe-function  . ivy-display-function-fallback)
      (counsel-describe-variable  . ivy-display-function-fallback)
      (counsel-find-file          . ivy-display-function-fallback)
      (counsel-recentf            . ivy-display-function-fallback)
      (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
      (nil                        . ivy-posframe-display))
    ivy-posframe-height-alist
    '((swiper . 20)
      (t . 10)))
  :config
  (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

(use-package company
  :init (company-mode))

(add-hook 'after-init-hook 'global-company-mode)

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Stop procrastinating")
  (setq dashboard-startup-banner 'official)
  ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((bookmarks . 3)
                          (agenda . 3)
                          (projects . 3)))

  :config
  (dashboard-setup-startup-hook))

;; Sets inital buffer when opening Emacs to *dashboard*
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(nvmap :prefix "SPC"
  "b i" '(ibuffer :which-key "Ibuffer")
  "b k" '(kill-current-buffer :which-key "Kill current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b K" '(kill-buffer :which-key "Kill buffer"))

(delete-selection-mode t)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package recentf
  :config
  (recentf-mode))
(use-package sudo-edit)

(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Which key
(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)

(use-package writeroom-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/Org/"
      org-agenda-files '("~/Org/agenda.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis "▼"
      org-log-done 'time
      org-journal-dir "~/Org/journal/"
      org-journal-date-format "%B %d, %Y (%A)"
      org-journal-file-format "%d-%m-%Y.org"
      org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-src-tabs-acts-natively t
      org-edit-src-content-indentation 0)
(setq org-support-shift-select 'always)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROJ(p)"           ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))  ; Task has been cancelled

(use-package org-tempo
  :ensure nil)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate t
    org-edit-src-content-indentation 0)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-blank-before-new-entry (quote ((heading . nil)
                                         (plain-list-item . nil))))

(use-package ox-man
    :ensure nil)

(use-package haskell-mode)

(nvmap :keymaps 'override :prefix "SPC"
	   "H i n" '(haskell-navigate-imports :which-key "Navigate to imports")
	   "H i s" '(haskell-sort-imports :which-key "Sort Imports")
	   "H i a" '(haskell-align-imports :which-key "Align imports"))

(use-package markdown-mode)

(use-package rust-mode)
(nvmap :keymaps 'override :prefix "SPC"
  "R r" '(rust-run :which-key "Rust run"))

(use-package elpy
  :ensure t
  :init (elpy-enable))

(use-package yaml
    :ensure t)
(use-package yaml-mode
    :ensure t)
;;(yaml-mode)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-SPC")
  :hook (
	 (python-mode . lsp)
	 (haskell-mode . lsp)
	 (rust-mode . lsp)
	 (go-mode . lsp)
	 (lsp-mode . enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))

(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
					 (lsp-format-buffer))))

(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-deferred)

(defun org-icons ()
   "Beautify org mode keywords."
   (setq prettify-symbols-alist '(("TODO" . "")
	                          ("WAIT" . "")
   				  ("NOPE" . "")
				  ("DONE" . "")
				  ("[#A]" . "")
				  ("[#B]" . "")
 				  ("[#C]" . "")
				  ("[ ]" . "")
				  ("[X]" . "")
				  ("[-]" . "")
				  ("#+BEGIN_SRC" . "")
				  ("#+END_SRC" . "―")
				  (":PROPERTIES:" . "")
				  (":END:" . "―")
				  ("#+STARTUP:" . "")
				  ("#+TITLE: " . "")
				  ("#+RESULTS:" . "")
				  ("#+NAME:" . "")
				  ("#+ROAM_TAGS:" . "")
				  ("#+FILETAGS:" . "")
				  ("#+HTML_HEAD:" . "")
				  ("#+SUBTITLE:" . "")
				  ("#+AUTHOR:" . "")
				  (":Effort:" . "")
				  ("SCHEDULED:" . "")
				  ("DEADLINE:" . "")
				  ("lambda" . "λ")))
   (prettify-symbols-mode))

(add-hook 'org-mode-hook 'org-icons)

(use-package rainbow-mode)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1)

(use-package flycheck)
(global-flycheck-mode)

(nvmap :prefix "SPC"
  "e h" '(counsel-esh-history :which-key "Eshell history")
  "e s" '(eshell :which-key "Eshell"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm)
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)
