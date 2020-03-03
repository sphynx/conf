;; Packages.
(require 'package)
(add-to-list 'package-archives
             (cons "melpa" "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/elisp") ;; for saving some custom code.

;; Save customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Use-package setup.
(setq use-package-always-ensure t) ;; to auto-download packages.
(require 'use-package)

;; $PATH
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; UI.
(use-package solarized-theme

  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)

  :config
  (load-theme 'solarized-light t))

(set-face-attribute 'default nil :family "Monaco" :height 130)
(setq-default line-spacing 2)
(setq resize-mini-windows nil)
(blink-cursor-mode   -1)
(column-number-mode   1)
(tool-bar-mode       -1)
(scroll-bar-mode     -1)
(menu-bar-mode       -1)

;; ignore visual bells
(setq ring-bell-function 'ignore)

;; Disable graphical pop-ups.
(defadvice yes-or-no-p (around prevent-dialog activate)
  (let ((use-dialog-box nil)) ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  (let ((use-dialog-box nil)) ad-do-it))
(defadvice message-box (around prevent-dialog activate)
  "Prevent message-box from activating a dialog"
  (apply #'message (ad-get-args 0)))

;; <ake all "yes or no" prompts show "y or n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load my custom functions.
(load "~/elisp/ds-functions.el")

;; General stuff.

;; Backups in a separate directory.
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Truncate lines in buffers by default but do not truncate
;; automatically splitted windows.
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

 ;; Do not use tabs while indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 ;; see what you type
 echo-keystrokes 0.01
 make-backup-files nil
 ;; insert space/slash after completion
 comint-completion-addsuffix t
 ;; delete line in one stage
 kill-whole-line t
 ;; text scrolling
 scroll-conservatively 50
 scroll-preserve-screen-position t
 scroll-margin 0
 ;; Scroll by one line at a time
 scroll-step 1
 ;; make sure file ends with NEWLINE
 require-final-newline t
 ;; delete excess backup versions
 delete-old-versions t
 ;; paste at cursor NOT at mouse pointer position
 mouse-yank-at-point t
 ;; calendar customizing
 system-time-locale "en"
 ;; apropos works better but slower
 apropos-do-all t
 ;; my sentences usually end with one space!
 sentence-end-double-space nil
 ;; for indenting comments as well
 comment-style 'indent
 ;; make dabbrev completions case sensitive
 dabbrev-case-fold-search nil
 mark-even-if-inactive t
 ;; do not ask for compilation cmd
 compilation-read-command nil
 ;; do not follow symlinks in version control
 vc-follow-symlinks nil
 create-lockfiles nil
 )

;; when I select something in another program to paste it into Emacs,
;; but kill something in Emacs before actually pasting it, don't lose
;; the selection; save it in the `kill-ring' before the Emacs kill so
;; that I can still paste it using C-y M-y.
(setq save-interprogram-paste-before-kill t)

;; show matching parens
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; highlights selected regions
(transient-mark-mode 1)

;; type to replace selected text (or press DEL)
(delete-selection-mode t)

;; store all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (concat user-emacs-directory "saveplace"))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)

;; for better experience with imenu
(set-default 'imenu-auto-rescan t)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; clean up whitespace in the buffer on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; winner-mode to save buffers' configs and redo/undo
(use-package winner :defer t)

;; spellchecking with aspell
(setq-default ispell-program-name "aspell"
              ispell-dictionary "ru-yo")

;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(defalias 'wrap 'visual-line-mode)

(require 'whitespace)

;; highlight trailing space and tabs
(setq whitespace-style '(face trailing))

;; start server for emacsclient usage
(server-start)

;; (remove-hook 'find-file-hook 'vc-find-file-hook)

;; browse-url tuning
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")

;; Dired.
(require 'dired-x)

(setq
 dired-omit-extensions '(".hi" ".o")
 dired-dwim-target t)

(defun ds-dired-hook ()
   (dired-omit-mode)
   ;; here we redefine keys in order to reuse dired buffers
   ;; because creating them for every intermediate directory
   ;; seems to be an overkill
   (define-key dired-mode-map (kbd "<return>")
     'dired-find-alternate-file) ; was dired-advertised-find-file
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
   (define-key dired-mode-map "e"
     'wdired-change-to-wdired-mode))

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook 'ds-dired-hook)

(use-package bind-key
  :init
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

   :config
   (require 'bind-key)
   (bind-keys
   ("M-g"        . goto-line)
   ("C-<tab>"    . other-window)
   ("M-`"        . other-window)
   ("C-z"        . undo)
   ("C-x C-1"    . delete-other-windows)
   ("C-x C-0"    . delete-window)
   ("C-x 4 t"    . ds-transpose-buffers)
   ("C-x 4 r"    . ds-toggle-window-split)
   ("M-<up>"     . ds-move-line-up)
   ("M-<down>"   . ds-move-line-down)
   ("C-M-<down>" . ds-duplicate-line-down)
   ("C-M-<up>"   . ds-duplicate-line-up)
   ("C-x a r"    . align-regexp)
   ("C-a"        . ds-cool-home-key)
   ("C-c r"      . revert-buffer)
   ("C-."        . dired-jump)
   ("C-c p"      . ds-copy-file-name-to-clipboard)
   ("C-w"        . ds-kill-region-if-mark-active)
   ("M-w"        . ds-kill-ring-save-if-mark-active)
   ("M-/"        . hippie-expand)
   ("C-x C-b"    . ibuffer)
   ("C-c C-q"    . join-line)
   ("H-SPC"      . set-rectangular-region-anchor)
   ("M-*"        . pop-tag-mark)
   ("C-x p"      . pop-to-mark-command)))

(define-key 'help-command "a" 'apropos)
(define-key 'help-command "s" 'info-lookup-symbol)

;; windmove-switching
(windmove-default-keybindings) ;; Shift + direction

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-/"))

;; Recent files.
(recentf-mode 1)
(setq recentf-max-menu-items 2000
      recentf-max-saved-items 2000)

;; Hippie expand.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Search.
;; case INsensitive search
(setq-default case-fold-search t)

(defun ds-search-hook ()
  ;; it works a little better if isearch puts you
  ;; at the start of the search, not the end
  (when isearch-forward (goto-char isearch-other-end)))

;; Activate `occur' easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote
                                                isearch-string))))))
(add-hook 'isearch-mode-end-hook 'ds-search-hook)

;; Ediff.
;; Do everything in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split windows horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

(setq default-input-method "russian-computer")
(prefer-coding-system 'utf-8-unix)

;; Haskell.
(use-package haskell-mode
  :init
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-compile-cabal-build-command "stack build")

  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-compile)
              ("M-n"     . next-error)
              ("M-p"     . previous-error)

              ("C-c C-l" . haskell-process-load-file)
              ("C-`"     . haskell-interactive-bring)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-k" . haskell-interactive-mode-clear)
              )


  :config
  (subword-mode 1)
  (whitespace-mode 1)
  ;;  (eldoc-mode 1)
  ;;  (turn-on-haskell-indentation)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)

  ;; :bind (:map interactive-haskell-mode-map
  ;;             ("C-c C-c" . haskell-compile)
  ;;             ("C-c C-l" . haskell-process-load-or-reload)
  ;;             )


  ;;  (setq haskell-process-type 'stack-ghci)
  ;;  (interactive-haskell-mode 1)

  ;; :hook
  ;; (haskell-mode interactive-haskell-mode)
  )

;; Clojure.
(use-package cider)

(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

;; Show time.
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode 1)

(use-package smart-mode-line
  :config
  (sml/setup))

(setq set-mark-command-repeat-pop t)

(use-package projectile
  :config
  (projectile-mode))

;; Smex.
(use-package smex
  :bind ("M-x" . smex)
  :config
  (smex-initialize))

;; Ido.
(use-package ido
  :bind (("C-," . ido-switch-buffer)
         ("C-x C-a" . ds-ido-imenu))
  :init
  (setq ido-enable-flex-matching t
        ido-show-dot-for-dired t ;; for easy access to dired
        ido-ignore-extensions t
        ido-save-directory-list-file (concat user-emacs-directory "ido.last"))
  :config
  (ido-mode 1)
  (ido-everywhere 1))

;; (ivy-mode 1)

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package flx-ido
  :init
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode 1))

;; Multiple coursors.
(use-package multiple-cursors
  :bind (("H-e" . mc/edit-lines)
         ("H-n" . mc/mark-next-like-this)
         ("H-p" . mc/mark-previous-like-this)
         ("H-a" . mc/mark-all-like-this)))


;; Markdown.
(use-package markdown-mode
  :mode ("\\.md" . markdown-mode)
  :init
  (setq markdown-command "pandoc -f markdown+fenced_code_blocks -t html+fenced_code_blocks"))

(use-package markdown-preview-mode
  :init
  (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
  )

(use-package flycheck
  :hook (rust-mode . flycheck-mode))

;; Rust (and related stuff)
(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1))

(use-package racer
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "/Users/sphynx/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src/")
  :hook ((racer-mode . eldoc-mode)
         (racer-mode . company-mode)))

(use-package rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :hook (rust-mode . racer-mode)
  :bind (("C-c C-c" . compile)
         ("C-c d" . ds-search-rust-docs)
         ("C-c C-d" . ds-search-rust-docs)
         ))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package flycheck-rust
;;   :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode)

;;;;;;;;;;;;;;;;;;;;;;

(defun my-java-mode-hook ()
  (define-key java-mode-map (kbd "C-c C-c") 'compile))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Python.
(use-package python
  :config
  (require 'python)
  (defun python-mode-hook ()))

;; Org-mode
(use-package org
  :mode ("\\.org$" . org-mode))
(unbind-key "C-," org-mode-map)

;; YaSnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "<S-tab>") #'yas-expand)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; Elisp
;; use `M-x ielm` for evaluator
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(setq source-directory "~/Code/emacs")

;; C
(setq c-default-style "linux"
      c-basic-offset 4)
(add-hook 'c-mode-hook #'yas-minor-mode)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code/c" "~/Code/openssl"))
  )


;; Bookmarks + IDO
(require 'bookmark)

(defun my-ido-bookmark-jump ()
  "Jump to bookmark using ido"
  (interactive)
  (let ((dir (my-ido-get-bookmark-dir)))
    (when dir
      (find-alternate-file dir))))

(defun my-ido-get-bookmark-dir ()
  "Get the directory of bookmark"
  (let* ((name (ido-completing-read "Use dir of bookmark: " (bookmark-all-names) nil t))
         (bmk (bookmark-get-bookmark name)))
    (when bmk
      (setq bookmark-alist (delete bmk bookmark-alist))
      (push bmk bookmark-alist)
      (let ((filename (bookmark-get-filename bmk)))
        (if (file-directory-p filename)
            filename
          (file-name-directory filename))))))

(defun my-ido-dired-mode-hook ()
  (define-key dired-mode-map "$" 'my-ido-bookmark-jump))

(add-hook 'dired-mode-hook 'my-ido-dired-mode-hook)

(defun my-ido-use-bookmark-dir ()
  "Use bookmark dir"
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (dir (my-ido-get-bookmark-dir)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun my-ido-set-keys-hook ()
  "Set a key for using bookmarks in ido"
  (define-key ido-completion-map (kbd "$") 'my-ido-use-bookmark-dir))

(add-hook 'ido-setup-hook 'my-ido-set-keys-hook)

(setq bookmark-save-flag 1)

(setq-default bidi-display-reordering nil)
