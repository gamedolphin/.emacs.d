;;; package --- Summary - My minimal Emacs init file

;;; Commentary:
;;; Simple Emacs setup I carry everywhere

(setq custom-file (concat user-emacs-directory "custom.el")) ;; keep this file pristine
(load custom-file 'noerror)

;;; Code:
(use-package emacs
  :ensure t
  :custom
  ;; ME!
  (user-full-name "Sandeep Nambiar")
  ;; memory configuration
  (gc-cons-threshold 10000000 "Higher garbage collection threshold, prevents frequent gc locks.")
  (byte-compile-warnings '(not obsolete) "Ignore warnings for (obsolete) elisp compilations.")
  (warning-suppress-log-types '((comp) (bytecomp)) "And other log types completely.")
  (large-file-warning-threshold 100000000 "Large files are okay in the new millenium.")
  (read-process-output-max (max 65536 read-process-output-max) "Read upto 64K (or max) based on system pipe capacity")
  ;; scrolling configuration
  (scroll-margin 0 "Lets scroll to the end of the margin.")
  (scroll-conservatively 100000 "Never recenter the window.")
  (scroll-preserve-screen-position 1 "Scrolling back and forth between the same points.")
  ;; frame configuration
  (frame-inhibit-implied-resize t "Improve emacs startup time by not resizing to adjust for custom settings.")
  (frame-resize-pixelwise t "Dont resize based on character height / width but to exact pixels.")
  ;; backups
  (backup-directory-alist '(("." . "~/.backups/")) "Don't clutter.")
  (backup-by-copying t "Don't clobber symlinks.")
  (create-lockfiles nil "Don't have temp files that trip the language servers.")
  (delete-old-versions t "Cleanup automatically.")
  (kept-new-versions 6 "Update every few times.")
  (kept-old-versions 2 "And cleanup even more.")
  (version-control t "Version them backups")
  (inhibit-startup-screen t "I have already done the tutorial. Twice.")
  (inhibit-startup-message t "I know I am ready")
  (inhibit-startup-echo-area-message t "Yep, still know it")
  (initial-scratch-message nil "I know it is the scratch buffer where I can write anything!")
  ;; packages
  (package-install-upgrade-built-in t)
  (use-package-always-ensure t)
  ;; tabs
  (tab-width 4 "Always tab 4 spaces.")
  (indent-tabs-mode nil "Never use actual tabs.")
  ;; desktop save path
  (desktop-path '("~/.emacs.d/.cache/"))

  :init
  (global-auto-revert-mode t)	;; revert automatically on external file changes
  (savehist-mode)		;; save minibuffer history
  ;; base visual
  (menu-bar-mode -1)			;; no menu bar
  (toggle-scroll-bar -1)		;; no scroll bar
  (tool-bar-mode -1)			;; no tool bar either
  (global-hl-line-mode +1)		;; always highlight current line
  (blink-cursor-mode -1)		;; stop blinking
  (global-display-line-numbers-mode 1)	;; always show line numbers
  (column-number-mode t)		;; column number in the mode line
  (size-indication-mode t)		;; file size in the mode line
  (pixel-scroll-precision-mode) ;; smooth mouse scroll
  (fset 'yes-or-no-p 'y-or-n-p) ;; dont ask me to type yes/no everytime, y/n is good enough
  (electric-pair-mode)          ;; i mean ... parens should auto create

  ;; UTF-8 EVERYWHERE
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  :bind
  (("C-<wheel-up>"   . nil)                  ; dont zoom in please
   ("C-<wheel-down>" . nil)                  ; dont zoom in either
   ("C-x k"          . kill-this-buffer))    ; kill the buffer, dont ask

  ;; :custom-face
  ;; set background to match nord colors, prevent white flash .
  ;; (default ((t (:foreground "#bbc2cf" :background "#2e3440"))))

  :hook
  ;; cleanup on save
  (before-save . whitespace-cleanup)

  :config
  (set-frame-font "Iosevka Semibold 12" nil t) ;; font of the century
  (desktop-save-mode)
  (desktop-read)
  )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package diminish :defer t)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package nerd-icons
  :defer t
  :custom
  (nerd-icons-color-icons nil))

(use-package doom-modeline
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-lsp nil)
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config))

(use-package pulsar
  :defer t
  :vc (:fetcher github
                :repo protesilaos/pulsar)
  :init
  (defface pulsar-nord
    '((default :extend t)
      (((class color) (min-colors 88) (background light))
       :background "#88c0d0")
      (((class color) (min-colors 88) (background dark))
       :background "#88c0d0")
      (t :inverse-video t))
    "Alternative magenta face for `pulsar-face'."
    :group 'pulsar-faces)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.05)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-nord)
  (pulsar-highlight-face 'pulsar-nord)
  :config
  (pulsar-global-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package expand-region
  :defer t
  :bind ("M-m" . er/expand-region))

(use-package puni
  :vc (:fetcher github
                :repo AmaiKinono/puni)
  :defer t
  :init
  (puni-global-mode))

(use-package avy
  :defer t
  :bind
  ("M-i" . avy-goto-char)
  :custom
  (avy-background t))

(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ag
  :defer t)

(use-package consult
  :defer t
  :bind (
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         )
  :custom
  (consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-'" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :defer t
  :init
  (vertico-mode)
  :config
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :defer t
  :init (marginalia-mode))

(use-package crux
  :defer t
  :bind
  ("C-c M-e" . crux-find-user-init-file)
  ("C-c C-w" . crux-transpose-windows)
  ("C-a" . crux-move-beginning-of-line))

(use-package magit
  :defer t
  :bind (("C-M-g" . magit-status)))

(use-package apheleia
  :defer t
  :config
  (add-to-list 'apheleia-formatters '(rustfmt . ("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021")))
  (apheleia-global-mode +1))

(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?_)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :defer t
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :defer t)

(use-package treesit-auto
  :defer t
  :config
  (global-treesit-auto-mode))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-globally-ignored-directories (append '("node_modules")))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package rust-ts-mode
  :defer t
  :custom
  (lsp-rust-analyzer-cargo-watch-command "check")
  (lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default)
  (lsp-eldoc-render-all t))

(use-package typescript-ts-mode
  :defer t
  :custom
  (lsp-javascript-preferences-import-module-specifier :relative)
  (typescript-indent-level 2)
  (typescript-ts-mode-indent-offset 2))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-lens-mode
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-csharp-server-path "/home/nambiar/dev/omnisharp-mono/run")
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :config
  (defun lsp-cleanup ()
    (lsp-format-buffer)
    (lsp-organize-imports t t))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  ((go-ts-mode
    rust-ts-mode
    csharp-ts-mode
    typescript-ts-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  (before-save . lsp-cleanup))

(use-package lsp-ui :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-eldoc-enable-hover nil)
  )

(provide 'init)
;;; init.el ends here
