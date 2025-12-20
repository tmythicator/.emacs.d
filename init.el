;; ── Bootstrap ────────────────────────────────────────────────────────────
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(put 'use-package 'lisp-indent-function 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-verbose nil
      use-package-enable-imenu-support t)

;; macOS modifiers
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier nil
        mac-right-command-modifier nil
        mac-command-modifier 'meta))

(setq custom-file (if (eq system-type 'windows-nt)
                      (locate-user-emacs-file "NUL")
                    null-device))

(use-package system-packages :custom (system-packages-noconfirm t))
(use-package quelpa :defer t :custom (quelpa-update-melpa-p nil))
(use-package quelpa-use-package)

;; ── Basic UX ─────────────────────────────────────────────────────────────
(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (delete-selection-mode 1)
  :hook (prog-mode . display-line-numbers-mode)
  :bind (:map global-map
              ("C-c <tab>" . indent-buffer)
              ("M-i"       . imenu)
              ("M-p"       . move-line-up)
              ("M-n"       . move-line-down)
              ("M-'"       . duplicate-line))
  :custom
  (column-number-mode t)
  (bidi-inhibit-bpa t)
  (default-input-method "german")
  (scroll-step 1)
  (inhibit-startup-screen t)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (sentence-end-double-space nil)
  (mode-require-final-newline nil)
  (require-final-newline nil)
  (native-comp-async-report-warnings-errors 'silent)
  :config
  ;; Tree-sitter modes
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))

  (add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'"  . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))

  ;; Utils
  (defun indent-buffer () (interactive) (indent-region (point-min) (point-max)))
  (defun move-line-up ()   (interactive) (transpose-lines 1) (forward-line -2) (indent-according-to-mode))
  (defun move-line-down () (interactive) (forward-line 1) (transpose-lines 1) (forward-line -1) (indent-according-to-mode))
  (defun duplicate-line () (interactive)
         (let ((col (current-column))) (move-beginning-of-line 1) (kill-line) (yank) (newline) (yank) (move-to-column col)))

  ;; Frame alpha
  (add-to-list 'default-frame-alist '(alpha . (95 . 75)))
  (set-frame-parameter nil 'alpha '(95 . 75)))

(use-package so-long :ensure nil :config (global-so-long-mode 1) :custom (so-long-threshold 4000))

;; Editing niceties
(use-package autorevert :ensure nil :init (global-auto-revert-mode 1) :custom (auto-revert-verbose nil))
(use-package no-littering
  :config
  (use-package recentf
    :ensure nil
    :init (recentf-mode 1)
    :custom (recentf-auto-cleanup 'never)
    :config (run-with-idle-timer 60 t #'recentf-save-list))
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/")))
        make-backup-files t
        create-lockfiles nil))

(use-package files
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package frame :ensure nil :bind ("C-z" . nil))

;; ── UI: Theme/Modeline/Icons/Tabs ───────────────────────────────────────
(use-package doom-themes
  :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline :hook (after-init . doom-modeline-mode))

;; Icons
(use-package all-the-icons :if (display-graphic-p))

(use-package tool-bar :ensure nil :config (tool-bar-mode -1))
(use-package scroll-bar :ensure nil :config (scroll-bar-mode -1))
(use-package menu-bar :ensure nil :config (menu-bar-mode -1) :bind ([S-f10] . menu-bar-mode))
(use-package tooltip :ensure nil :custom (tooltip-mode -1))
(use-package paren :ensure nil :config (show-paren-mode 1))
(use-package hl-line  :ensure nil :hook (prog-mode . hl-line-mode))
(use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
(use-package highlight-escape-sequences :config (hes-mode))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))

;; ── Completion stack
(use-package vertico
  :init (vertico-mode 1)
  :custom (vertico-resize t) (vertico-cycle t))

(use-package savehist :ensure nil :init (savehist-mode 1))
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind (("C-z"     . consult-line)
         ("C-s"     . consult-line)
         ("C-S-s"   . consult-ripgrep)
         ("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c k"   . consult-keep-lines)
         ("C-c m"   . consult-mode-command)
         ("C-c i"   . consult-imenu)
         ("C-c !"   . consult-flymake))
  :custom
  (register-preview-delay 0.3)
  (register-preview-function #'consult-register-format))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult :after (embark consult))
(use-package consult-lsp :after (consult lsp-mode))

;; ── Popup completion via CAPF: corfu (+ cape)
(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; ── Project / Git / Grep ────────────────────────────────────────────────
(use-package projectile
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  (projectile-auto-discover nil)
  (projectile-track-known-projects-automatically nil)
  (projectile-completion-system 'default))

(use-package magit :bind ("C-x g" . magit-status))
(use-package git-gutter-fringe :diminish git-gutter-mode :config (global-git-gutter-mode 1))
(use-package git-timemachine :defer t)
(use-package rg :config (rg-enable-default-bindings))

;; ── Dired / Treemacs ────────────────────────────────────────────────────
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ([mouse-2] . dired-mouse-find-file)
              ([mouse-3] . dired-mouse-find-file-other-window)
              ("," . dired-hide-details-mode))
  :hook (dired-mode . dired-hide-details-mode))
(use-package dired-x :ensure nil)
(use-package dired-git-info :bind (:map dired-mode-map (")" . dired-git-info-mode)))
(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode))
  :hook (dired-mode . dired-hide-dotfiles-mode))
(use-package async :commands (dired-async-mode)
             :init (with-eval-after-load 'dired (dired-async-mode 1)))
(use-package dired-rsync :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package treemacs
  :bind (:map global-map
              ("C-x C-n" . treemacs)
              ("C-x t b" . treemacs-bookmark)))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-mode))
(use-package treemacs-magit :after (treemacs magit))

;; ── Shell / Eshell ──────────────────────────────────────────────────────
(use-package exec-path-from-shell
  :if (memq system-type '(gnu/linux darwin))
  :init (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables '("PATH" "SHELL"))
  (exec-path-from-shell-arguments '("-l")))

(use-package em-term :ensure nil
             :custom (eshell-destroy-buffer-when-process-dies t)
             :config (setq eshell-visual-commands `(,@eshell-visual-commands "pip" "pipenv")))
(use-package esh-autosuggest :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-fringe-status :hook (eshell-mode . eshell-fringe-status-mode))
(use-package eshell-prompt-extras
  :after esh-opt
  :custom (eshell-highlight-prompt nil)
  :config (setq eshell-prompt-function #'epe-theme-dakrone))
(use-package eshell-toggle
  :custom (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-size-fraction 5)
  :bind ("M-`" . eshell-toggle))

;; ── Compile pretty ──────────────────────────────────────────────────────
(use-package ansi-color :defer t)
(use-package compile
  :ensure nil
  :hook (compilation-filter . compile/visual-setup)
  :config
  (defun compile/visual-setup ()
    (let ((inhibit-read-only t))
      (visual-line-mode 1)
      (when (require 'ansi-color nil t)
        (ansi-color-apply-on-region compilation-filter-start (point))))))

;; ── Org / Markdown / Babel ──────────────────────────────────────────────
(use-package org
  :hook ((org-mode . org/visual-setup)
         (org-mode . org/refiling-setup)
         (org-mode . org/captures-setup))
  :bind (("C-c a" . org-agenda) ("C-c j" . org-capture))
  :custom
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-files '("~/Org/Tasks.org"))
  (org-return-follows-link t)
  (org-image-actual-width nil)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-hide-emphasis-markers t)
  :config
  (defun org/visual-setup () (visual-line-mode 1) (org-indent-mode) (variable-pitch-mode 1) (auto-fill-mode 0))
  (defun org/refiling-setup ()
    (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
          org-outline-path-complete-in-steps nil
          org-refile-use-outline-path t)
    (advice-add 'org-refile :after 'org-save-all-org-buffers))
  (defun org/captures-setup ()
    (setq org-capture-templates
          `(("t" "Tasks / Projects")
            ("tt" "Task" entry (file+olp "~/Org/Tasks.org" "Inbox")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
            ("j" "Journal Entries")
            ("jj" "Journal" entry (file+olp+datetree "~/Org/Journal.org")
             "* %<%I:%M %p> - Journal :journal:\n%?"
             :empty-lines 1 :kill-buffer t))))
  (setq org-babel-load-languages
        '((python . t) (shell . t) (java . t) (sql . t) (lisp . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages))

(use-package org-habit
  :ensure nil
  :after org
  :config
  (add-to-list 'org-modules 'org-habit)
  (org-clock-persistence-insinuate)
  :custom
  (org-habit-show-graph t)
  (org-habit-graph-column 60)
  (org-clock-into-drawer "LOGBOOK")
  (org-clock-out-when-done t)
  (org-clock-persist 'history)
  (org-habit-show-habits t)
  (org-habit-show-all-today nil)
  (org-habit-show-habits-only-for-today nil)
  (org-habit-preceding-days 14)
  (org-habit-following-days 1)
  (org-priority-faces
   '((?A . (:foreground "red" :weight bold))
     (?B . (:foreground "orange"))
     (?C . (:foreground "green")))))

(use-package org-agenda
  :ensure nil
  :after org
  :custom (org-agenda-sorting-strategy
           '((agenda priority-down time-up habit-down category-keep)
             (todo   priority-down category-keep)
             (tags   priority-down category-keep)
             (search category-keep))))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode) ("\\.md\\'" . markdown-mode))
  :hook (markdown-mode . visual-line-mode))

;; Babel: TS via ts-node
(use-package ob-ts-node
  :after org
  :custom
  (ob-ts-node-tsconfig "~/.config/ts-node/tsconfig.json")
  :config
  (ob-ts-node-setup)
  (add-to-list 'org-babel-load-languages '(ts-node . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))
(use-package ob-async :defer t)

;; Dashboard
(use-package dashboard
  :bind ("<f5>" . open-dashboard-buffer)
  :init
  (defun open-dashboard-buffer ()
    (interactive) (switch-to-buffer "*dashboard*"))
  (dashboard-setup-startup-hook)
  :custom

  (dashboard-startup-banner  'logo)
  (dashboard-items '((recents . 5) (bookmarks . 3)
                     (projects . 5) (agenda . 10))))

;; ── Comment & Editing ───────────────────────────────────────────────────
(use-package smartparens
  :init (require 'smartparens-config)
  :config (smartparens-global-mode 1))
(use-package smart-comment :bind ("M-;" . smart-comment))
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; ── Hydra / Jumps ───────────────────────────────────────────────────────
(use-package hydra
  :bind (("C-c C-m" . hydra-smerge/body)
         ("C-c C-g" . hydra-git-gutter/body))
  :config
  (defhydra hydra-smerge (:color red :hint nil :pre (smerge-mode 1))
    ("RET" smerge-keep-current "current" :column "Keep")
    ("u" smerge-keep-upper "upper") ("l" smerge-keep-lower "lower")
    ("a" smerge-keep-all "all")     ("b" smerge-keep-base "base")
    ("m" smerge-keep-mine "mine")   ("o" smerge-keep-other "other")
    ("n" smerge-next "next" :column "Nav") ("p" smerge-prev "prev")
    ("<" smerge-diff-base-mine "base-mine" :column "Diff")
    ("=" smerge-diff-mine-other "mine-other")
    (">" smerge-diff-base-other "base-other")
    ("C" smerge-combine-with-next "combine" :column "Other")
    ("R" smerge-refine "refine")
    ("r" smerge-resolve "resolve")
    ("E" smerge-ediff "ediff")
    ("q" nil "quit" :color blue))
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1) :hint nil)
    ("n" git-gutter:next-hunk "next" :column "Nav")
    ("p" git-gutter:previous-hunk "prev")
    ("h" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "first")
    ("l" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "last")
    ("s" git-gutter:stage-hunk "stage" :column "Stage")
    ("r" git-gutter:revert-hunk "revert")
    ("<SPC>" git-gutter:popup-hunk "popup" :column "Show")
    ("q" nil "quit")))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))
(use-package winner :ensure nil :config (winner-mode 1))
(use-package wgrep :defer t)
(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map goto-map
         ("M-c" . avy-goto-char)
         ("M-g" . avy-goto-line)))
(use-package ace-window
  :bind (("C-x o"   . ace-window) ("C-x C-o" . ace-swap-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ── Languages / Modes ───────────────────────────────────────────────────
(use-package yaml-mode :mode "\\.yml\\'")
(use-package dockerfile-mode)
(use-package hl-todo :hook ((prog-mode . hl-todo-mode) (yaml-mode . hl-todo-mode)))
(use-package go-mode)
(use-package eros :hook (emacs-lisp-mode . eros-mode))
(use-package json-mode :defer t)

;; Clojure & CIDER
(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode))
  :custom (clojure-indent-style 'always-indent))

(use-package cider
  :after clojure-mode
  :commands (cider-jack-in cider-connect)
  :hook ((cider-repl-mode . rainbow-delimiters-mode))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history t)
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-result-prefix ";; => ")
  (cider-repl-history-size 3000)
  (cider-auto-select-error-buffer t))

;; ── LSP Stack ───────────────────────────────────────────────────────────
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode c-mode c++-mode java-mode) . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l l" . lsp-find-references))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-on-type-formatting nil)
  (lsp-completion-provider :none)
  (lsp-eslint-enable t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c l d" . lsp-ui-doc-mode)
              ("C-c l c" . lsp-ui-sideline-apply-code-actions)
              ("C-c l s" . lsp-ui-find-workspace-symbol)
              ("C-c l i" . lsp-ui-imenu)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references))
  :custom
  (lsp-lens-enable t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-signature-auto-activate nil))

(use-package lsp-treemacs :after (lsp-mode treemacs) :commands lsp-treemacs-errors-list :config (lsp-treemacs-sync-mode 1))
(use-package consult-lsp :after (consult lsp-mode))

;; Flymake
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :hook
  ((emacs-lisp-mode)
   . (lambda () (flymake-mode -1))))

(use-package nix-mode
  :mode "\\.nix\\'")