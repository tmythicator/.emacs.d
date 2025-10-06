(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
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

(use-package system-packages :custom (system-packages-noconfirm t))
(use-package quelpa :defer t :custom (quelpa-update-melpa-p nil))
(use-package quelpa-use-package)

;;;; ───────────────────────────────── Basic UX ─────────────────────────────────
(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (delete-selection-mode t)
  (setq-default tab-width 2
                nxml-child-indent 2
                c-basic-offset 4)
  (setq native-comp-async-report-warnings-errors 'silent)

  ;; Emacs 30+ builtin TS/JS major modes (tree-sitter)
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))


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

  (add-to-list 'default-frame-alist '(alpha . (95 . 75)))
  (set-frame-parameter nil 'alpha '(95 . 75))
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
  (require-final-newline nil))

(use-package so-long :ensure nil :config (global-so-long-mode t) :custom (so-long-threshold 4000))
(use-package cus-edit
  :ensure nil
  :defer t
  :custom
  (custom-file
   (if (eq system-type 'windows-nt)
       (locate-user-emacs-file "custom.el")
     null-device)))

;; Editing niceties
(use-package expand-region
  :bind (("C-'" . er/expand-region) ("C-;" . er/contract-region))
  :config (pending-delete-mode t))
(use-package autorevert :ensure nil :custom (global-auto-revert-mode t))
(use-package ibuffer :ensure nil :bind ([remap list-buffers] . ibuffer))
(use-package imenu :ensure nil :custom (imenu-auto-rescan t))
(use-package files
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (auto-save-default nil) (create-lockfiles nil)
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(expand-file-name (concat user-emacs-directory "backups")))))
  (delete-old-versions t) (kept-new-versions 6) (kept-old-versions 2))
(use-package recentf
  :ensure nil
  :defer 0.1
  :custom (recentf-auto-cleanup 30)
  :config (recentf-mode) (run-with-idle-timer 30 t 'recentf-save-list))
(use-package frame :ensure nil :bind ("C-z" . nil))

;;;; ─────────────────────────────── UI: Theme/Modeline/Tabs ───────────────────
(use-package doom-themes
  :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t) (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline :hook (window-setup . doom-modeline-mode))

(use-package centaur-tabs
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-mode t)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (eshell-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; Parens / highlighting
(use-package smartparens
  :commands (smartparens-mode)
  :hook ((clojure-mode cider-repl-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))
(use-package paren :ensure nil :config (show-paren-mode t))
(use-package hl-line :ensure nil :hook (prog-mode . hl-line-mode))
(use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
(use-package highlight-escape-sequences :config (hes-mode))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))

;; Icons
(use-package all-the-icons)
(use-package tool-bar :ensure nil :config (tool-bar-mode -1))
(use-package scroll-bar :ensure nil :config (scroll-bar-mode -1))
(use-package menu-bar :ensure nil :config (menu-bar-mode -1) :bind ([S-f10] . menu-bar-mode))
(use-package tooltip :ensure nil :custom (tooltip-mode -1))

;;;; ───────────────────────────── Ivy / Counsel / Friends ─────────────────────
(use-package ivy
  :init (ivy-mode 1)
  :custom (ivy-count-format "%d/%d ") (ivy-use-selectable-prompt t))
(use-package counsel :after ivy :init (counsel-mode 1) :bind (("M-y" . counsel-yank-pop)))
(use-package swiper  :after ivy :bind (("C-z" . swiper)))
(use-package amx     :after ivy :defer t)
(use-package ivy-rich :after ivy :init (ivy-rich-mode 1))
(use-package helpful :bind (("C-h f" . helpful-callable) ("C-h v" . helpful-variable) ("C-h k" . helpful-key)))
;;;; ────────────────────────────── Project / Git / Grep ───────────────────────
(use-package projectile
  :diminish projectile-mode
  :bind (:map mode-specific-map ("p" . projectile-command-map))
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  :custom
  (projectile-track-known-projects-automatically nil)
  (projectile-auto-discover nil)
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy))

(use-package magit :custom (magit-completing-read-function 'ivy-completing-read) :bind ("C-x g" . magit-status))
(use-package git-gutter-fringe :diminish git-gutter-mode :config (global-git-gutter-mode t))
(use-package git-timemachine :defer t)
(use-package rg :config (rg-enable-default-bindings))
(use-package counsel-projectile :after (counsel projectile) :config (counsel-projectile-mode))

;;;; ────────────────────────────────── Dired / Treemacs ───────────────────────
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
(use-package async :init (dired-async-mode t))
(use-package dired-rsync :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package treemacs
  :bind (:map global-map
              ("C-x C-n" . treemacs)
              ("C-x t b" . treemacs-bookmark)))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-mode) :config (treemacs-icons-dired-mode t))
(use-package treemacs-magit :after (treemacs magit))

;; --- Dired cluster

;;;; ─────────────────────────────── Shell / Eshell setup ──────────────────────
(use-package exec-path-from-shell
  :if (memq system-type '(gnu/linux darwin))
  :custom (exec-path-from-shell-variables '("PATH" "SHELL"))
  (exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))
(use-package load-bash-alias :if (eq system-type 'gnu/linux))
(use-package bash-completion
  :commands bash-completion-dynamic-complete
  :hook (bash-completion-dynamic-complete . shell-dynamic-complete-functions)
  :init  (setq fish-completion-fallback-on-bash-p t))
(use-package em-term
  :ensure nil
  :config (setq eshell-visual-commands `(,@eshell-visual-commands "pip" "pipenv"))
  :custom (eshell-destroy-buffer-when-process-dies t))
(use-package esh-autosuggest :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-fringe-status :hook (eshell-mode . eshell-fringe-status-mode))
(use-package eshell-prompt-extras
  :after esh-opt
  :custom (eshell-highlight-prompt nil) (eshell-prompt-function #'epe-theme-dakrone))
(use-package eshell-toggle
  :custom (eshell-toggle-use-projectile-root t) (eshell-toggle-run-command nil) (eshell-toggle-size-fraction 5)
  :bind ("M-`" . eshell-toggle))

;;;; ─────────────────────────────── Compile pretty ────────────────────────────
(use-package compile
  :ensure nil
  :hook (compilation-filter . compile/visual-setup)
  :config
  (defun compile/visual-setup ()
    (let ((inhibit-read-only t))
      (use-package ansi-color)
      (visual-line-mode 1)
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;;; ──────────────────────────── Org / Markdown / Babel ───────────────────────
(use-package org
  :hook ((org-mode . org/visual-setup)
         (org-mode . org/refiling-setup)
         (org-mode . org/captures-setup))
  :bind (("C-c a" . org-agenda) ("C-c j" . org-capture))
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
             "* %<%I:%M %p> - Journal :journal:\n%?" :empty-lines 1 :kill-buffer t))))
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (shell . t) (java . t) (sql . t) (lisp . t)))
  :custom
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-files '("~/Org/Tasks.org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

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

;; Dashboard
(use-package dashboard
  :bind ("<f5>" . open-dashboard-buffer)
  :init
  (defun open-dashboard-buffer () (interactive) (switch-to-buffer "*dashboard*"))
  (dashboard-setup-startup-hook)
  :custom

  (dashboard-startup-banner  'logo)
  (dashboard-items '((recents . 5) (bookmarks . 3) (projects . 5) (agenda . 10))))

;; Babel extras
(use-package ob-ts-node
  :after org
  :config
  (ob-ts-node-setup)
  (add-to-list 'org-babel-load-languages '(ts-node . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages)
  :custom
  (ob-ts-node-tsconfig "~/.config/ts-node/tsconfig.json"))

(use-package ob-async :defer t)

;;;; ────────────────────────────── Comment & Editing ──────────────────────────
(use-package smart-comment :bind ("M-;" . smart-comment))
(use-package shrink-whitespace :commands shrink-whitespace :bind ("C-c DEL" . shrink-whitespace))

;;;; ─────────────────────────────── Hydra / Avy / Jump ────────────────────────
(use-package hydra
  :bind (("C-c m" . hydra-smerge/body)
         ("C-c g" . hydra-git-gutter/body))
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
    ("R" smerge-refine "refine") ("r" smerge-resolve "resolve") ("E" smerge-ediff "ediff")
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
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :custom (dumb-jump-selector 'ivy))
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

;;;; ─────────────────────────────── Languages / Modes ─────────────────────────
(use-package yaml-mode :mode "\\.yml\\'")
(use-package dockerfile-mode)
(use-package hl-todo :hook ((prog-mode . hl-todo-mode) (yaml-mode . hl-todo-mode)))
(use-package go-mode)
(use-package eros :hook (emacs-lisp-mode . eros-mode))

;; JSON
(use-package json-mode :defer t)

;; ---------- Clojure & CIDER ----------
(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode))
  :custom
  (clojure-indent-style 'always-indent))

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

;;;; ─────────────────────────────────── LSP Stack ─────────────────────────────
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l l" . lsp-find-references))
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode c-mode c++-mode java-mode) . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-on-type-formatting nil)
  (lsp-prefer-capf t))

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
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-enable nil)
  (lsp-signature-auto-activate nil))

(use-package lsp-treemacs :after (lsp-mode treemacs) :commands lsp-treemacs-errors-list :config (lsp-treemacs-sync-mode 1))
(use-package lsp-ivy :after (lsp-mode ivy) :commands lsp-ivy-workspace-symbol)

;; Company completion
(use-package company
  :diminish company-mode
  :bind (("C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :hook (after-init . global-company-mode))

;; Flycheck + eslint_d
(use-package flycheck
  :init (global-flycheck-mode t)
  :config
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang emacs-lisp emacs-lisp-checkdoc typescript-tslint))
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode))

(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :custom (flycheck-highlighting-mode 'symbols))