;; ── Bootstrap ────────────────────────────────────────────────────────────
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile (require 'use-package))
(setq use-short-answers t)
(put 'use-package 'lisp-indent-function 1)

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



(use-package envrc :hook (after-init . envrc-global-mode))


;; ── Basic UX ─────────────────────────────────────────────────────────────
(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (delete-selection-mode 1)
  (electric-pair-mode 1)

  (dolist (mapping '(("\\.ts\\'"       . typescript-ts-mode)
                     ("\\.tsx\\'"      . tsx-ts-mode)
                     ("\\.java\\'"     . java-ts-mode)
                     ("\\.js\\'"       . js-ts-mode)
                     ("\\.mjs\\'"      . js-ts-mode)
                     ("\\.json\\'"     . json-ts-mode)
                     ("\\.yaml\\'"     . yaml-ts-mode)
                     ("Dockerfile\\'"  . dockerfile-ts-mode)
                     ("\\.go\\'"       . go-ts-mode)
                     ("/go\\.mod\\'"   . go-mod-ts-mode)
                     ("\\.nix\\'"      . nix-mode)))
    (add-to-list 'auto-mode-alist mapping))
  :hook (prog-mode . display-line-numbers-mode)
  :bind (:map global-map
              ("C-c <tab>" . indent-buffer)
              ("M-i"       . imenu)
              ("M-<up>"    . move-line-up)
              ("M-<down>"  . move-line-down)
              ("M-'"       . duplicate-line))
  :custom
  (column-number-mode t)
  (bidi-inhibit-bpa t)
  (default-input-method "german")
  (scroll-step 1)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (sentence-end-double-space nil)
  (mode-require-final-newline nil)
  (require-final-newline nil)
  (native-comp-async-report-warnings-errors 'silent)
  :config
  ;; Utils
  (defun indent-buffer () (interactive) (indent-region (point-min) (point-max)))
  (defun move-line-up ()   (interactive) (transpose-lines 1) (forward-line -2) (indent-according-to-mode))
  (defun move-line-down () (interactive) (forward-line 1) (transpose-lines 1) (forward-line -1) (indent-according-to-mode))
  (defun duplicate-line () (interactive)
         (let ((col (current-column))) (move-beginning-of-line 1) (kill-line) (yank) (newline) (yank) (move-to-column col))))

(use-package so-long :ensure nil :config (global-so-long-mode 1) :custom (so-long-threshold 4000))

;; Editing niceties
(use-package autorevert :ensure nil :init (global-auto-revert-mode 1) :custom (auto-revert-verbose nil))

(use-package gcmh
  :init
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 0.5)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

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
(use-package doom-modeline :hook (after-init . doom-modeline-mode))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'mocha))

;; Icons
(use-package all-the-icons :if (display-graphic-p))

(use-package menu-bar :ensure nil :bind ([S-f10] . menu-bar-mode))
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
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom (vertico-resize t) (vertico-cycle t))

(use-package savehist :ensure nil :init (savehist-mode 1))
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-S-s"   . consult-ripgrep)
         ("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c k"   . consult-keep-lines)
         ("C-c m"   . consult-mode-command)
         ("C-c i"   . consult-imenu)
         ("C-c !"   . consult-flymake))
  :init
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  :custom
  (register-preview-delay 0.3)
  (register-preview-function #'consult-register-format))

(use-package consult-eglot :after (consult eglot))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult :after (embark consult))

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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-head '(:height 0.8 :scale 0.8))
  (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
              ("j"       . consult-line)
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

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("p" "~/Development/"              "Projects")))
  :config
  (dirvish-peek-mode)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)))

;; ── Shell / Eshell ──────────────────────────────────────────────────────
(use-package exec-path-from-shell
  :if (memq system-type '(gnu/linux darwin))
  :init (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables '("PATH" "SHELL" "MANPATH"))
  (exec-path-from-shell-arguments '("-l")))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :hook (eshell-mode . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after esh-opt
  :custom
  (eshell-highlight-prompt nil)
  :config
  (setq eshell-prompt-function #'epe-theme-dakrone))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (eshell/alias "update" "sudo nixos-rebuild switch --flake ~/Development/nixos-config#nixos-rog")
                         (eshell/alias "upgrade" "nix flake update --flake ~/Development/nixos-config && update")
                         (eshell/alias "gc" "nix-collect-garbage -d")))
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-hist-ignoredups t)
  :config
  (setq eshell-visual-commands
        '("htop" "top" "less" "more" "ssh" "tmux" "docker-compose" "node" "ipython" "pip" "pipenv")))

(use-package eshell-toggle
  :bind ("M-`" . eshell-toggle)
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-size-fraction 3))

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

(use-package org-present
  :bind (("<f9>" . org-present))
  :hook
  ((org-present-mode . (lambda ()
                         (org-present-big)
                         (org-display-inline-images)
                         (org-present-read-only)))
   (org-present-mode-quit . (lambda ()
                              (org-present-small)
                              (org-remove-inline-images)
                              (org-present-show-cursor)
                              (org-present-read-write)))))

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

(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(defun query-replace-from-top ()
  "Go to the beginning of the buffer (or narrowing) and start query-replace."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'query-replace)))
(global-set-key (kbd "C-c ;") 'query-replace-from-top)

(use-package puni
  :defer t
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("M-(" . puni-wrap-round)
              ("M-{" . puni-wrap-curly)
              ("M-[" . puni-wrap-square)
              ("M-s" . puni-splice)
              ("M-r" . puni-raise)
              ("C-{" . puni-slurp-forward)
              ("C-}" . puni-barf-forward)
              ("M-d" . puni-forward-kill-word)
              ("C-w" . puni-kill-region)))

(use-package hl-todo :hook ((prog-mode . hl-todo-mode) (yaml-ts-mode . hl-todo-mode)))
(use-package eros :hook (emacs-lisp-mode . eros-mode))

;; EGLOT
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eglot
  :ensure nil
  :hook
  ((typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format-buffer)
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-action-organize-imports))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  ;; Formatting
  ;; (add-hook 'before-save-hook
  ;;           (lambda () (when (eglot-managed-p) (eglot-format-buffer))))
  )

(use-package dumb-jump
  :custom (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Flymake
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :hook
  ((emacs-lisp-mode) . (lambda () (flymake-mode -1))))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package ob-mermaid
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook
  ((clojure-mode . subword-mode)
   (clojure-mode . puni-mode)
   (clojure-mode . eglot-ensure)))

(use-package cider
  :hook ((clojure-mode . cider-mode)
         (cider-repl-mode . puni-mode))
  :bind (:map cider-mode-map
              ("C-c u" . cider-user-ns)
              :map cider-repl-mode-map
              ("C-c M-o" . cider-repl-clear-buffer))
  :custom
  (cider-preferred-build-tool 'clojure-cli)
  (cider-enrich-classpath t)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package eca
  :bind ("C-c e" . eca))