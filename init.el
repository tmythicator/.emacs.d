(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  (use-package-enable-imenu-support t))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

;; :quelpa keyword
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)

(use-package paradox
  :ensure t
  :defer t
  :config
  (paradox-enable))

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :config
  (delete-selection-mode t)
  ;; Set default indentation for various languages (add your own!)
  (setq-default tab-width 2)
  ;; Javascript
  (setq-default js2-basic-offset 2)
  ;; JSON
  (setq-default js-indent-level 2)
  ;; Coffeescript
  (setq-default coffee-tab-width 2)
  ;; Typescript
  (setq-default typescript-indent-level 4)
  ;; Python
  (setq-default py-indent-offset 2)
  ;; XML
  (setq-default nxml-child-indent 2)
  ;; C/Java (lsp)
  (setq-default c-basic-offset 4)

  ;; Custom functions
  (defun indent-buffer ()
    "Autoindent the whole buffer"
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun move-line-up ()
    "Move up the current line."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode))

  (defun move-line-down ()
    "Move down the current line."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode))

  (defun duplicate-line ()
    "Duplicates the current line."
    (interactive)
    (let ((col (current-column)))
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (newline)
      (yank)
      (move-to-column col)))

  :bind (:map global-map
              ("C-c <tab>" . indent-buffer)
              ("M-i" . imenu)
              ("M-p" . move-line-up)
              ("M-n" . move-line-down)
              ("M-'" . duplicate-line))
  :custom
  ;; Long text scanning improvements
  (bidi-inhibit-bpa t)

  (default-input-method "german")
  (scroll-step 1)
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (debug-on-quit nil)
  ;; Indentation and things
  (sentence-end-double-space nil)
  (mode-require-final-newline nil)
  (require-final-newline nil)
  ;; For LSP
  (read-process-output-max (* 1024 1024))
  (gc-cons-threshold 100000000))

(use-package so-long
  :config
  (global-so-long-mode t)
  :custom
  (so-long-threshold 400))

;; Emacs Window Manager
(use-package exwm
  :requires xelb
  :quelpa
  (exwm :repo "ch11ng/exwm"
        :fetcher github)
  :commands (exwm-init))

(use-package expand-region
  :ensure t
  :bind (:map global-map
              (("C-'" . er/expand-region )
               ("C-;" . er/contract-region)))
  :config
  (pending-delete-mode t))

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package imenu
  :config (setq imenu-auto-rescan t))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  ;; backup settings
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(expand-file-name
               (concat user-emacs-directory "backups")))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package recentf
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))

(use-package cus-edit
  :defer t
  :custom
  (custom-file
   (if (eq system-type 'windows-nt)
       (locate-user-emacs-file "custom.el")
     null-device)))

(use-package frame
  :bind
  ("C-z" . nil))

(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-style "bar"
	      centaur-tabs-height 32
	      centaur-tabs-set-icons t
	      centaur-tabs-set-modified-marker t
	      centaur-tabs-show-navigation-buttons t
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

(use-package doom-modeline
  :ensure t
  :hook (window-setup . doom-modeline-mode))

;; Parentheses + hline
(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1))

(use-package paren
  :config
  (show-paren-mode t))

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :config (hes-mode))

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

;; Ivy and counsel
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (counsel-mode)
  :bind
  ("M-y" . counsel-yank-pop))

(use-package amx :ensure t :defer t)

(use-package counsel-world-clock
  :ensure t
  :after counsel)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :bind
  (:map mode-specific-map
        ("C-r" . ivy-resume))
  :config
  (ivy-mode t))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package flyspell-correct-ivy
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

(use-package swiper
  :ensure t
  :bind("C-z" . swiper))

;; Rainbow
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook prog-mode)

(use-package dashboard
  :ensure t
  :bind
  ("<f5>" . open-dashboard-buffer)
  :init
  (dashboard-setup-startup-hook)

  ;; Custom functions
  (defun open-dashboard-buffer ()
    "Opens up the dashboard buffer, if exists"
    (interactive)
    (switch-to-buffer "*dashboard*"))

  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 7)
                     (projects . 5)
                     (agenda . 5))))

;; Tramp things
(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method (if (eq system-type 'windows-nt)
                            "plink"
                          "ssh"))
  (tramp-default-proxies-alist nil))

(use-package counsel-tramp
  :after tramp
  :ensure t
  :custom
  (make-backup-files nil)
  (create-lockfiles nil))

;; Rest client stuff
(use-package request
  :defer t
  :ensure t)

(use-package restclient
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package restclient-test
  :ensure t
  :hook
  (restclient-mode-hook . restclient-test-mode))

(use-package ob-restclient
  :ensure t
  :after org restclient)

(use-package ob-ts-node
  :after org typescript
  :quelpa
  (ob-ts-node :repo "tmythicator/ob-ts-node"
              :fetcher github))

(use-package ob-async
  :defer t
  :ensure t)

;; Commenting
(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

;; Eshell stuff
(use-package exec-path-from-shell
  :if (eq system-type 'gnu/linux)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package load-bash-alias
  :if (eq system-type 'gnu/linux)
  :ensure t)

(use-package bash-completion
  :commands bash-completion-dynamic-complete
  :hook
  (bash-completion-dynamic-complete . shell-dynamic-complete-functions)
  :init
  (setq fish-completion-fallback-on-bash-p t))

(use-package em-term
  :config
  (setq eshell-visual-commands
        `(,@eshell-visual-commands
          "pip" "pipenv" "npm" "mvn"))
  :custom
  (eshell-destroy-buffer-when-process-dies t))

(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :ensure t
  :hook
  (eshell-mode . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :ensure t
  :after esh-opt
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function #'epe-theme-dakrone))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-size-fraction 5)
  :bind
  ("M-`" . eshell-toggle))

;; Elisp stuff
(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

;; LSP-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l l" . lsp-find-references)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l r" . lsp-rename))

  :hook(((java-mode js2-mode c-mode c++-mode web-mode typescript-mode) . lsp)
        (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c l c" . lsp-ui-sideline-apply-code-actions)
              ("C-c l s" . lsp-ui-find-workspace-symbol)
              ("C-c l d" . lsp-ui-doc-mode)
              ("C-c l i" . lsp-ui-imenu)
              ("C-c l e" . lsp-ui-flycheck-list)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-use-webkit t)  ;; Use lsp-ui-doc-webkit only in GUI
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  ;; (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-ignore-duplicate t)
  ;;  (lsp-ui-sideline-show-code-actions nil)

  :config
  ;; (flycheck-add-next-checker 'lsp-ui 'typescript-tslint)
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; JAVA
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :bind (:map java-mode-map
              ("C-c j a" . lsp-java-add-import)
              ("C-c j o" . lsp-java-organize-imports)))

;; Debugger for LSP
(use-package dap-mode
  :ensure t
  :after lsp
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  :bind (:map dap-mode-map
              ("<f6>" . dap-step-in)
              ("<f7>" . dap-next)
              ("<f8>" . dap-continue)))

(use-package dap-java :after lsp-java)

;; Haskell stuff
(use-package haskell-mode
  :defer t
  :ensure t)

;; Clojure stuff
(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t)

;; Groovy
(use-package groovy-mode
  :ensure t
  :defer t)

;; Python stuff
(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt --pprint"))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (lsp))))

(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate)
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package ipython-shell-send
  :ensure t
  :bind ("C-c r" . ipython-shell-send-region)
  :hook (python-mode-hook))

;; Scheme
(use-package geiser
  :ensure t
  :defer t
  :custom
  (geiser-default-implementation 'guile))

(use-package slime
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl"))

;; JS/TS/React stuff
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts$" "\\.tsx$")
  :config
  (defun ts/typescript-setup-hook ()
    (interactive)
    (major-mode-suspend)
    (web-mode)
    (major-mode-restore))

  (add-hook 'find-file-hook
            (lambda ()
              (when (eq major-mode 'typescript-mode)
                (ts/typescript-setup-hook)))))


(use-package web-mode
  :ensure t
  :defer t
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-style-padding 4)
  (web-mode-script-padding 4))

(use-package rjsx-mode
  :ensure t
  :mode
  (("\\.js$" . rjsx-mode)
   ("\\.jsx$" . rjsx-mode)))


(use-package js-comint
  :ensure t
  :commands(js-comint-repl)
  :config
  (add-hook 'js2--mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))))

(use-package eslintd-fix
  :ensure t
  :defer t)

(use-package jest
  :ensure t
  :defer t)

(use-package flycheck-jest
  :ensure t
  :after jest
  :config
  (flycheck-jest-setup))

;; JSON
(use-package json-mode
  :ensure t
  :defer t)

;; Company
(use-package company
  :ensure t
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :config
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)
    :custom
    (company-quickhelp-delay 3))

  (use-package company-shell
    :ensure t
    :config
    (add-to-list 'company-backends 'company-shell))

  :custom
  ;; Except when you're in term-mode/python-mode.
  ;; (company-global-modes '(not term-mode))
  ;; Give Company a decent default configuration.
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :hook
  (after-init . global-company-mode))

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package company-emoji
  :ensure t
  :after company
  :config
  (company-emoji-init))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'find-file-hook
            (lambda ()
              (unless (memq major-mode '(emacs-lisp-mode solidity-mode))
                (flycheck-mode)))))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; Whitespaces
(use-package shrink-whitespace
  :ensure t
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Navigation and thing
(use-package hydra
  :ensure t
  :bind (("C-c w" . hydra-window/body)
         ("C-c g" . hydra-git-gutter/body)
         ("C-c c" . hydra-clock/body))
  :config

  (use-package hydra-examples)
  (defhydra hydra-window (:color pink :hint nil :timeout 20)
    ("<up>" windmove-up "up" :column "Move")
    ("<down>" windmove-down "down")
    ("<left>" windmove-left "left")
    ("<right>" windmove-right "right")
    ("C-<up>" hydra-move-splitter-up "up" :column "Resize")
    ("C-<down>" hydra-move-splitter-down "down")
    ("C-<left>" hydra-move-splitter-left "left")
    ("C-<right>" hydra-move-splitter-right "right")
    ("+" text-scale-increase "increase" :column "Scale")
    ("-" text-scale-decrease "decrease")
    ("0" text-scale-set "normal")
    ("h" split-window-below "horizontal" :column "Split")
    ("v" split-window-right "vertical")
    ("q" nil "quit"))

  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    ("n" git-gutter:next-hunk "next hunk" :column "Navigation")
    ("p" git-gutter:previous-hunk "previous hunk")
    ("h" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "first hunk")
    ("l" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "last hunk")
    ("s" git-gutter:stage-hunk "stage hunk" :column "Staging")
    ("r" git-gutter:revert-hunk "revert hunk")
    ("<SPC>" git-gutter:popup-hunk "popup h" :column "Display")
    ("q" nil "quit"))

  (defhydra hydra-clock (:color blue)
    ("q" nil "quit" :column "Clock")
    ("c" org-clock-cancel "cancel" :color pink :column "Do")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("r" org-clock-report "report")))

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :custom (dumb-jump-selector 'ivy))

(use-package winner
  :config
  (winner-mode 1))

(use-package wgrep
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  (("C-," .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   :map goto-map
   ("M-c" . avy-goto-char)
   ("M-g" . avy-goto-line)))

(use-package avy-zap
  :ensure t
  :bind
  ([remap zap-to-char] . avy-zap-to-char))

(use-package ace-jump-buffer
  :ensure t
  :bind
  (:map goto-map
        ("M-b" . ace-jump-buffer)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  :custom
  (projectile-indexing-method 'alien)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (counsel-projectile-mode))

;; Use ibuffer instead of list-buffers (C-x C-b) and sort by project.
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; Org-mode/PDF/Markdown stuff
(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . org/visual-setup)
         (org-mode . org/habits-setup)
         (org-mode . org/refiling-setup)
         (org-mode . org/captures-setup))
  :bind
  (("C-c a" . org-agenda)
   ("C-c j" . org-capture))
  :config
  (defun org/visual-setup ()
    "Sets up org-mode to be visually more appealing"
    (visual-line-mode 1)
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1))

  (defun org/refiling-setup ()
    "Sets up refiling for org-mode"
    (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-use-outline-path t)
    (advice-add 'org-refile :after 'org-save-all-org-buffers))

  (defun org/habits-setup ()
    "Sets up habits for org-mode"
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60))

  (defun org/captures-setup ()
    "Sets up captures for org-mode"
    (setq org-capture-templates
          `(("t" "Tasks / Projects")
            ("tt" "Task" entry (file+olp "~/Org/Tasks.org" "Inbox")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

            ("j" "Journal Entries")
            ("jj" "Journal" entry
             (file+olp+datetree "~/Org/Journal.org")
             "* %<%I:%M %p> - Journal :journal:\n%?"
             :empty-lines 1
             :kill-buffer t)

            ("jd" "Dream Journal" entry
             (file+olp+datetree "~/Org/DreamJournal.org")
             "* %^{Dream title} %t :dream:\n%?"
             :empty-lines 1
             :kill-buffer t))))

  ;; Babel src evaluator
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (java . t)
     (sql . t)
     (lisp . t)
     (scheme . t)
     (ts-node . t)
     (restclient . t)))

  :custom
  (org-agenda-files '("~/Org/Tasks.org"
                      "~/Org/Habits.org"
                      "~/Org/Journal.org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (org-todo-keyword-faces
   '(("SAMPLE" . "red")
     ("NONE" . "white"))))

;; Needs msys2 for windows. Deactivate as workaround
(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :ensure t
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package nov
  :ensure t
  :mode
  ("\\.epub$" . nov-mode))

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Git stuff
(use-package magit
  :ensure t
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind ("C-x g" . magit-status))

;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

;; Travel back in git
(use-package git-timemachine
  :ensure t
  :defer t)

;; Docker/Kubernetes things
(use-package docker
  :ensure t
  :bind
  (:map mode-specific-map
        ("d" . docker)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;Dired things
(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ([mouse-2] . dired-mouse-find-file)
        ([mouse-3] . dired-mouse-find-file-other-window)
        ("," . dired-hide-details-mode))
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil)

(use-package dired-git-info
  :ensure t
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package async
  :init
  (dired-async-mode t))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync)))

;; Treemacs
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
        ("C-x C-n" . treemacs)
        ("C-x t b" . treemacs-bookmark)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :ensure t
  :config (treemacs-icons-dired-mode t)
  :hook(dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Help things
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package free-keys
  :ensure t
  :commands free-keys)

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package sudo-edit
  :ensure t
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

(use-package all-the-icons :ensure t)

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1)
  :bind
  ([S-f10] . menu-bar-mode))

(use-package tooltip
  :defer t
  :custom
  (tooltip-mode -1))

(use-package mood-line
  :ensure t
  :hook (after-init . mood-line-mode))

;; Transparency
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))
(set-frame-parameter nil 'alpha '(95 . 75))
(fringe-mode 10)

(use-package modus-themes
  :ensure t
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-org-blocks 'rainbow)
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs t))

;; My custom functions
(defun my/insert-change-id ()
  "Generate change Id on the third line of the current buffer (gerrit)."
  (interactive)
  (let* ((charset "0123456789abcdef")
         (base-count (length charset))
         (number-of-hex-chars 40)
         (res '()))

    (dotimes (_ number-of-hex-chars)
      (setq res (cons (elt charset (random base-count)) res)))
    (goto-line 3)
    (move-beginning-of-line nil)
    (insert (concat "Change-Id: I" res "\n"))
    (goto-line 1)
    (move-beginning-of-line nil)))

(defun my/tslint-fix-current-buffer ()
  "TSlints the current file/buffer"
  (interactive)
  (start-process-shell-command "tslint-autofix"
                               "*tslint-autofix*"
                               (concat "tslint --fix " (buffer-file-name))))

(defun my/play-media (file-path &optional
                                minimized
                                pr-name)
  "Plays the media asynchronously"
  (interactive "f\nfile-name")
  (unless pr-name (setq pr-name "Emacs-VLC"))
  (let* ((player-name "vlc")
         (player-flags (and minimized "--qt-start-minimized --qt-notification 0 --play-and-exit"))
         (shell-command (concat player-name " " file-path " " player-flags)))
    (start-process-shell-command pr-name
                                 "*VLC-Log*"
                                 shell-command)))
