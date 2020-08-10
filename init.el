(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

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
  :defer 1
  :config
  (paradox-enable))

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :config
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
  (default-input-method "german")
  (delete-selection-mode t)
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
  (require-final-newline nil))

;; Emacs Window Manager
(use-package exwm
  :ensure t
  :config
  (use-package exwm-config
    :config (exwm-config-default)))

(use-package expand-region
  :ensure t
  :bind (:map global-map
              (("C-'" . er/expand-region )
               ("C-;" . er/contract-region)))
  :config
  (pending-delete-mode t))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package imenu
  :defer
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
  (run-with-idle-timer 30 t 'recentf-save-list))

(use-package cus-edit
  :custom
  (custom-file
   (if (eq system-type 'windows-nt)
       (locate-user-emacs-file "custom.el")
     null-device)))

(use-package frame
  :bind
  ("C-z" . nil))

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
  :after (ivy)
  :config
  (ivy-rich-mode 1))

(use-package flyspell-correct-ivy
  :ensure t
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

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
  (("<f5>" . open-dashboard-buffer)
   :map dashboard-mode-map
   ("l" . play-dashboard-theme)
   ("s" . initiate-hvatit))
  :init
  (dashboard-setup-startup-hook)

  ;; Custom functions
  (defun open-dashboard-buffer ()
    "Opens up the dashboard buffer, if exists"
    (interactive)
    (switch-to-buffer "*dashboard*"))

  (defun initiate-hvatit ()
    "Stops the dashboard theme"
    (interactive)
    (my/play-media (concat user-emacs-directory "dashboard/dashboard-enough.mp3") t)
    (shell-command "pkill -f dashboard-theme.mp3"))

  (defun play-dashboard-theme ()
    "Plays the dashboard theme"
    (interactive)
    (my/play-media (concat user-emacs-directory "dashboard/dashboard-theme.mp3") t))

  :custom
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
  :ensure t
  :custom
  (make-backup-files nil)
  (create-lockfiles nil))

;; Rest client stuff
(use-package request
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
  :ensure t)

(use-package ob-async :ensure t)

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

(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc))

(use-package em-term
  :custom
  ;; Visual commands are commands which require a proper terminal
  ;; eshell will run them in a term buffer when you invoke them.
  (eshell-visual-commands
   '("pip" "pipenv" "npm" "mvn" "tmux" "htop" "top"))
  (eshell-visual-subcommands
   '(("git" "log" "l" "diff" "show"))))

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

  :hook ((java-mode python-mode typescript-mode web-mode js2-mode c-mode c++-mode) . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
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
  (lsp-ui-doc-border (face-foreground 'default))
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

  (use-package dap-firefox
    :config
    (dap-firefox-setup)
    (dap-register-debug-template "Custom Firefox Debug Config"
                                 (list :type "firefox"
                                       :cwd nil
                                       :request "launch"
                                       :file nil
                                       :url "http://localhost:3000"
                                       :webRoot "${workspaceRoot}/src"
                                       :reAttach t
                                       :program nil
                                       :name "Custom Firefox::Run")))

  (use-package dap-node
    :config
    (dap-node-setup))

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
(use-package haskell-mode :ensure t)

;; Clojure stuff
(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider :ensure t)

;; Groovy
(use-package groovy-mode :ensure t)

;; ;; ABAP stuff
;; ;; haven't found the workaround in windows yet
;; (use-package abap
;;   :if (eq system-type 'gnu/linux)
;;   :quelpa
;;   (abap :repo "qianmarv/sap-abap-mode"
;;         :fetcher github
;;         :version original))

;; (use-package abap-mode
;;   :if (eq system-type 'gnu/linux)
;;   :mode ("\\.abap\\'" . abap-mode)
;;   :quelpa
;;   (abap-mode :repo "qianmarv/ABAPInEmacs"
;;              :fetcher github
;;              :version original))

;; Python stuff
(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt --pprint"))

(use-package pyvenv
  :ensure t)

(use-package ipython-shell-send
  :ensure t
  :bind ("C-c r" . ipython-shell-send-region)
  :hook (python-mode-hook))

;; ;; JS/TS/React stuff
(use-package rjsx-mode
  :ensure t
  :mode
  (("\\.js$" . rjsx-mode)
   ("\\.jsx$" . rjsx-mode)))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts$" . typescript-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.tsx$" . web-mode)
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-style-padding 4)
  (web-mode-script-padding 4))

(use-package ts-comint
  :ensure t
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'ts-send-buffer)
              (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'ts-load-file-and-go))))

(use-package eslintd-fix :ensure t)

(use-package jest :ensure t)

(use-package flycheck-jest
  :ensure t
  :after jest
  :config
  (flycheck-jest-setup))

;; Autofix missing imports.
(use-package import-js :ensure t)

;; JSON
(use-package json-mode :ensure t)

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

  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :ensure t
    :config
    (company-emoji-init))

  (use-package company-shell
    :ensure t
    :config
    (add-to-list 'company-backends 'company-shell))

  :custom
  ;; Except when you're in term-mode/python-mode.
  (company-global-modes '(not term-mode))
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

(use-package windmove
  :bind
  ("M-<up>" . windmove-up)
  ("M-<down>" . windmove-down)
  ("M-<left>" . windmove-left)
  ("M-<right>" . windmove-right))

(use-package wgrep :ensure t)

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
  ;; to be sure we have latest Org version
  :ensure org-plus-contrib
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :config
  (setq org-default-notes-file "~/Org/notes.org")
  ;; Should normaly support russian
  (setq org-latex-compiler "xelatex")
  ;; Set default inline image width
  (setq org-image-actual-width '(400))
  ;; Stop org-mode from highjacking shift-cursor keys.
  (setq org-replace-disputed-keys t)
  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)))
  ;; Minted
  (require 'ox-latex)
  (setq org-latex-listings 'minted)

  ;; Babel src evaluator
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (java . t)
     (restclient . t)))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex $(basename %b)"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :custom
  (org-src-tab-acts-natively t)
  (org-todo-keyword-faces
   '(("SAMPLE" . "red")
     ("NONE" . "white"))))

(use-package org-projectile
  :ensure t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list '("•"))
  (org-ellipsis "▼")
  :hook
  (org-mode . org-bullets-mode))

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
  (magit-completing-read-function 'ivy-completing-read "Force Ivy usage.")
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
  :defer t
  :bind
  (:map global-map
        ("C-x C-n" . treemacs)
        ("C-x t b" . treemacs-bookmark)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

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
  :defer t
  :commands free-keys)

(use-package helpful
  :ensure t
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package sudo-edit
  :ensure t
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

;; Ricing
(use-package emojify
  :ensure t
  :custom
  ;; Set emojify to only replace Unicode emoji, and do it everywhere.
  (emojify-emoji-styles '(unicode))
  (emojify-inhibit-major-modes '())
  :hook
  ((after-init . global-emojify-mode-line-mode)
   (after-init . global-emojify-mode)))

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

(use-package modus-operandi-theme
  :ensure t
  :config
  (load-theme 'modus-operandi t)
  :custom
  (modus-operandi-theme-org-blocks 'rainbow)
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t))

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