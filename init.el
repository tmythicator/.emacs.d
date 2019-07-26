(require 'package)
(setq package-archives
      `(;;,@package-archives
        ("gnu" . "http://elpa.gnu.org/packages/") ;; No https :/
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

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  (use-package-enable-imenu-support t))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

;; :diminish keyword
(use-package diminish :ensure t)

;; :bind keyword
(use-package bind-key :ensure t)

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
  (setq coffee-tab-width 2)
  ;; Typescript
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2)
  ;; Python
  (setq-default py-indent-offset 2)
  ;; XML
  (setq-default nxml-child-indent 2)
  ;; C
  (setq-default c-basic-offset 2)
  ;; HTML etc with web-mode
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-style-padding 2
                web-mode-script-padding 2)
  ;; Custom functions

  ;; Hit C-c <tab> to auto-indent the entire buffer you're in.
  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))
  (global-set-key (kbd "C-c <tab>") 'indent-buffer)

  :custom
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

(use-package autorevert
  :diminish auto-revert-mode)

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
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
  (custom-file null-device "Don't store customizations"))

(use-package frame
  :bind
  ("C-z" . nil))

;; Parent + hline
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
(use-package smex :ensure t)

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (counsel-mode)
  :bind
  ("M-y" . counsel-yank-pop))

(use-package counsel-world-clock
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

(use-package swiper
  :ensure t
  :bind("C-z" . swiper))

;; Rainbow
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; (use-package rainbow-identifiers
;;   :ensure t
;;   ;;:custom
;;   ;;(rainbow-identifiers-cie-l*a*b*-lightness 80)
;;   ;;(rainbow-identifiers-cie-l*a*b*-saturation 50)
;;   ;;(rainbow-identifiers-choose-face-function
;;   ;; #'rainbow-identifiers-cie-l*a*b*-choose-face)
;;   :hook
;;   (emacs-lisp-mode . rainbow-identifiers-mode) ; actually, turns it off
;;   (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  ;;   :ensure t
  :diminish rainbow-mode
  :hook prog-mode)

;; Telegram
(use-package telega
  :ensure t
  :defer t
  :commands (telega))

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
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5))))

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

;; Elisp stuff
(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

;; Clojure stuff
(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  :config
  ;; sadly, we can't use :diminish keyword here, yet
  (diminish 'cider-mode
            '(:eval (format " 🍏%s" (cider--modeline-info)))))

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

(use-package jedi
  :ensure t
  :custom
  (jedi:complete-on-dot t)
  ;;  (jedi:tooltip-method '(popup))
  :hook
  (python-mode-hook . jedi:setup-function)
  (python-mode-hook . jedi:ac-setup-function))

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
    :config
    (company-quickhelp-mode 1)
    :custom
    (company-quickhelp-delay 3))

  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :config
    (company-emoji-init))

  (use-package company-shell
    :config
    (add-to-list 'company-backends 'company-shell))

  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :custom
  ;; Except when you're in term-mode/python-mode.
  (company-global-modes '(not term-mode python-mode))
  ;; Give Company a decent default configuration.
  (company-minimum-prefix-length 2)
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
              (when (and (not (equal 'emacs-lisp-mode major-mode)) (not (equal 'solidity-mode major-mode)))
                (flycheck-mode)))))

(use-package flycheck-color-mode-line
  :config
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; Whitespaces
(use-package ethan-wspace
  :ensure t
  :demand t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c c" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

(use-package shrink-whitespace
  :ensure t
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Navigation and thing
(use-package winner
  :config
  (winner-mode 1))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  (("C-," .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   :map goto-map
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
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Projjectile
(use-package projectile
  :ensure t
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  ;; Use C-c C-f to find a file anywhere in the current project.
  ;;  :bind
  ;;  ("C-c C-f" . projectile-find-file)
  :diminish projectile-mode)

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
  :config
  ;; Org-ref
  (use-package org-ref
    :init
    (setq reftex-default-bibliography '("~/Bibliography/refs.bib"))
    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/Bibliography/notes.org"
          org-ref-default-bibliography '("~/Bibliography/refs.bib")
          org-ref-pdf-directory "~/Bibliography/bibtex-pdfs/")
    (setq helm-bibtex-bibliography "~/Bibliography/refs.bib")
    (setq helm-bibtex-library-path "~/Bibliography/bibtex-pdfs/")
    (setq helm-bibtex-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath)))
    :config
    ;; variables that control bibtex key format for auto-generation
    ;; I want firstauthor-year-title-words
    ;; this usually makes a legitimate filename to store pdfs under.
    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5))

  ;; Open PDF with pdfview
  (use-package org-pdfview)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))
  ;; Should normaly support russian
  (setq org-latex-compiler "xelatex")
  ;; Set default inline image width
  (setq org-image-actual-width '(400))
  ;; Stop org-mode from highjacking shift-cursor keys.
  (setq org-replace-disputed-keys t)
  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)
     ;;(set-visual-wrap-column 80)
     (linum-mode -1)))
  ;; Minted
  (require 'ox-latex)
  (setq org-latex-listings 'minted)
  ;; Babel src evaluator
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (restclient . t)))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex $(basename %b)"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :custom
  (org-src-tab-acts-natively t)
  (org-todo-keyword-faces
   '(
     ("TODO" . (:background "red"
                            :foreground "white"
                            :weight bold))
     ("IN-PROGRESS" . (:background "yellow"
                                   :foreground "black"
                                   :weight bold))
     ("SKIP" . (:background "purple"
                            :foreground "white"
                            :weight bold))
     ("DONE" . (:background "green"
                            :foreground "black"
                            :weight bold))))
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "SKIP" "|" "DONE")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))))

(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list '("•"))
  (org-ellipsis "▼")
  :hook
  (org-mode . org-bullets-mode))

(use-package pdf-tools
  :ensure t
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

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


(use-package gist
  :ensure t)

;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

;; Elfeed
(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '(("https://sachachua.com/blog/feed/" s-chua)
                  ("https://www.iwi.hs-karlsruhe.de/iwii/REST/rssfeed/newsbulletinboard/INFB.xml" hska)))
  :bind (("C-x w" . elfeed)))


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

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

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

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;; Lighter theme
;; (use-package silkworm-theme
;;  :ensure t
;;   :config
;;   (load-theme 'silkworm t))

;;(use-package solarized-theme
;;  :ensure t
;;  :config
;;  (load-theme 'solarized-dark t)
;;  :custom
;;  (x-underline-at-descent-line t))

;; Darker theme
;; (use-package nord-theme
;;   :ensure t
;;   :init
;;   (setq nord-uniform-mode-lines t)
;;   ;;  (setq nord-region-highlight "snowstorm")
;;   :config
;;   ;;(load-theme 'nord t)) ;; buggy in daemon mode ;(
;;   ;; Temporary fix for daemon-mode
;;   (if (daemonp)
;;       (cl-labels ((load-nord (frame)
;;                              (with-selected-frame frame
;;                                (load-theme 'nord t))
;;                              (remove-hook 'after-make-frame-functions #'load-nord)))
;;         (add-hook 'after-make-frame-functions #'load-nord))
;;     (load-theme 'nord t)))

;; Engage Nyan Cat!
;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (nyan-mode 1)
;;   :custom
;;   (nyan-bar-length 16)
;;   (nyan-wavy-trail t))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 5)
  :hook (after-init . doom-modeline-mode))

(add-to-list 'default-frame-alist '(alpha . (95 . 75)))
(set-frame-parameter nil 'alpha '(92 . 75))
(display-time-mode 1)

;; Functions
(defun my/calc-ml-time (enum etime-sec)
  "Calculates time in mins and hours, given the number of epochs and time for each epoch in seconds"
  (interactive "nNumber of epochs? \nnHow much time does each epoch take? ")
  (setq time-min (*(/ etime-sec 60) enum))
  (setq time-hours (/ time-min 60))
  (message "Time in min: %s. Time in hours: %s" time-min time-hours ))

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

;; Hack for setting a fixed wrap column in visual-line-mode.
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))
