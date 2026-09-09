;;; early-init.el --- Early Initialization  -*- lexical-binding: t; -*-

;; Fast startup optimizations
(setq gc-cons-threshold most-positive-fixnum)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

;; Frame visual settings
(unless (eq system-type 'android)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars . nil) default-frame-alist)
  (push '(horizontal-scroll-bars . nil) default-frame-alist))

;; Fonts
(push '(font . "JetBrainsMono Nerd Font-11") default-frame-alist)

;; Fallback fontset for full PUA Nerd Font range
(defun my/setup-nerd-fontset (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (dolist (range '((#xe000 . #xf8ff)
                       (#xf0000 . #xffffd)
                       (#x100000 . #x10fffd)))
        (set-fontset-font t range "Symbols Nerd Font Mono" nil 'prepend)))))

(add-hook 'after-make-frame-functions #'my/setup-nerd-fontset)

;; Transparency
(push '(alpha-background . 95) default-frame-alist)

(when (eq system-type 'gnu/linux)
  (push '(undecorated . t) default-frame-alist))

;; UI inhibitors
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(provide 'early-init)
;;; early-init.el ends here
