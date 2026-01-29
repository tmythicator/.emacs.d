;;; early-init.el --- Early Initialization  -*- lexical-binding: t; -*-
(setq gc-cons-threshold most-positive-fixnum)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(alpha . (95 . 75)) default-frame-alist)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(provide 'early-init)