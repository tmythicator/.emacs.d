;;; init.el --- Bootstrap for literate configuration  -*- lexical-binding: t; -*-

(defvar my/config-org (expand-file-name "README.org" user-emacs-directory))
(defvar my/config-el (expand-file-name "README.el" user-emacs-directory))

(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang-list))

(when (or (not (file-exists-p my/config-el))
          (file-newer-than-file-p my/config-org my/config-el))
  (let ((vc-handled-backends nil))
    (require 'org)
    (require 'ob-tangle)
    (org-babel-tangle-file my/config-org my/config-el)))

(load my/config-el nil 'nomessage)

;;; init.el ends here
