;;; Bootstrap
;; bootstrap straigt.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Core setup 
;; install use-package and use it by default for straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 1)
(setq inhibit-startup-screen t)
(setq scroll-preserve-screen-position 'always)
(add-to-list 'default-frame-alist
                       '(font . "JetBrains Mono-10"))

(use-package outshine)
(use-package magit)
