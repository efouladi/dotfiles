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

;; install use-package and use it by default for straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Always follow symlinks. init files are normally stowed/symlinked.
(setq vc-follow-symlinks t
      find-file-visit-truename t
      ;; Avoid stale compiled code shadow newer source code
      load-prefer-newer t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Basic Setup
;;;; Emacs folder setup

;; Define configuration directory. Emacs supports XDG_CONFIG_HOME since v27
(setq my-emacs-conf-directory (expand-file-name "~/.config/emacs"))

;;;; General settings
(setq-default ;; Use setq-default to define global default
 ;; Who I am
 user-mail-address "shafo@posteo.net"
 user-full-name "Shayan Fouladi"
 ;; Enable all disabled commands
 disabled-command-function nil
 ;; Enable recursive minibuffer edit
 enable-recursive-minibuffers t
 ;; Don't show scratch message and remove echo area message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message 'nil
 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ; don't use tabs to indent
 tab-width 8 ; but maintain correct appearance
 ;; Use one space as sentence end
 sentence-end-double-space 'nil
 ;; Don't adjust window-vscroll to view tall lines.
 auto-window-vscroll nil

 ;; TODO Don't create lockfiles.
 ;; recentf frequently prompts for confirmation.
 ;;create-lockfiles nil
 ;; Leave some rooms when recentering to top, useful in emacs ipython notebook.
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show column number
 column-number-mode t
 ;; More message logs
 message-log-max 16384
 ;; Don't prompt up file dialog when click with mouse
 use-file-dialog nil
 ;; Place all auto-save files in /tmp directory and make them unique.
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; Remember my location when the file is last opened
 ;; activate it for all buffers
 save-place t
 ;; smooth scrolling
 scroll-conservatively 101
 ;; Reserve one line when scrolling
 scroll-margin 1
 ;; turn off the bell
 ring-bell-function 'ignore
 ;; Smoother scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 )

;; Misc
(set-frame-name "emacs")
(fringe-mode '(1 . 3))
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Let C-g works when cursor is in buffers other than minibuffer
;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))
(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

;; Don't add custom section directly under init.el.
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; Continue poping the mark when repeating C-SPC 
(setq-default set-mark-command-repeat-pop t)
;; When popping the mark, continue popping until the cursor actually
;; moves Also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(add-to-list 'default-frame-alist
                       '(font . "JetBrains Mono-10"))

;;; Window and Buffer management

(use-package windmove
  :straight nil
  :bind (("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-h" . windmove-left)
         ("s-l" . windmove-right))
  )

(use-package window
  ;; Handier movement over default window.el
  :straight nil
  :bind (
         ("C-x 2"             . split-window-below-and-move-there)
         ("C-x 3"             . split-window-right-and-move-there)
         ("C-x \\"            . toggle-window-split)
         ("C-0"               . delete-window)
         ("C-1"               . delete-other-windows)
         ("C-2"               . split-window-below-and-move-there)
         ("C-3"               . split-window-right-and-move-there)
         )
  :init
  ;; Functions for easier navigation
  (defun split-window-below-and-move-there ()
    (interactive)
    (split-window-below)
    (windmove-down))

  (defun split-window-right-and-move-there ()
    (interactive)
    (split-window-right)
    (windmove-right))

  (defun toggle-window-split ()
    "When there are two windows, toggle between vertical and
horizontal mode."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  )

(use-package ace-window
  :defer 3
  :bind (("<C-return>" . ace-window))
  :custom-face (aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 1.0))))
  :config
  (setq
   ;; Home row is more convenient. Use home row keys that prioritize fingers that don't move.
   aw-keys '(?j ?k ?l ?m ?n ?o ?g)
   aw-scope 'visible)
  )

(use-package winner
  ;; Enable window restoration
  :defer 1
  :config
  (winner-mode 1))

(setq
 ;; Kill a frame when quitting its only window
 frame-auto-hide-function 'delete-frame
 ;; Maximum number of side-windows to create on (left top right bottom)
 window-sides-slots '(0 1 2 2)
 ;; Default rules
 display-buffer-alist
 `(;; Right side for most Help, Agenda, Trace, etc buffers
   ("*\\(Help\\|help\\|Man.*\\|trace-\\|Backtrace\\|RefTeX.*\\|ess-describe\\|SDCV.*\\| Merriam.*\\)"
    (display-buffer-reuse-mode-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . right)
    (slot . 1)
    (window-width . 80)
    (window-height . 0.7)
    (reusable-frames . visible))
   ;; Same window
   ("*\\(R.*\\|Python\\)"
    (display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . visible))
   ;; Show on bottom
   ("*\\(ielm\\)"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 10)
    (reusable-frames . visible))
   ("^\\vterm"
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (slot . 2)
    (window-width . 80)
    (reusable-frames . visible))
   ;; Always show notmuch in new frame
   ("^\\*info"
    (display-buffer-reuse-window display-buffer-in-previous-window))
   ;; Display *BBDB* buffer on the bottom frame
   ("\\*BBDB"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 10)
    (reusable-frames . visible))
   ;; Split shells at the bottom
   ("^\\*e?shell"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
    (window-min-height . 20)
    (reusable-frames . visible)
    )
   )
 )

(use-package nswbuff
  ;; Quickly switching buffers. Quite useful!
  :bind (("<C-tab>"           . nswbuff-switch-to-next-buffer)
         ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  :config
  (setq nswbuff-display-intermediate-buffers t)
  )

;;; Terminal Support

(use-package vterm
  :defer 3
  ;; Don't let whole-line-or-region shadows the C-y
  :config
  (defun create-or-switch-to-vterm ()
    "Switch to default `vterm' buffer.
      Start `vterm' if it's not already running."
    (interactive)
    (pop-to-buffer "vterm" nil t)
    (if (not (equal major-mode 'vterm-mode))
        (vterm-mode)))
  :hook (vterm-mode . (lambda () (whole-line-or-region-local-mode -1)))
  :bind (("C-x C-z" . create-or-switch-to-vterm)
         :map vterm-mode-map
         ("C-y"  . vterm-yank)
         ("<f5>" . nil)
         ("<f6>" . nil)
         ("<f7>" . nil)
         ("<f8>" . nil)
         ("<f9>" . nil)
         ("<f10>" . nil)
         ("<f11>" . nil)
         ("<f12>" . nil)))

;;; Completion Framework: Ivy / Swiper / Counsel
(use-package counsel
  :demand t
  :straight ivy-hydra
  :straight ivy-rich
  :straight counsel-projectile
  :straight ivy-posframe
  :straight smex
  :straight (flx :repo "lewang/flx" :host github :files ("flx.el"))
  :bind (("C-s"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("C-h V"   . counsel-set-variable)
         ("C-h l"   . counsel-find-library)
         ("C-h u"   . counsel-unicode-char)
         ("C-c j"   . counsel-git-grep)
         ("C-c i"   . counsel-imenu)
         ("C-x l"   . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ;; Search-replace with ag and rg:
         ;; C-u prefix to choose search directory
         ;; C-c C-o opens an occur buffer
         ;; e to toggle writable state
         ("C-c C-s" . counsel-ag)
         ("C-c r"   . counsel-rg)
         ("C-c f"   . counsel-file-jump) ;; Jump to a file below the current directory.
         ("C-x C-b" . counsel-switch-buffer)
         )
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 50))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))   ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file
  :config
  (ivy-mode 1)
  (ivy-rich-mode 1)
  (counsel-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (counsel-projectile-mode 1)
  (setq ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-use-virtual-buffers nil ;; don't show recent files/bookmarks as buffers in C-x b
        ivy-use-selectable-prompt t ;; C-M-j to rename similar filenames
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-count-format "(%d/%d) "
        ;; Useful settings for long action lists
        ;; See https://github.com/tmalsburg/helm-bibtex/issues/275#issuecomment-452572909
        max-mini-window-height 0.30
        ;; Don't parse remote files, it's slow
        ivy-rich-parse-remote-buffer 'nil)

  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  )

;;;; Extra packages
(use-package outshine)
(use-package magit)
