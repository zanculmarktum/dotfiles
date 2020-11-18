;; Fix "Bad Request" issue.
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Load packages in ~/.emacs.d/packages/
(dolist (x `(;;"evil" "undo-tree" "goto-chg"
             ;;"evil-collection" "annalist"
             ,(when (executable-find "xclip") "xclip")
             "nix-mode"
             "markdown-mode"
             "highlight-indentation"
             "indent-guide"
             "powerline"
             "yaml-mode"
             "haskell-mode"
             ;;"word-count-mode"
             ;;"modeline-posn"
             ;;"visual-regexp"
             ;;"visual-regexp-steroids"
             ;;"mic-paren"
             "highlight-indent-guides"
             "git-modes"
             ;;"keycast"
             ;;"keyfreq"
             "command-log-mode"
             "smartparens" "dash.el"
             ))
  (add-to-list 'load-path (locate-user-emacs-file (concat "packages/" x))))

(require 'info)
(info-initialize)

;;(require 'powerline)
;;(powerline-default-theme)

;;(require 'word-count)

;;(require 'modeline-posn)

;;(require 'visual-regexp)

;;(require 'visual-regexp-steroids)

;;(require 'mic-paren)
;;(paren-activate)

;;(when (executable-find "xclip")
;;  (require 'xclip)
;;  (xclip-mode 1))

;;(require 'undo-tree)
;;(global-undo-tree-mode)

;;(setq evil-want-keybinding nil)
;;(setq evil-want-C-u-scroll t)
;;(require 'evil)
;;;;(evil-set-initial-state 'ibuffer-mode 'normal)
;;;;(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
;;;;(evil-set-initial-state 'dired-mode 'emacs)
;;;;(evil-set-initial-state 'sunrise-mode 'emacs)
;;;;(evil-set-initial-state 'help-mode 'emacs)
;;;;(evil-set-initial-state 'Info-mode 'emacs)
;;(evil-mode 1)

;;(require 'evil-collection)
;;(evil-collection-init)

(require 'highlight-indent-guides)
;;(highlight-indent-guides-mode 1)
;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-foreground 'highlight-indent-guides-character-face "blue")

;;(require 'highlight-indentation)
;;(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;;(dolist (x '(nix-mode-hook emacs-lisp-mode-hook))
;;  (add-hook x (lambda ()
;;                (make-local-variable 'highlight-indentation-offset)
;;                (setq highlight-indentation-offset 2))))

;;(require 'indent-guide)
;;(indent-guide-global-mode)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'git-modes)

(require 'command-log-mode)
;;(global-command-log-mode)

;;(require 'haskell-mode)
(require 'haskell-mode-autoloads)
(add-to-list 'Info-directory-list "~/.emacs.d/packages/haskell-mode")

;;(require 'dash)
;;(require 'smartparens-config)

;; Load themes in ~/.emacs.d/themes/
(dolist (x `("nord-emacs"
             ))
  (add-to-list 'custom-theme-load-path (locate-user-emacs-file (concat "themes/" x))))

(load-theme 'nord t)

;; Display
(menu-bar-mode 0)           ;; hides menu bar
(tool-bar-mode 0)           ;; hides tool bar
(blink-cursor-mode 0)       ;; disables cursor blinking
(scroll-bar-mode 0)         ;; disables scroll bar

;; Frames
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))
;;(add-to-list 'default-frame-alist '(alpha . 75)) ;; transparency

;; Set backup locations
(setq backup-directory-alist
      `((".*" . ,(locate-user-emacs-file "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "backups/") t)))
(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)
;;(setq blink-matching-delay 0)

;; Enable line numbers
(when (version<= "26.0.50" emacs-version)
  (progn
    (require 'display-line-numbers)
    (defun display-line-numbers--turn-on ()
       "Turn on line numbers but excempting certain majore modes."
       (if (and
            (not (member major-mode '(help-mode Info-mode ibuffer-mode occur-mode)))
            (not (minibufferp)))
           (display-line-numbers-mode)))
    (global-display-line-numbers-mode)
    (setq column-number-mode t)))

;; Disregard read-only-mode
;;(setq inhibit-read-only t)

;; Disable large file warning
;;(setq large-file-warning-threshold nil)

;; Remember point position
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t)
      (setq save-place-file (locate-user-emacs-file "places")))
  (save-place-mode 1))

;; File completions
(global-set-key (kbd "C-c C-f")
                'comint-dynamic-complete-filename
                ;;'comint-replace-by-expanded-filename
                )

;; Don't insert tabs
(setq-default indent-tabs-mode nil)

;; Disable line wrap
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 'truncation 32)
;;(fringe-mode 1)

;; Scroll one line at a time
(setq scroll-step 1)

;; Hide all buffers with an asterisk in ibuffer
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Prevent M-/ look in other buffers
(setq dabbrev-check-other-buffers nil)

;; Enable upper case and lower case conversion commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Move point to the bottom line when reaching end of buffer
(setq scroll-error-top-bottom t)

;; Preserve point position when scrolling
(setq scroll-preserve-screen-position t)

;; Rebind M-DEL to its original function in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (local-unset-key (kbd "M-DEL"))))

;; Don't save auth info to ~/.authinfo
(setq auth-source-save-behavior nil)

;; Treat alacritty like screen
;;(add-to-list 'term-file-aliases '("alacritty" . "screen"))

;; Set CC Mode indentation
(add-hook 'c-mode-common-hook (lambda ()
                                (setq c-basic-offset 4)))
