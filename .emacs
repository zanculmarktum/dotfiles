;; Fix "Bad Request" issue.
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Prevent package.el loading packages prior to their init-file loading
(when (version<= "27" emacs-version)
  (setq package-enable-at-startup nil))

(defvar bootstrap-version)
(let* ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5)
       (install-file
        (concat (file-name-directory bootstrap-file) "install.el")))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://github.com/raxod502/straight.el/raw/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp))
    ;;(load install-file nil 'nomessage)
    )
  (load bootstrap-file nil 'nomessage))

(require 'straight-x)

(defun straight-recipes-find (pat &optional print-repo)
  (let* ((find (lambda (repo)
                 (cl-remove-if (lambda (x) (not (string-match-p pat x)))
                               (straight-recipes-list repo))))
         (result (rassq-delete-all
                  nil
                  (cl-mapcar (lambda (x)
                               (cons x (funcall find `(,x))))
                             straight-recipe-repositories))))
    (if print-repo
        result
      (cl-mapcan #'cdr result))))

(defun fetch-file (url filename)
  (fetch-content url filename))

(defun fetch-content (url &optional filename)
  (let ((max-retries 3)
        (enable-local-variables nil)
        (f (lambda (retry)
             (if (= retry max-retries)
                 (error (concat "Fetching failed after " max-retries " retries"))
               (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)
                 ;; Clean up HTTP headers
                 (goto-char (point-min))
                 (if (search-forward-regexp "^$" nil t)
                     (progn
                       (flush-lines ".*" (point-min) (1+ (point)))
                       (if filename
                           (progn ;; Write to a file
                             (let ((dir (file-name-directory filename)))
                               (and dir (make-directory dir t)))
                             (write-file filename))
                         (replace-regexp-in-string
                          "\n+$"
                          ""
                          (buffer-substring-no-properties (point-min) (point-max)))))
                   (funcall f (1- retry))))))))
    (funcall f 0)))

(defun octal-to-int (number)
  "Convert octal to integer.

Example:

   (octal-to-int 644) ⇒ 420"
  (let ((f (lambda (number)
             (let* ((p (floor (log (abs number) 10)))
                    (d (funcall (if (natnump number) #'floor #'ceiling)
                                number (expt 10 p)))
                    (n (- number (* d (expt 10 p)))))
               (if (zerop n)
                   (* d (expt 8 p))
                 (+ (* d (expt 8 p)) (funcall f n)))))))
    (if (zerop number)
        0
      (funcall f number))))
;; Test:
;; (dolist (i (number-sequence -100 100)) (let ((n (string-to-number (format "%o" i)))) (message "%s -> %s" n (octal-to-int n))))

(defun straight-get-files-wildcard (files prefix flavor)
  (let ((f (lambda (files prefix flavor)
             (unless (listp files)
               (error "Invalid :files directive: %S" files))
             (let ((mappings nil)
                   (exclusions nil))
               (while files
                 (let ((spec (car files)))
                   (setq files (cdr files))
                   (cond
                    ((eq spec :defaults)
                     (setq files (append straight-default-files-directive files)))
                    ((stringp spec)
                     (setq files
                           (nconc
                            (list (let ((filename (file-name-nondirectory spec)))
                                    (when (eq flavor 'melpa)
                                      (setq filename
                                            (replace-regexp-in-string
                                             "\\.el\\.in\\'" ".el" filename 'fixedcase)))
                                    (cons spec (concat prefix filename))
                                    ))
                            files)))
                    ((not (consp spec))
                     (error "Invalid entry in :files directive: %S" spec))
                    ((eq (car spec) :exclude)
                     (cl-destructuring-bind
                         (rec-mappings . rec-exclusions)
                         (funcall f (cdr spec) prefix flavor)
                       (setq mappings (cl-remove-if
                                       (lambda (mapping)
                                         (assoc (car mapping) rec-mappings))
                                       mappings))
                       (dolist (mapping rec-mappings)
                         (push (car mapping) exclusions))))
                    ((consp (cdr spec))
                     (unless (stringp (car spec))
                       (error "Invalid sub-list head in :files directive: %S" (car spec)))
                     (cl-destructuring-bind
                         (rec-mappings . rec-exclusions)
                         (funcall f (cdr spec) (concat prefix (car spec) "/") flavor)
                       (setq mappings (cl-remove-if
                                       (lambda (mapping)
                                         (member (car mapping) rec-exclusions))
                                       mappings))
                       (dolist (mapping rec-mappings)
                         (push mapping mappings))
                       (dolist (exclusion rec-exclusions)
                         (push exclusion exclusions))))
                    ((or (not (stringp (car spec)))
                         (not (stringp (cdr spec))))
                     (error "Invalid entry in :files directive: %S" spec))
                    (t
                     (push spec mappings)))))
               (cons (reverse mappings) (reverse exclusions))))))
    (cl-mapcar #'car (car (funcall f files prefix flavor)))))
;; (format "%s" (straight--with-plist (straight--convert-recipe 'stan-snippets) (files flavor) (straight-get-files-wildcard files "" flavor)))

(defun straight--fetch-blobs-caller (f melpa-style-recipe)
  (straight--with-plist (straight--convert-recipe melpa-style-recipe)
      (type host branch repo local-repo files package flavor)
    (unless (file-exists-p (straight--repos-dir local-repo))
      (cl-mapcar #'(lambda (wildcard)
                     (funcall f repo local-repo (or branch "master") nil wildcard)
                     )
                 (cl-mapcar #'listify-wildcard
                            (straight-get-files-wildcard files "" flavor))))))

(setq github-api-token nil)

(defun straight--fetch-github-blobs-internal (repo local-repo sha &optional path wildcard)
  (let* ((first (or (car wildcard) "*"))
         (rest (or (cdr wildcard) '("*")))
         (default-directory (straight--repos-dir local-repo))
         (process-hash (lambda (hash)
                         (let* ((name (gethash "name" hash))
                                (type (gethash "type" hash))
                                (download-url (gethash "download_url" hash))
                                (path (gethash "path" hash))
                                ;;(target (gethash "target" hash))
                                )
                           (make-directory default-directory t)
                           (when (wildcard-match-p first name)
                             (cond ((string= type "file")
                                    (fetch-file download-url path))
                                   ((string= type "symlink")
                                    (let ((target (fetch-content download-url)))
                                      (make-symbolic-link target path)))
                                   ((string= type "dir")
                                    (straight--fetch-github-blobs-internal repo local-repo sha path rest)))))))
         (max-retries 3)
         (f (lambda (retry)
              (cond ((= retry max-retries)
                     (error (concat "Fetching failed after " max-retries " retries")))
                    ((file-name-directory first)
                     (straight--fetch-github-blobs-internal repo local-repo sha first rest))
                    (t
                     (with-current-buffer
                         (url-retrieve-synchronously (concat "https://" (when github-api-token (concat github-api-token "@")) "api.github.com/repos/"
                                                             repo "/contents/" path "?ref=" sha)
                                                     'silent 'inhibit-cookies)
                       ;; Clean up HTTP headers
                       (goto-char (point-min))
                       (if (search-forward-regexp "^$" nil t)
                           (progn
                             (flush-lines ".*" (point-min) (1+ (point)))
                             (let ((json (json-parse-buffer)))
                               (unless (and (hash-table-p json)
                                            (string= (gethash "message" json) "Not Found"))
                                 (cl-mapcar process-hash json))))
                         (funcall f (1- retry)))))))))
    (funcall f 0)))

(defun straight-fetch-github-blobs (melpa-style-recipe)
  (straight--fetch-blobs-caller #'straight--fetch-github-blobs-internal melpa-style-recipe))

(defun straight--fetch-gitlab-blobs-internal (repo local-repo sha &optional path wildcard)
  (let* ((first (or (car wildcard) "*"))
         (rest (or (cdr wildcard) '("*")))
         (pattern "\\(gitlab[^\x0]*\\.com\\)/\\([^/]*\\)/\\([^/.]*\\)\\(?:\\.git\\)?")
         (host (if (string-match pattern repo)
                   (match-string 1 repo)
                 nil))
         (owner (if (string-match pattern repo)
                    (match-string 2 repo)
                  (car (split-string repo "/"))))
         (project (if (string-match pattern repo)
                      (match-string 3 repo)
                    (cadr (split-string repo "/"))))
         (default-directory (straight--repos-dir local-repo))
         (process-hash (lambda (hash)
                         (let* ((name (gethash "name" hash))
                                (type (gethash "type" hash))
                                (id (gethash "id" hash))
                                (path (gethash "path" hash))
                                (download-url (concat "https://" (or host "gitlab.com")
                                                      "/api/v4/projects/" owner "%2F" project
                                                      "/repository/blobs/" id "/raw"))
                                (mode (string-to-number (gethash "mode" hash) 8)))
                           (make-directory default-directory t)
                           (when (wildcard-match-p first name)
                             (cond ((string= type "blob")
                                    (if (= (logand (ash mode -12) 7) 2) ;; symlink?
                                        (let ((target (fetch-content download-url)))
                                          (make-symbolic-link target path))
                                      (fetch-file download-url path)
                                      (set-file-modes path (logand mode (1- (ash 1 12))))))
                                   ((string= type "tree")
                                    (straight--fetch-gitlab-blobs-internal repo local-repo sha path rest)))))))
         (max-retries 3)
         (f (lambda (retry)
              (cond ((= retry max-retries)
                     (error (concat "Fetching failed after " max-retries " retries")))
                    ((file-name-directory first)
                     (straight--fetch-gitlab-blobs-internal repo local-repo sha first rest))
                    (t
                     (with-current-buffer
                         (url-retrieve-synchronously (concat "https://" (or host "gitlab.com")
                                                             "/api/v4/projects/" owner "%2F" project
                                                             "/repository/tree?ref=" sha
                                                             (and path "&path=") path)
                                                     'silent 'inhibit-cookies)
                       ;; Clean up HTTP headers
                       (goto-char (point-min))
                       (if (search-forward-regexp "^$" nil t)
                           (progn
                             (flush-lines ".*" (point-min) (1+ (point)))
                             (let ((json (json-parse-buffer)))
                               (unless (eq json [])
                                 (cl-mapcar process-hash json))))
                         (funcall f (1- retry)))))))))
    (funcall f 0)))

(defun straight-fetch-gitlab-blobs (melpa-style-recipe)
  (straight--fetch-blobs-caller #'straight--fetch-gitlab-blobs-internal melpa-style-recipe))

(defun wildcardp (string)
  (and (stringp string)
       (string-match-p "[[.*+\\^$?]" string)))

(defun wildcard-match-p (wildcard string)
  (string-match-p (wildcard-to-regexp wildcard) string))

(defun listify-wildcard (wildcard)
  (let* ((listify (lambda (filename acc)
                    (let* ((dirpart (file-name-directory filename))
                           (nondir (file-name-nondirectory filename))
                           (first (car acc))
                           (rest (cdr acc))
                           (result (if (or (and (wildcardp nondir) acc) (and first (wildcardp first)))
                                       (cons nondir acc)
                                     (cons (if first
                                               (concat (file-name-as-directory nondir) first)
                                             nondir)
                                           rest))))
                      (if dirpart
                          (funcall listify (directory-file-name dirpart) result)
                        result)))))
    (when (file-name-absolute-p wildcard)
      (error "The wildcard must be relative"))
    (funcall listify wildcard '())))

(defun append-path (new-path &optional add-exec-path)
  "Append to PATH if NEW-PATH doesn't exist in PATH.

Also add to `exec-path' if ADD-EXEC-PATH is non-nil."
  (unless (or (string= new-path "")
              (seq-contains-p (split-string (getenv "PATH") ":") new-path #'string=))
    (setenv "PATH" (concat new-path ":" (getenv "PATH")))
    (when add-exec-path
      (add-to-list 'exec-path new-path))))

(defun straight-list-installed-packages ()
  (let ((packages nil))
    (maphash (lambda (package recipe)
               (when (or (null #'straight--installed-p)
                         (funcall #'straight--installed-p (plist-put recipe :package package)))
                 (push package packages)))
             straight--recipe-cache)
    (nreverse packages)))

(defun straight-update-installed-packages ()
  (dolist (package (straight-list-installed-packages))
    (unless (or
             ;; exclude the following packages
             (string= package "cmake-mode")
             (string= package "erlang"))
      (straight-pull-package package))))

;; Insert numbers from n to m
(defun insert-number (n m)
  (dolist (x (number-sequence n m))
    (insert (concat (int-to-string x) "\n"))))

;; Copy the region into the clipboard using xclip
(defun xclip-region ()
  (interactive)
  (unless (executable-find "xclip")
    (error "xclip not found in exec-path"))
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "xclip -selection clipboard >/dev/null 2>&1"
                           nil
                           nil
                           nil
                           nil))

;; Copy the string into the clipboard using xclip
(defun xclip-string (input)
  (interactive (list (read-shell-command "String input: ")))
  (unless (executable-find "xclip")
    (error "xclip not found in exec-path"))
  (shell-command (concat "printf " (shell-quote-argument input) " | xclip -selection clipboard >/dev/null 2>&1")))

;; Copy the kill ring into the clipboard using xclip
(defun xclip-kill-ring ()
  (interactive)
  (unless (executable-find "xclip")
    (error "xclip not found in exec-path"))
  (shell-command (concat "printf " (shell-quote-argument (current-kill 0)) " | xclip -selection clipboard >/dev/null 2>&1")))

;; Forward region to 0x0.st
(defun 0x0 ()
  (interactive)
  (unless (executable-find "curl")
    (error "curl not found in exec-path"))
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "curl -F file=@- https://0x0.st/"
                           nil
                           nil
                           nil
                           nil))

(defun insert-css ()
  (interactive)
  (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"\">"))

(defun insert-html ()
  (interactive)
  (insert "<!DOCTYPE html>
<html>
    <head>
    </head>

    <body>
    </body>
</html>
"))

(defun insert-form ()
  (interactive)
  (insert "<form class=\"needs-validation was-validated\" method=\"POST\" action=\"/\">
  @csrf

  <div>
    <label for=\"title\" class=\"form-label\">Title</label>
    <input type=\"text\" class=\"form-control\" id=\"title\" name=\"title\" value=\"{{ old(\'title\') }}\" required>
  </div>

  <button type=\"submit\" class=\"btn btn-primary\">Submit</button>
</form>
"))

(defun insert-input ()
  (interactive)
  (insert "<div>
  <label for=\"title\" class=\"form-label\">Title</label>
  <input type=\"text\" class=\"form-control\" id=\"title\" name=\"title\" aria-describedby=\"titleHelp\" value=\"{{ old(\'title\') }}\" required>
  <div id=\"titleHelp\" class=\"form-text\">Lorem ipsum dolor sit amet.</div>
</div>
"))

(straight-use-package 'use-package)

(use-package nord-theme
  :straight t
  :config (load-theme 'nord t))
;; To show currently enabled themes:
;; M-: custom-enabled-themes

(require 'info)
(info-initialize)

(use-package modeline-posn
  :straight t
  :config
  (set-face-attribute 'modelinepos-region nil :inherit 'mode-line)
  (set-face-attribute 'modelinepos-region-acting-on nil :inherit 'mode-line)
  (set-face-attribute 'modelinepos-region-acting-on nil :box 'unspecified))
;; To show current value:
;; M-: (face-attribute 'modelinepos-region :inherit)
;; M-: (face-all-attributes 'modelinepos-region)

;;(when (executable-find "xclip")
;;  (use-package xclip
;;    :straight t
;;    :config (xclip-mode 1)))

(use-package highlight-indent-guides
  :straight t
  :config
  ;;(highlight-indent-guides-mode 1)
  ;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  ;; Don't enable if the size of the file is too big
  (add-hook 'find-file-hook
            #'(lambda ()
                (let ((size (file-attribute-size (file-attributes (buffer-name))))
                      (large-file-warning-threshold 500000))
                  (unless (and large-file-warning-threshold size
                               (> size large-file-warning-threshold))
                    ;;(highlight-indent-guides-mode 1)
                    ))))

  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)

  ;; Change the face in accordance with nord theme
  (when (memq 'nord custom-enabled-themes)
    (if (display-graphic-p)
        (set-face-attribute 'highlight-indent-guides-character-face nil :foreground "#3b4252")
      (set-face-attribute 'highlight-indent-guides-character-face nil :foreground "black"))
    ;;(set-face-attribute 'highlight-indent-guides-character-face nil :foreground (face-attribute 'tty-menu-disabled-face :foreground))
    )

  ;; Delete " h-i-g" indicator from mode line
  (setq minor-mode-alist (assoc-delete-all 'highlight-indent-guides-mode minor-mode-alist)))
;; To show current value
;; M-: (face-attribute 'highlight-indent-guides-character-face :foreground)
;; M-: (face-all-attributes 'highlight-indent-guides-character-face)

(use-package f
  :straight t)
(use-package nix-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (setq nix-indent-function 'nix-indent-line))

(use-package markdown-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package git-modes
  :straight t)

(use-package command-log-mode
  :straight t
  ;; :config (global-command-log-mode)
  )

(use-package haskell-mode
  :straight t
  :config
  ;;(require 'haskell-mode-autoloads)
  ;;(add-to-list 'Info-directory-list
  ;;             (expand-file-name "straight/repos/haskell-mode" user-emacs-directory))
  )

(straight-fetch-gitlab-blobs 'cmake-mode)
(use-package cmake-mode
  :straight t)

(use-package tuareg
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
  (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
                  ".annot" ".cmt" ".cmti"))
    (add-to-list 'completion-ignored-extensions ext))
  (add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode)))

(straight-fetch-github-blobs 'erlang)
(use-package erlang
  :straight t
  :config (require 'erlang-start))

(use-package neotree
  :straight t
  :config
  (global-set-key (kbd "<f8>") 'neotree-toggle)
  (setq ;;neo-smart-open t
        neo-autorefresh nil
        neo-show-hidden-files t
        neo-window-fixed-size nil
        neo-auto-indent-point t)
  (when (display-graphic-p)
    (setq neo-theme 'icons)))

(use-package gnuplot
  :straight t
  :config
  (autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist)))

(use-package selectrum
  :straight t
  :config (selectrum-mode +1))

;;(use-package icomplete-vertical
;;  :straight t
;;  :config
;;  ;; (icomplete-mode)
;;  ;; (icomplete-vertical-mode)
;;  )

(use-package lua-mode
  :straight t
  :config
  (setq lua-indent-level 2
        lua-documentation-url "https://www.lua.org/manual/5.3/manual.html"))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  (setq minor-mode-alist
        (cl-remove-if (lambda (x)
                        (equal (car x) 'editorconfig-mode))
                      minor-mode-alist)))

(use-package rainbow-mode
  :straight t)

(use-package tex
  :straight auctex
  :config
  (setq TeX-view-program-list (append TeX-view-program-list '(("zathura" "zathura -P %(outpage) %o")))
        TeX-view-program-selection (cl-mapcar #'(lambda (x) (if (eq (car x) 'output-pdf) '(output-pdf "zathura") x)) TeX-view-program-selection))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package rust-mode
  :straight t)

(use-package php-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (setq typescript-indent-level 2))

(use-package vue-mode
  :straight t)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package move-text
  :straight t
  :config
  ;;(move-text-default-bindings)
  (global-set-key (kbd "M-n") 'move-text-down)
  (global-set-key (kbd "M-p") 'move-text-up))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

(use-package doom-themes
  :straight t
  ;;:config
  ;;(load-theme 'doom-nord t)
  ;;(doom-themes-neotree-config)
  )

(use-package centaur-tabs
  :straight t
  ;;:config
  ;;(centaur-tabs-mode t)
  ;;(centaur-tabs-change-fonts "Fira Code" 100)
  ;;(centaur-tabs-headline-match)
  ;;(setq centaur-tabs-style "bar"
  ;;      centaur-tabs-enable-ido-completion nil
  ;;      centaur-tabs-set-icons t
  ;;      centaur-tabs-set-modified-marker t)
  ;;(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
  ;;(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
  )

;;(use-package geiser-guile
;;  :straight t)

(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package dockerfile-mode
  :straight t)

(use-package dotenv-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

;; Display
(menu-bar-mode 0)           ;; hides menu bar
(tool-bar-mode 0)           ;; hides tool bar
(blink-cursor-mode 0)       ;; disables cursor blinking
(scroll-bar-mode 0)         ;; disables scroll bar

;; Frames
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))
;;(setq default-frame-alist (cl-remove-if
;;                           #'(lambda (x) (eq (car x) 'font))
;;                           default-frame-alist))
;;(add-to-list 'default-frame-alist '(alpha . 75)) ;; transparency

;; Mode Line
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Set backup locations
(setq backup-directory-alist
      `((".*" . ,(locate-user-emacs-file "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "backups/") t)))
(customize-set-variable
 'tramp-backup-directory-alist backup-directory-alist)

;; Highlight parentheses
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
            (not (member major-mode '(help-mode Info-mode ibuffer-mode dired-mode occur-mode neotree-mode)))
            (not (minibufferp)))
           (display-line-numbers-mode)))
    (global-display-line-numbers-mode)))

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
(fringe-mode 0)

;; Scroll one line at a time
(setq scroll-step 1)

;; Hide all buffers with an asterisk in ibuffer
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Prevent ‘M-/’ looking in other buffers
(setq dabbrev-check-other-buffers nil)

;; Enable upper case and lower case conversion commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Move point to the bottom line when reaching end of buffer
(setq scroll-error-top-bottom t)

;; Preserve point position when scrolling
(setq scroll-preserve-screen-position t)

;; Rebind ‘M-DEL’ to its original function in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (local-unset-key (kbd "M-DEL"))))

;; Don't save auth info to ~/.authinfo
(setq auth-source-save-behavior nil)

;; Treat alacritty like screen
;;(add-to-list 'term-file-aliases '("alacritty" . "screen"))
;;(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

;; Set CC Mode indentation
;;(add-hook 'c-mode-common-hook (lambda ()
;;                                (setq c-basic-offset 4)))

;; Set sh-script mode indentation
;;(add-hook 'sh-mode-hook (lambda ()
;;                          (setq sh-basic-offset 2)))

;; Fix TRAMP hanging
(setq tramp-shell-prompt-pattern (concat "\\(?:^\\|\r\\)"
                                         "[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"))

;; Case-insensitive file name completion
(setq read-file-name-completion-ignore-case t)

;; Case-insensitive buffer completion
(setq read-buffer-completion-ignore-case t)

;; Use the primary selection for kill and yank commands.
;; To access the clipboard:
;;   clipboard-kill-region
;;   clipboard-kill-ring-save
;;   clipboard-yank
;;(setq select-enable-clipboard nil
;;      select-enable-primary t
;;      mouse-drag-copy-region t)

;; Minibuffer completion at point
;;(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
;;(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
;;(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
;;(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)

;; Save minibuffer history
(savehist-mode 1)

;; Do not convert tag names to words in Customize, i.e. foo-bar-baz -> "Foo Bar Baz"
(setq custom-unlispify-tag-names nil)

;; Truncate long buffer name in mode line
(setq-default mode-line-buffer-identification (append '(-20) (propertized-buffer-identification "%12b")))

;; Set bash history size
(setenv "HISTSIZE" "100000")
(setenv "HISTFILESIZE" (getenv "HISTSIZE"))

;; NFO viewer
;; (revert-buffer-with-coding-system 'cp437)

;; Set JavaScript mode indentation
(setq js-indent-level 2)

;; Use fundamental-mode if the line is too overly long
(add-hook 'find-file-hook
          #'(lambda ()
              (unless (or (= (point-max) 1)
                          (string-match-p "\n" (buffer-substring-no-properties
                                                (point-min)
                                                (if (> (point-max) 1000) 1000 (point-max)))))
                (fundamental-mode)
                (setq truncate-lines nil)
                (highlight-indent-guides-mode 0))))

;; Use base-10 for ‘C-q’ (‘quoted-insert’)
(setq read-quoted-char-radix 10)

;; Set the browser
(setenv "BROWSER" "chromium")

;; Set the editor
(setenv "EDITOR" "emacsclient -nw")

;; Set CSS mode indent offset
(setq css-indent-offset 2)

;; Set PATH
(append-path (expand-file-name "~/.config/composer/vendor/bin") t)
(append-path (expand-file-name "~/.node_modules/bin") t)
(append-path (expand-file-name "~/.cabal/bin") t)
(append-path (expand-file-name "~/.local/bin") t)

;; Kill an entire line without having to C-a first
(global-set-key (kbd "C-M-k") #'(lambda ()
                                  (interactive)
                                  (move-beginning-of-line 1)
                                  (kill-line)))

(setq tab-stop-list ((lambda (n)
                       (let ((i n)
                             (result nil))
                         (while (<= i 10)
                           (setq result (append result (list i)))
                           (setq i (+ n i)))
                         result)) 2))

(setq sql-mariadb-options '("--default-character-set=utf8mb4"))

;; Disable blinking cursor in terminal
(setq visible-cursor nil)

;; Don't show *Warnings* buffer
(setq native-comp-async-report-warnings-errors 'silent)
