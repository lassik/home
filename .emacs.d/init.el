;;; Personal Emacs initialization file

;; The following directory layout is expected:

;; ~/public/src/emacs/   -- non-standard elisp files downloaded from the internet
;; ~/private/src/emacs/  -- non-standard elisp files I'm editing

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Find out who we are

(defconst my-mac-p (equal 'darwin system-type))
(defconst my-windows-p (equal 'windows-nt system-type))
(defconst my-unix-p (not my-windows-p))

;;; Find out where our stuff is

(defconst my-emacs-conf-dir
  (expand-file-name (file-name-directory load-file-name)))

(defconst my-emacs-public-code-dir
  (expand-file-name "~/persist/public/src/emacs/"))

(defconst my-emacs-private-code-dir
  (expand-file-name "~/persist/private/src/emacs/"))

;;; Load extensions bundled with Emacs

;; We load all extensions immediately for two reasons:

;; 1) To avoid the byte-compiler warnings about undefined variables
;; that configuring an unloaded extension would generate.

;; 2) To eliminate the load time that would otherwise be incurred upon
;; the first use of a load-on-demand extension.

(require 'cl)                           ; For great justice.
(require 'comint)
(require 'dired)

;;; Load extensions not bundled with Emacs

(dolist (code-dir (list my-emacs-public-code-dir
                        my-emacs-private-code-dir))

  (when (file-accessible-directory-p code-dir)

    ;; Kill all byte-compiled .elc files; they are a nuisance. In
    ;; addition to cluttering directory listings they may be out of
    ;; date or made by an incompatible Emacs, and in such cases `load'
    ;; won't do the right thing, which is to resort to loading the
    ;; corresponding .el file instead.  Note that the `delete-file'
    ;; call below deletes even such .elc files that have no
    ;; corresponding .el file. This is not an issue for me since I
    ;; always keep the .el files.
    (mapc #'delete-file (directory-files code-dir t "\\.elc$"))

    ;; Before loading anything, add the extension directory to
    ;; `load-path' to make sure the extensions can find each other in
    ;; case one of them `require's another. This is necessary because
    ;; we don't make an effort to load the extensions in dependency
    ;; order; we delegate all dependency-chasing work to the Emacs
    ;; Lisp `require'/`provide' mechanism which causes recursive
    ;; `load' calls. Also, we use `pushnew' instead of plain `push' to
    ;; prevent multiple `load-path' entries for the extension
    ;; directory in case this init file is loaded multiple times in
    ;; one Emacs session.
    (pushnew code-dir load-path :test #'equal)

    ;; At last, load the extensions.
    (mapc #'load-file (directory-files code-dir t "\\.el$"))))

(load-file (concat my-emacs-conf-dir "subr-lassi.el"))

(when (require 'exec-path-from-shell nil t)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LC_ALL"))

;; Magit needs git fron PATH, so postpone it until here.
(require 'magit nil t)

;;; Internationalization and localization

;; Default to UTF-8 with Unix newlines everywhere. The present setup
;; routine is a mess uncovered through much trial and error, but
;; hopefully at some point down the line it will become easier as
;; defaults begin to reflect the brave new world of Unicode.

(prefer-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;;; Unfasten some seatbelts

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(set-variable 'initial-scratch-message "")

;; Make Emacs startup with a blank scratch buffer instead of a splash
;; screen.
(set-variable 'inhibit-splash-screen t)
(set-variable 'inhibit-startup-echo-area-message (user-login-name))

;; Prevent the computer from beeping and flashing at me.
(defun my-dummy-ring-bell-function () (interactive))
(set-variable 'ring-bell-function 'my-dummy-ring-bell-function)

;; Set a frame title that I can use to distinguish one Emacs from
;; another. Having the name of the computer in the frame title is
;; useful because I might operate a remote GUI emacs using SSH X
;; forwarding.
(setq frame-title-format
      (concat "%b"
              " " (downcase (substring (system-name) 0 (position ?. (system-name))))
              " " (downcase (replace-regexp-in-string "[^A-Za-z0-9_-].*$" "" invocation-name))))

;; The remainder of this file consists entirely of custamizations that
;; the Emacs `customize' facility can't yet handle. `customize' is
;; awesome.

;;; Custom hooks

(defun my-case-fold-hook ()
  (setq case-fold-search t)
  (setq sort-fold-case t))

;;; Custom key bindings

;; In some configurations the minibuffer editing cursor can go into
;; the prompt (which is pretty useless and is triggered accidentally
;; most of the time), sometimes it's restricted to the user input
;; area (which is what I want 100% of the time).  No matter which is
;; the case, skip the prompt and find the start of user input.
(defun minibuffer-input-start ()
  (or (unless (text-properties-at (point-min)) (point-min))
      (next-property-change (point-min))
      (point-max)))

;; Binding this extremely useful command to Control-Backspace in the
;; minibuffer keymap allows you to backspace text one pathname
;; component at a time instead of creeping along one character at a
;; time, all the while keeping the kill ring intact unlike
;; `backward-kill-word' would.
(defun my-minibuffer-backspace ()
  (interactive)
  (let ((min (minibuffer-input-start)))
    (let ((end (point)))
      (symbol-macrolet ((delim (case (char-before) ((?/ ?\\) t))))
        (while (and (< min (point)) delim) (backward-char))
        (while (and (< min (point)) (not delim)) (backward-char))
        (when (< (point) end) (delete-region (point) end))))))

;; This command replaces the directory listing in the current `dired'
;; buffer with a listing of its parent directory.
(defun my-dired-up ()
  (interactive)
  (find-alternate-file (my-parent-dir default-directory)))

(defun my-parent-dir (dir)
  (file-name-directory
   (directory-file-name
    (file-name-as-directory
     (expand-file-name dir)))))

(defun my-dired-run ()
  (interactive)
  ;; TODO: (dired-get-filename)
  (call-interactively 'dired-advertised-find-file))

(define-key (current-global-map) [(control h) (return)] 'manual-entry)
(define-key (current-global-map) [(control t)] nil) ; ratpoison prefix key
(define-key (current-global-map) [(control x) (control d)] 'dired)
(define-key (current-global-map) [(meta c)] 'capitalize-region-or-word)
(define-key (current-global-map) [(meta l)] 'downcase-region-or-word)
(define-key (current-global-map) [(meta u)] 'upcase-region-or-word)
(define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
(define-key dired-mode-map [(B)] 'my-dired-up)
(define-key dired-mode-map [(control return)] 'dired-find-alternate-file)
(define-key dired-mode-map [(shift return)] 'dired-find-alternate-file)
(define-key dired-mode-map [return] 'my-dired-run)
(define-key minibuffer-local-completion-map [space] 'self-insert-command)
(define-key minibuffer-local-map [(control backspace)] 'my-minibuffer-backspace)
(define-key minibuffer-local-map [(control n)] 'next-history-element)
(define-key minibuffer-local-map [(control p)] 'previous-history-element)
(define-key minibuffer-local-map [(space)] 'self-insert-command)

(eval-after-load "picture"
  '(progn
     (define-key picture-mode-map [(control c) down]  'picture-movement-down)
     (define-key picture-mode-map [(control c) left]  'picture-movement-left)
     (define-key picture-mode-map [(control c) right] 'picture-movement-right)
     (define-key picture-mode-map [(control c) up]    'picture-movement-up)))

(eval-after-load "ruby-mode"
  '(define-key ruby-mode-map [(meta backspace)] 'backward-kill-word))

(eval-after-load "tex-mode"
  '(define-key tex-mode-map [?\"] 'self-insert-command))

(when (featurep 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Custom Lisp indentation

;; I'm not sure this is the right way to do it, but it seems to work.

(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

(defun set-lisp-indent (symbol value)
  (dolist (prop '(lisp-indent-function common-lisp-indent-function) value)
    (if (equal 0 value) (remprop symbol prop) (put symbol prop value))))

(set-lisp-indent 'asm 1)
(set-lisp-indent 'casequal 1)
(set-lisp-indent 'dcase 1)
(set-lisp-indent 'defconst 1)           ; Emacs Lisp
(set-lisp-indent 'defface 1)            ; Emacs Lisp
(set-lisp-indent 'define 1)             ; Scheme
(set-lisp-indent 'define-derived-mode 3) ; Emacs Lisp
(set-lisp-indent 'define-modify-macro 0)
(set-lisp-indent 'define-symbol-macro 1)
(set-lisp-indent 'defpackage 1)         ; Common Lisp
(set-lisp-indent 'defsubstrate 1)
(set-lisp-indent 'ecasequal 1)
(set-lisp-indent 'edcase 1)
(set-lisp-indent 'event-case 1)
(set-lisp-indent 'letrec 2)
(set-lisp-indent 'loop 0)
(set-lisp-indent 'prog1 0)
(set-lisp-indent 'until 1)
(set-lisp-indent 'when-output 1)
(set-lisp-indent 'whilet 2)
(set-lisp-indent 'with-output-to-string 0)
(set-lisp-indent :section 1)

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-clojure-indent
              (match 1))))

;;; Pathnames

(setq ecumenist-db (concat my-emacs-conf-dir "ecumenist"))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . nasm-mode))

(dolist (alist '(auto-mode-alist magic-mode-alist))
  (when (boundp alist)
    (dolist (entry (symbol-value alist))
      (when (equal (cdr entry) 'html-helper-mode)
        (setf (cdr entry) 'html-mode)))))

;;; Find the right `customize' data file and load it

(setq custom-file
      (concat my-emacs-conf-dir
              "custom-gnuemacs-"
              (format "%d" emacs-major-version)
              "-"
              (cond
               (my-mac-p     "mac")
               (my-windows-p "win")
               (t            "unix"))
              ".el"))

;; If you try to load your customizations by simply feeding your
;; newly-set `custom-file' to `load-file' in this initialization file,
;; some of the customizations won't have any effect because
;; who-knows-what will override them.  EmacsWiki folk wisdom says
;; `window-setup-hook' and `term-setup-hook' are the right places to
;; load them.  I don't understand the bizarre complications of the
;; Emacs startup process so I'm going with folk wisdom.  It seems to
;; work for me.
(add-hook 'window-setup-hook
          (lambda ()
            (when (file-exists-p custom-file)
              (load-file custom-file)
              (set-variable 'dired-use-ls-dired nil)
              (require 'ess)
              (require 'ess-site)
              nil)))
