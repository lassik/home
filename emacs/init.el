;;; Personal Emacs initialization file

;; Lassi Kortela <lassi@lassikortela.net>

;; To use, append the following line of code to the file that the
;; variable `user-init-file' on a vanilla emacs points to:

;; (load "~/path/to/this/init.el")

;; The following directory layout is expected:

;; public/code/emacs/  -- non-standard elisp files downloaded from the internet
;; private/code/emacs/ -- non-standard elisp files I'm editing
;; private/conf/emacs/ -- personal configuration files for emacs including this file

;; I use this on the following platforms.

;; Emacs             Operating system    GUI
;; ----------------  ----------------    ---
;; Aquamacs 2.4      Mac OS X 10.6       Cocoa
;; GNU Emacs 23.1.1  Ubuntu Linux 10.04  GTK
;; GNU Emacs 22.3.1  Ubuntu Linux 11.10  GTK
;; GNU Emacs 23.4.1  Windows XP          Windows
;; XEmacs 21.5.31    Windows XP          Windows

;;; Find out where our stuff is

(defconst my-emacs-conf-dir
  (expand-file-name (file-name-directory load-file-name)))

(defconst my-emacs-public-code-dir
  (expand-file-name (concat my-emacs-conf-dir "../../../public/code/emacs/")))

(defconst my-emacs-private-code-dir
  (expand-file-name (concat my-emacs-conf-dir "../../../private/code/emacs/")))

;;; Load extensions bundled with Emacs

;; We load all extensions immediately for two reasons:

;; 1) To avoid the byte-compiler warnings about undefined variables
;; that configuring an unloaded extension would generate.

;; 2) To eliminate the load time that would otherwise be incurred upon
;; the first use of a load-on-demand extension.

(require 'cl)                           ; For great justice.

(require 'cc-mode)
(require 'css-mode)
(require 'dired)
(require 'picture)
(require 'ruby-mode)
(require 'tex-mode)

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

;;; Internationalization and localization

;; Default to UTF-8 with Unix newlines everywhere. The present setup
;; routine is a mess uncovered through much trial and error, but
;; hopefully at some point down the line it will become easier as
;; defaults begin to reflect the brave new world of Unicode.

(whenhost (unix xemacs)
  (require 'un-define)
  (set-coding-priority-list '(utf-8))
  (set-coding-category-system 'utf-8 'utf-8))

(prefer-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(whenhost xemacs
  (set-default-buffer-file-coding-system 'utf-8-unix)
  (set-coding-category-system 'utf-8 'utf-8-unix)
  (set-coding-priority-list '(utf-8))
  (set-language-environment "Latin-1"))

;;; Unfasten some seatbelts

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(set-variable 'initial-scratch-message "")

;; Make Emacs startup with a blank scratch buffer instead of a splash
;; screen.
(whenhost gnuemacs
  (set-variable 'inhibit-splash-screen t)
  (set-variable 'inhibit-startup-echo-area-message (user-login-name)))

;; Prevent the computer from beeping and flashing at me.
(whenhost gnuemacs
  (defun my-dummy-ring-bell-function () (interactive))
  (set-variable 'ring-bell-function 'my-dummy-ring-bell-function))

;; Set a frame title that I can use to distinguish one Emacs from
;; another. Having the name of the computer in the frame title is
;; useful because I might operate a remote GUI emacs using SSH X
;; forwarding.
(setq frame-title-format
      (concat "%b"
              " " (downcase (substring (system-name) 0 (position ?. (system-name))))
              " " (downcase (replace-regexp-in-string "[^A-Za-z0-9_-].*$" "" invocation-name))))

(whenhost xemacs
  (set-variable 'menubar-visible-p nil))

;;; Operating system specific fixes

;; This is appended by default to grep command lines and seems plain
;; and simply set wrong in the default configuration: it points to the
;; file "/dev/null" that exists only under Unix and Cygwin.
(whenhost (xemacs windows)
  (set-variable 'grep-null-device ""))

;; Make Aquamacs have classic Emacs behavior instead of Mac OS X style
;; behavior in many respects.  The most important changes are that 1)
;; the Mac keyboard's Command key works as the Emacs Meta key in
;; commands such as M-x and M-w, which puts much less strain on my
;; fingers; and 2) new frames, called windows in standard GUI lingo,
;; are never auto-created.  This code was copied over from Emacswiki.
(whenhost aquamacs
  (osx-key-mode -1)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier nil)
  (setq ns-use-mac-modifier-symbols nil)
  (setq aquamacs-scratch-file nil)
  (setq initial-major-mode 'emacs-lisp-mode)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (tabbar-mode -1)
  (one-buffer-one-frame-mode -1)
  (setq special-display-regexps nil)
  (aquamacs-autoface-mode -1)
  (unless window-system
    (normal-erase-is-backspace-mode nil)
    (set-face-inverse-video-p 'mode-line-inactive t)))

;; The remainder of this file consists entirely of custamizations that
;; the Emacs `customize' facility can't yet handle. `customize' is
;; awesome.

;;; Custom hooks

(defun my-case-fold-hook ()
  (setq case-fold-search t)
  (setq sort-fold-case t))

(whenhost xemacs
  (add-hook 'dired-mode-hook 'my-case-fold-hook))

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
(define-key (current-global-map) [(control x) b] 'iswitchb-buffer)
(define-key (current-global-map) [(meta c)] 'capitalize-region-or-word)
(define-key (current-global-map) [(meta l)] 'downcase-region-or-word)
(define-key (current-global-map) [(meta u)] 'upcase-region-or-word)
(define-key c++-mode-map [(control c) (control c)] 'compile)
(define-key c-mode-map [(control c) (control c)] 'compile)
(define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
(define-key dired-mode-map [(B)] 'my-dired-up)
(define-key dired-mode-map [(control return)] 'dired-find-alternate-file)
(define-key dired-mode-map [(shift return)] 'dired-find-alternate-file)
(define-key dired-mode-map [return] 'my-dired-run)
(define-key java-mode-map [(control c) (control c)] 'compile)
(define-key minibuffer-local-completion-map [space] 'self-insert-command)
(define-key minibuffer-local-map [(control backspace)] 'my-minibuffer-backspace)
(define-key minibuffer-local-map [(control n)] 'next-history-element)
(define-key minibuffer-local-map [(control p)] 'previous-history-element)
(define-key minibuffer-local-map [(space)] 'self-insert-command)
(define-key picture-mode-map [(control c) down]  'picture-movement-down)
(define-key picture-mode-map [(control c) left]  'picture-movement-left)
(define-key picture-mode-map [(control c) right] 'picture-movement-right)
(define-key picture-mode-map [(control c) up]    'picture-movement-up)
(define-key ruby-mode-map [(meta backspace)] 'backward-kill-word)
(define-key tex-mode-map [?\"] 'self-insert-command)

(whenhost aquamacs
  (define-key osx-key-mode-map "\C-z" 'suspend-emacs))

(whenhost xemacs
  (define-key cssm-mode-map [?{] 'self-insert-command))

;;; Custom Lisp indentation

;; I'm not sure this is the right way to do it, but it seems to work.

(set-variable 'lisp-indent-function 'common-lisp-indent-function)

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
(set-lisp-indent 'whenhost 1)
(set-lisp-indent 'whilet 2)
(set-lisp-indent 'with-output-to-string 0)
(set-lisp-indent :section 1)

;;; Pathnames

(setq ecumenist-db (concat my-emacs-conf-dir "ecumenist"))

;;; Find the right `customize' data file and load it

(setq custom-file
      (concat my-emacs-conf-dir
              "custom-"
              (ehostcase
               (aquamacs "aquamacs")
               (gnuemacs "gnuemacs")
               (xemacs   "xemacs"))
              "-"
              (format "%d.%d" emacs-major-version emacs-minor-version)
              "-"
              (case system-type
                (windows-nt "win")
                (darwin     "mac")
                (t          "unix"))
              ".el"))

;; If you try to load your customizations by simply feeding your
;; newly-set `custom-file' to `load-file' in this initialization file,
;; some of the customizations won't have any effect because
;; who-knows-what will override them.  EmacsWiki folk wisdom says
;; `window-setup-hook' and `term-setup-hook' are the right places to
;; load them.  I don't understand the bizarre complications of the
;; Emacs/Aquamacs startup process so I'm going with folk wisdom.  It
;; seems to work for me.
(when (file-exists-p custom-file)
  (add-hook 'window-setup-hook (lambda () (load-file custom-file))))
