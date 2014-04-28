(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "ack")
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style "bsd")
 '(clojure-defun-style-default-indent t)
 '(column-number-mode t)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote box) t)
 '(directory-free-space-args "-k")
 '(dired-use-ls-dired (quote unspecified))
 '(enable-recursive-minibuffers t)
 '(grep-command "grep -niR ")
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "cl-script-clisp")
 '(initial-major-mode (quote lisp-interaction-mode))
 '(iswitchb-mode t)
 '(iswitchb-prompt-newbuffer nil)
 '(make-backup-files nil)
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(package-archives (quote (("melpa" . "http://melpa.milkbox.net/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(require-final-newline t)
 '(sort-fold-case t t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(cursor ((t (:background "red"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "cyan" :weight bold))))
 '(diff-function ((t (:foreground "orange"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "cyan"))))
 '(diff-hunk-header ((t (:foreground "orange"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "cyan"))))
 '(echo-area ((((type ns)) (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal :family "Monaco"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "lightgreen"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "wheat"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "lightgreen"))))
 '(lassi-todo-face-hash-tag ((t (:foreground "yellow"))))
 '(lassi-todo-face-minus ((t (:foreground "red3"))))
 '(lassi-todo-face-plus ((t (:foreground "chartreuse"))))
 '(minibuffer-prompt ((t (:inherit minibuffer :foreground "aquamarine"))))
 '(renamed-face ((((class color)) (:background "blue" :foreground "white"))))
 '(rst-level-1-face ((t (:foreground "cyan"))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "cyan"))) t))
