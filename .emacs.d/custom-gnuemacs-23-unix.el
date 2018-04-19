(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-offsets-alist (quote ((arglist-cont . 4) (arglist-cont-nonempty . 4))))
 '(column-number-mode t)
 '(diff-switches "-u")
 '(editorconfig-custom-hooks (quote (editorconfig-domain-specific)))
 '(editorconfig-mode t)
 '(enable-recursive-minibuffers t)
 '(grep-command "grep -niR ")
 '(grep-use-null-device nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "lassi")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(iswitchb-mode t)
 '(iswitchb-prompt-newbuffer nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-file-name-completion-ignore-case-completion-ignore-case "nil" t)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((Lowercase . Yes) (Base . 10) (Package . XLIB) (Syntax . Common-lisp))))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(sort-fold-case t)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown"))))
 '(cursor ((t (:background "aquamarine" :foreground "black"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-indicator-added ((t (:inherit diff-added :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-comment-face ((nil (:foreground "cyan"))))
 '(renamed-face ((((class color)) (:foreground "orange"))))
 '(rpm-spec-tag-face ((((class color) (background dark)) (:foreground "cyan")))))
