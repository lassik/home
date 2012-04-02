(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote box))
 '(directory-free-space-args "-k")
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "cl-script-clisp")
 '(initial-major-mode (quote lisp-interaction-mode))
 '(iswitchb-mode t)
 '(iswitchb-prompt-newbuffer nil)
 '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
 '(require-final-newline t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "cyan"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "lightgreen"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "wheat"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "lightgreen"))))
 '(minibuffer-prompt ((t (:inherit minibuffer :foreground "aquamarine"))))
 '(renamed-face ((((class color)) (:background "blue" :foreground "white")))))
