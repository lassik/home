;;; Personal Emacs subroutines

;; Lassi Kortela <lassi@lassikortela.net>

;; Provide uniform versions of basic functions that are missing in
;; some emacsen or have different semantics in different emacsen.

;; XEmacs fix
(unless (fboundp 'use-region-p)
  (defalias 'use-region-p 'region-active-p))

;; The following three functions are copied from the XEmacs source
;; file simple.el. They're included here because they're not in GNU
;; Emacs. They seem trivial enough not to fall under copyright.

(defun capitalize-region-or-word (arg)
  "Capitalize the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
      (capitalize-word arg)))

(defun upcase-region-or-word (arg)
  "Upcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
      (upcase-word arg)))

(defun downcase-region-or-word (arg)
  "Downcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
      (downcase-word arg)))

;;; Extend `delete-blank-lines' to likewise work on the region if it's
;;; active.  That's what I want most of the time.

(defalias 'delete-blank-lines-std (symbol-function 'delete-blank-lines))

(defun delete-blank-lines ()
  (interactive "*")
  (if (not (use-region-p))
      (delete-blank-lines-std)
      (save-excursion
        (save-restriction
          (goto-char (region-beginning))
          (narrow-to-region (region-beginning) (region-end))
          (while (re-search-forward "\\(^[ \t]*\n\\)+" nil t)
            (replace-match ""))))))

;;; Date and time

(defun insert-date-iso ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d ")))

;;; Miscellaneous commands

(defun scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (funcall (or initial-major-mode 'lisp-interaction-mode))
  (font-lock-mode 1))                   ; XEmacs fix

(defun customize-face-at-point ()
  (interactive)
  (custom-buffer-create
   (mapcar (lambda (symbol) `(,symbol custom-face))
           ((lambda (x) (if (listp x) x (list x)))
            (or (get-char-property (point) 'face) '(default))))
   "*Customize Faces*"))

(defun dashify (dash-char)
  "Put a line of dashes under the titles on the current line, which should look like a heading or a table header."
  (interactive (list (if current-prefix-arg ?= ?-)))
  (goto-char (point-at-eol))
  (delete-horizontal-space)
  (goto-char (point-at-bol))
  (let (titles)
    (let ((start (point-at-bol)))
      (while (re-search-forward "[ \t]+" (point-at-eol) t)
	(unless (equal " " (match-string 0))
	  (when (< (point-at-bol) (match-beginning 0))
	    (setq titles (nconc titles (list (cons (- start (point-at-bol)) (- (match-beginning 0) (point-at-bol)))))))
	  (setq start (match-end 0))))
      (when (< start (point-at-eol))
	(setq titles (nconc titles (list (cons (- start (point-at-bol)) (- (point-at-eol) (point-at-bol))))))))
    (goto-char (point-at-eol))
    (insert (with-temp-buffer
	      (insert "\n")
	      (let ((last 0))
		(dolist (title titles (buffer-substring (point-min) (point-max)))
		  (destructuring-bind (start . end) title
		    (insert (make-string (- start last) ? ))
		    (insert (make-string (- end start) dash-char))
		    (setq last end))))))))

;; Adapted from code in <http://emacswiki.org/emacs/DuplicateLines>
(defun uniq (start end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((count 0))
        (while (re-search-forward "^\\(.*\n\\)\\1+" nil t)
          (incf count (1- (truncate (- (match-end 0) (match-beginning 0))
                                    (- (match-end 1) (match-beginning 1)))))
          (replace-match "\\1"))
        (when (interactive-p)
          (if (= count 0)
              (message "No superfluous lines found")
              (message "Deleted %d superfluous %s" count (if (= count 1) "line" "lines"))))
        count))))

(defun compact-blank-lines (start end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "^\\([ \t]*\n\\)+" nil t)
        (replace-match "\n")))))

(when (fboundp 'w32-shell-execute)
  (defun terminal-window-here ()
    (interactive)
    (w32-shell-execute nil "cmd.exe")))

;; For Windows XEmacs. This is complicated but I finally got it to work.
(when (fboundp 'mswindows-shell-execute)
  (defun terminal-window-here ()
    (interactive)
    ;; NOTE: In the code below, default-directory doesn't need to be
    ;; shell-quoted because the Windows shell's cd command parses it
    ;; as a single argument.
    (mswindows-shell-execute
     nil
     (concat (getenv "SystemRoot") "\\system32\\cmd.exe")
     (concat "/k cd /d " default-directory))))

(defun count-hours-region (start end)
  (interactive "r")
  (save-match-data
    (save-excursion
      (goto-char start)
      (let ((hours 0))
        (while (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\([0-9]+\\)h" end t)
          (incf hours (car (read-from-string (match-string 1)))))
        (when (interactive-p)
          (message "%d hours in region" hours))
        hours))))
