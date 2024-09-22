;; Personal Emacs subroutines

;; Lassi Kortela <lassi@lassi.io>

;; TODO: delete all blank lines (in region)
;; TODO: compact all blank lines so there are never two or more consecutive blank lines (in region)

(require 'cl-lib)

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-space-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[^ \t]+" "\\&" beg end))

;;;; Utility functions

(defun region-bounds (&optional need-bol-p need-eol-p)
  (unless (region-active-p)
    (error "The region is not active now"))
  (unless (eq (current-buffer) (marker-buffer (mark-marker)))
    (error "The mark is not set now"))
  (let ((start (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char start)
      (when (and need-bol-p (not (bolp)))
        (goto-char (min (point-max) (1+ (point-at-eol))))
        (setq start (point)))
      (goto-char end)
      (when (and need-eol-p (not (eolp)))
        (goto-char (point-at-bol))
        (setq end (point))))
    (if (< start end) (list start end) (list nil nil))))

;;; Buffer and window management commands

(defun scratch ()
  (interactive)
  (let ((default-directory (expand-file-name "~")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (funcall (or initial-major-mode 'lisp-interaction-mode))))

(defun foo ()
  (interactive)
  (let ((names '("foo" "bar" "baz" "qux" "quux" "quuux" "quuuux")))
    (let ((name (cl-dolist (name names (car (last names)))
                  (let ((buf (get-buffer name)))
                    (when (or (null buf) (= 0 (buffer-size buf)))
                      (cl-return name))))))
      (switch-to-buffer (get-buffer-create name)))))

(defun customize-face-at-point ()
  (interactive)
  (custom-buffer-create
   (mapcar (lambda (symbol) `(,symbol custom-face))
           ((lambda (x) (if (listp x) x (list x)))
            (or (get-char-property (point) 'face) '(default))))
   "*Customize Faces*"))

;; For Windows GNU Emacs. This is complicated but I finally got it to work.
(when (fboundp 'w32-shell-execute)
  (defun terminal-window-here ()
    (interactive)
    ;; NOTE: In the code below, default-directory doesn't need to be
    ;; shell-quoted because the Windows shell's cd command parses it
    ;; as a single argument.
    (w32-shell-execute
     nil
     (concat (getenv "SystemRoot") "\\system32\\cmd.exe")
     (concat "/k cd /d " default-directory))))

;;;; Word commands

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

;;;; Commands to manipulate region lines

(defun prefix-region (prefix start end)
  (interactive
   (cl-destructuring-bind (start end) (region-bounds t nil)
     (list (read-string "Prefix: ") start end)))
  (when (and start end)
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (< (point) (point-max))
        (insert prefix)
        (goto-char (min (point-max) (1+ (point-at-eol))))))))

(defun suffix-region (suffix start end)
  (interactive
   (cl-destructuring-bind (start end) (region-bounds nil t)
     (list (read-string "Suffix: ") start end)))
  (when (and start end)
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((done nil))
        (while (not done)
          (goto-char (point-at-eol))
          (insert suffix)
          (goto-char (min (point-max) (1+ (point))))
          (setq done (= (point) (point-max))))))))

(defun unprefix-region (prefix start end)
  (interactive
   (cl-destructuring-bind (start end) (region-bounds t nil)
     (list (read-string "Prefix: ") start end)))
  (when (and start end (not (equal "" prefix)))
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((done nil))
        (while (and (not done) (re-search-forward (concat "^" (regexp-quote prefix)) nil t))
          (replace-match "")
          (goto-char (min (point-max) (1+ (point))))
          (setq done (= (point) (point-max))))))))

(defun unsuffix-region (suffix start end)
  (interactive
   (cl-destructuring-bind (start end) (region-bounds nil t)
     (list (read-string "Suffix: ") start end)))
  (when (and start end (not (equal "" suffix)))
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((done nil))
        (while (and (not done) (re-search-forward (concat (regexp-quote suffix) "$") nil t))
          (replace-match "")
          (goto-char (min (point-max) (1+ (point))))
          (setq done (= (point) (point-max))))))))

(defun compact-blank-lines (start end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "^\\([ \t]*\n\\)+" nil t)
        (replace-match "\n")))))

;; Adapted from code in <http://emacswiki.org/emacs/DuplicateLines>
(defun uniq (start end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (let ((count 0) (case-fold-search nil))
        (while (re-search-forward "^\\(.*\n\\)\\1+" nil t)
          (setq count
                (+ count
                   (1- (truncate (- (match-end 0) (match-beginning 0))
                                 (- (match-end 1) (match-beginning 1))))))
          (replace-match "\\1"))
        (when (called-interactively-p 'any)
          (if (= count 0)
              (message "No superfluous lines found")
              (message "Deleted %d superfluous %s" count (if (= count 1) "line" "lines"))))
        count))))

;;;; Miscellaneous text editing commands

(defun insert-date-iso ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d ")))

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
                  (cl-destructuring-bind (start . end) title
                    (insert (make-string (- start last) ? ))
                    (insert (make-string (- end start) dash-char))
                    (setq last end))))))))

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

(defun append-yanked-lines-to-those-starting-at-point ()
  (interactive)
  (let ((act-buf (current-buffer))
        (tmp-buf nil))
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (yank)
      (goto-char (point-min))
      (while (and (not (with-current-buffer act-buf (eobp)))
                  (not (with-current-buffer tmp-buf (eobp))))
        (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
          (set-buffer act-buf)
          (goto-char (point-at-eol))
          (insert line)
          (forward-line 1)
          (set-buffer tmp-buf)
          (forward-line 1))))))

(defun prepend-yanked-lines-to-those-starting-at-point ()
  (interactive)
  (let ((act-buf (current-buffer))
        (tmp-buf nil))
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (yank)
      (goto-char (point-min))
      (while (and (not (with-current-buffer act-buf (eobp)))
                  (not (with-current-buffer tmp-buf (eobp))))
        (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
          (set-buffer act-buf)
          (goto-char (point-at-bol))
          (insert line)
          (forward-line 1)
          (set-buffer tmp-buf)
          (forward-line 1))))))

(defun collect-matches-in-region (start end regexp)
  (interactive "r\nsRegexp (obeys case-fold-search): ")
  (let ((outbuf (get-buffer-create "*collect*")))
    (goto-char start)
    (while (re-search-forward regexp end t)
      (let ((match (match-string 0)))
        (with-current-buffer outbuf
          (insert (format "%s\n" match)))))
    (switch-to-buffer outbuf)))

(defun url-decode-region (start end)
  (interactive "r")
  (let ((decoded (url-unhex-string (buffer-substring start end))))
    (goto-char start)
    (delete-region start end)
    (insert decoded)))

(defun whitespace-cleanup-buffer ()
  (interactive)
  "Improved version of the `whitespace-cleanup' function that
comes with Emacs in the source file whitespace.el."
  (let ((buffer-undo-list t))  ; Temporarily disable undo.
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (search-forward "\r\n" nil t) (replace-match "\n"))
          (goto-char (point-min))
          (while (search-forward "\r" nil t) (replace-match "\n"))
          (unless indent-tabs-mode (untabify (point-min) (point-max)))
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t) (replace-match ""))
          (goto-char (point-max))
          (delete-region (if (re-search-backward "[^ \t\n]" nil t) (match-end 0) (point-min))
                         (point-max))
          (when (< (point-min) (point-max)) (goto-char (point-max)) (insert "\n")))))))

(defun netify-region (beg end)
  (interactive "r")
  (let ((text (buffer-substring beg end)))
    (switch-to-buffer (generate-new-buffer "*netify*"))
    (insert text "\n")
    (goto-char (point-min))
    (while (re-search-forward "\t+" nil t) (replace-match " "))
    (goto-char (point-min))
    (while (not (= (point-at-eol) (point-max)))
      (goto-char (point-at-eol))
      (if (looking-at "\n\n+")
          (goto-char (match-end 0))
        (progn (delete-char 1)
               (insert " ")
               (goto-char (point-at-eol)))))
    (goto-char (point-min))
    (while (re-search-forward " +" nil t) (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward " +$" nil t) (replace-match ""))
    (goto-char (point-min))
    (when (looking-at " +") (replace-match ""))
    (goto-char (point-max))
    (re-search-backward "\n+")
    (delete-region (1- (point)) (point-max))
    (push-mark (point-min) t t)))

(defun sum-numbers-in-region (beg end)
  (interactive "r")
  (let ((nums (mapcar (lambda (num) (car (read-from-string num)))
                      (split-string
                       (replace-regexp-in-string
                        "[^0-9.]" " " (buffer-substring beg end))
                       " +" t))))
    (message "%S = %S" (cons '+ nums) (apply '+ nums))))

(defun nroff-update-date ()
  (interactive)
  (save-match-data
    (goto-char (point-min))
    (let* ((case-fold-search nil)
           (parts (decode-time (current-time)))
           (d (nth 3 parts))
           (m (nth 4 parts))
           (y (nth 5 parts))
           (month-names
            '("January" "February" "March" "April" "May" "June"
              "July" "August" "September" "October" "November" "December"))
           (month-name (nth (1- m) month-names))
           (new-timestamp (format ".Dd %s %d, %d" month-name d y)))
      (if (re-search-forward "^\\.Dd [A-Za-z0-9, ]*$" nil t)
          (replace-match new-timestamp t)
          (error ".Dd line not found")))))
