;; The following three functions are copied straight from the XEmacs
;; source file "simple.el".

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
