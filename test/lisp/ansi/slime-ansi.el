(defun copy-previous-ansi-failures ()
  "From the SLIME REPL buffer, copy the previous ANSI error report to kill ring."
  (interactive)
  (save-excursion 
    (unless 
        (search-backward "<--- Invocation of ")
      (error "Failed to find end of test invocation"))
    (previous-line 4)
    (let ((end (point)))
      (backward-sexp)
      (copy-region-as-kill (point) end))))
