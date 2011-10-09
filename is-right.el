
(defun mk-is-right ()
  "inserts the last executed repl command in the buffer"
  (interactive)
  (insert (concat "(is-right:is-right " (first slime-repl-input-history) ")")))
