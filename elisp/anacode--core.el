;;; anacode--core.el -- core library

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords:

(defun anacode-call-check (buffer message command &rest args)
  "Run a checking command and display the results appropriately.
This runs a command and saves its output to an alternative buffer.
If the command succeeds then the alternative buffer is hidden
and its contents are shown in the message area.
If the command fails then the alternative buffer is displayed.
This is useful because many checking programs (eg. perlcritic)
will either succeed and generate a small amount of output (which
is best displayed in the message area) or fail and generate a
substantial amount of output (which is best displayed in an
alternative buffer)."
  (let ((old (get-buffer buffer)))
    (unless
        ;; old may have wrong local vars, pwd, mode, read-only status.
        ;; We could fix it, but this looks easier.
        (if old (kill-buffer old) t)
      (error "Cannot re-use buffer name %s" buffer)))
  (with-current-buffer (get-buffer-create buffer)
    (let ((status
           (with-temp-message message
             (apply 'call-process command args))))
      (cond
       ((eql status 0)
        (delete-windows-on (current-buffer))
        (message "%s" (anacode-message)))
       (t
        (compilation-minor-mode (current-buffer))
        (set (make-local-variable 'truncate-lines) t)
        (set (make-local-variable 'compile-command) ; for recompile
             (mapconcat 'shell-quote-argument       ; cannot quote newlines?
                        (apply 'list command (cdr (cddr args)))
                        " "))
        (set (make-local-variable 'compilation-buffer-name-function)
             'anacode-call-recheck-name)
        (display-buffer (current-buffer)))))))

(defun anacode-call-recheck-name (major-modestr)
  "Recompile buffer name chooser for `anacode-call-check'."
  (buffer-name (current-buffer)))

(defun anacode-message ()
  "Generate a message from the current buffer for the message command.
This returns the content of the current buffer with trailing
whitespace removed.  This is convenient when the content is a
one-liner because removing the trailing newline lets us display
it in the minibuffer without enlarging it."

  (let* ((string (buffer-string))
         (index (string-match "\\s-*\\'" string))
         (message (substring string 0 index)))
    message))

(provide 'anacode--core)

;;; anacode--core.el ends here
