;;; anacode-perlcritic.el -- run perlcritic on Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl perlcritic

(require 'anacode-perl--core)

(defconst anacode-perlcritic nil
  "The documentation for the anacode\\=-perlcritic library.
Commands:
  \\<perl-mode-map>\\[anacode-perlcritic-run] - run perlcritic on your Perl code.
    mnemonic: (c)ritic
    details: `anacode-perlcritic-run'")

(defvar anacode-perlcritic-severity-default 3
  "The default severity for `anacode-perlcritic'.
The initial value is 3, corresponding to --harsh.
The valid values are 1 to 5 inclusive.")

(defvar anacode-perlcritic-exclusions
  '(
    "RegularExpressions::RequireExtendedFormatting"
    "ErrorHandling::RequireCarping"
    )
  "The list of policies to exclude.")

(defun anacode-perlcritic-arguments (severity)
  "Return a list of arguments for perlcritic."
  `("--severity" ,(number-to-string severity)
    ,@(apply 'append
             (mapcar (lambda (exclude) `("--exclude" ,exclude))
                     anacode-perlcritic-exclusions))))

(defun anacode-perlcritic-run (raw-prefix)
  "Run perlcritic on your Perl source.
This runs perlcritic on the contents of the buffer and
displays the results in an alternative buffer.
Unsaved changes are saved first.
The prefix argument RAW-PREFIX specifies the severity of the criticism.
If RAW-PREFIX is nil the severity is the value of
`anacode-perlcritic-severity-default'."
  (interactive "P")
  (anacode-perl-require-major-mode-is-perl
   (let ((file (buffer-file-name)))
     (when file
       (basic-save-buffer)
       (with-current-buffer (get-buffer-create "*perlcritic*")
         (erase-buffer)
         (let* ((severity
                 (if raw-prefix
                     (prefix-numeric-value raw-prefix)
                   anacode-perlcritic-severity-default))
                (arguments (anacode-perlcritic-arguments severity))
                (status
                 (with-temp-message "Running perlcritic..."
                   (apply 'call-process "perlcritic" file t nil arguments))))
           (cond
            ((eql status 0)
             (delete-windows-on (current-buffer))
             (message "%s" (anacode-perlcritic-message)))
            (t
             (display-buffer (current-buffer))))))))))

(defun anacode-perlcritic-message ()
  "Generate a syntax message for the message command.
This returns the content of the current buffer with trailing
whitespace removed.  This is convenient when the content is a
one-liner because removing the trailing newline lets us display
it in the minibuffer without enlarging it."

  (let* ((string (buffer-string))
         (index (string-match "\\s-*\\'" string))
         (message (substring string 0 index)))
    message))

(defun anacode-perlcritic-perl-mode-hook ()
  "The anacode\\=-perlcritic hook function for Perl mode."
  (local-set-key [?\C-c ?c] 'anacode-perlcritic-run))

(anacode-perl-mode-add-hook 'anacode-perlcritic-perl-mode-hook)

(provide 'anacode-perlcritic)

;;; anacode-perlcritic.el ends here
