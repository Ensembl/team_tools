;;; anacode-perlcritic.el -- run perlcritic on Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl perlcritic

(require 'anacode--core)
(require 'anacode-perl--core)

(defconst anacode-perlcritic nil
  "The documentation for the anacode\\=-perlcritic library.
Commands:
  \\<perl-mode-map>\\[anacode-perlcritic-run] - run perlcritic on your Perl code.
    mnemonic: (c)ritic
    details: `anacode-perlcritic-run'")

(defconst anacode-perlcritic-severity-list
  '(
    "brutal"
    "cruel"
    "harsh"
    "stern"
    "gentle"
    )
  "The list of perlcritic severity levels.")

(defvar anacode-perlcritic-severity-default 3
  "The default severity for `anacode-perlcritic'.
The initial value is 3, corresponding to --harsh.
The valid values are 1 to 5 inclusive.")

(defun anacode-perlcritic-arguments (severity file)
  "Return a list of arguments for perlcritic."
  (let*
      ((perlcriticrc (getenv "ANACODE_PERLCRITICRC"))
       (profile-arguments
        (if perlcriticrc
            (list "--profile" (getenv "ANACODE_PERLCRITICRC"))
          (list)))
       (severity-arguments
        (list "--severity" (number-to-string (abs severity))))
       (verbosity-arguments
        (list "--verbose"
              (if (< severity 0)
                  "%f:%l:%c: %m, near '%r'.\\n\\t\\t%p (Severity: %s)\\n\\n%d\\n"
                "%f:%l:%c:%m\\t[%s] %p\\n")))
       (filename (file-name-nondirectory file))
       (arguments
        `( "--nocolour"                 ; for recompile
           ,@profile-arguments
           ,@severity-arguments
           ,@verbosity-arguments
           ,filename)))
    arguments))

(defun anacode-perlcritic-run (raw-prefix)
  "Run perlcritic on your Perl source.
This runs perlcritic on the contents of the buffer and
displays the results in an alternative buffer.
Unsaved changes are saved first.
The prefix argument RAW-PREFIX specifies the severity of the criticism.
If RAW-PREFIX is nil the severity is the value of
`anacode-perlcritic-severity-default'.
When RAW-PREFIX is negative, severity is the absolute value and
verbosity goes up to 11."
  (interactive "P")
  (anacode-perl-require-major-mode-is-perl
   (let ((file (buffer-file-name)))
     (when file
       (basic-save-buffer)
       (let*
           ((severity
             (if raw-prefix
                 (prefix-numeric-value raw-prefix)
               anacode-perlcritic-severity-default))
            (severity-as-string
             (elt anacode-perlcritic-severity-list (- (abs severity) 1)))
            (arguments (anacode-perlcritic-arguments severity file))
            (message
             (format "Running perlcritic, severity = %d (%s)..."
                     (abs severity) severity-as-string)))
         (apply 'anacode-call-check
                "*perlcritic*" message
                "perlcritic" nil t nil arguments))))))

(defun anacode-perlcritic-perl-mode-hook ()
  "The anacode\\=-perlcritic hook function for Perl mode."
  (local-set-key [?\C-c ?c] 'anacode-perlcritic-run))

(anacode-perl-mode-add-hook 'anacode-perlcritic-perl-mode-hook)

(provide 'anacode-perlcritic)

;;; anacode-perlcritic.el ends here
