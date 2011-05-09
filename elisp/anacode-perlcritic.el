;;; anacode-perlcritic.el -- run perlcritic on Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl perlcritic

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

(defun anacode-perlcritic-run (raw-prefix)
  "Run perlcritic on your Perl source.
This runs perlcritic on the contents of the buffer and
displays the results in an alternative buffer.
Unsaved changes are saved first.
The prefix argument RAW-PREFIX specifies the severity of the criticism.
If RAW-PREFIX is nil the severity is the value of
`anacode-perlcritic-severity-default'."
  (interactive "P")
  (let* ((severity
          (if raw-prefix
              (prefix-numeric-value raw-prefix)
            anacode-perlcritic-severity-default))
         (severity-arg (number-to-string severity)))
    (if (eq major-mode 'perl-mode)
        (let ((file (buffer-file-name)))
          (when file
            (basic-save-buffer)
            (with-temp-message "Running perlcritic..."
              (let ((buffer-name "*perlcritic*"))
                (with-output-to-temp-buffer buffer-name
                  (with-current-buffer (get-buffer buffer-name)
                    (call-process
                     "anacode_perlcritic"
                     nil t t file severity-arg)))))))
      (message
       "The buffer %s does not appear to contain Perl code!"
       (buffer-name)))))

(defun anacode-perlcritic-perl-mode-hook ()
  "The anacode\\=-perlcritic hook function for Perl mode."
  (local-set-key [?\C-c ?c] 'anacode-perlcritic-run))

(add-hook 'perl-mode-hook 'anacode-perlcritic-perl-mode-hook)

(provide 'anacode-perlcritic)

;;; anacode-perlcritic.el ends here
