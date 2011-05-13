;;; anacode-perl-syntax.el -- syntax check Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl syntax

(require 'anacode--core)
(require 'anacode-perl--core)

(defconst anacode-perl-syntax nil
  "The documentation for the anacode\\=-perl-syntax library.
Commands:
  \\<perl-mode-map>\\[anacode-perl-syntax-run] - syntax check your Perl code.
    mnemonic: (s)yntax
    details: `anacode-perl-syntax-run'")

(defun anacode-perl-syntax-run ()
  "Syntax check your Perl source.
This runs perl in syntax check mode on the contents of the buffer.
If the check fails then it displays the output in an alternative
buffer.
Unsaved changes are saved before checking the syntax."
  (interactive)
  (anacode-perl-require-major-mode-is-perl
   (let ((file (buffer-file-name)))
     (when file
       (basic-save-buffer)
       (anacode-call-check
        "*Perl Syntax*" "Checking syntax..."
        "anacode_perl_syntax" nil t nil file)))))

(defun anacode-perl-syntax-perl-mode-hook ()
  "The anacode\\=-perl-syntax hook function for Perl mode."
  (local-set-key [?\C-c ?s] 'anacode-perl-syntax-run))

(anacode-perl-mode-add-hook 'anacode-perl-syntax-perl-mode-hook)

(provide 'anacode-perl-syntax)

;;; anacode-perl-syntax.el ends here
