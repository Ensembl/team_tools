;;; anacode-perldoc.el -- run perldoc on Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl perldoc

(require 'anacode-perl--core)

(defconst anacode-perldoc nil
  "The documentation for the anacode\\=-perldoc library.
Commands:
  \\<perl-mode-map>\\[anacode-perldoc-run] - run perldoc on your Perl code.
    mnemonic: (d)oc
    details: `anacode-perldoc-run'")

(defun anacode-perldoc-run ()
  "Run perldoc on your Perl source.
This runs perldoc on the contents of the buffer and
displays the results in an alternative buffer.
Unsaved changes are saved first.
Perldoc is run with the text formatter so the results
are not identical to running perldoc on the terminal."
  (interactive)
  (anacode-perl-require-major-mode-is-perl
   (let ((file (buffer-file-name)))
     (when file
       (basic-save-buffer)
       (with-temp-message "Running perldoc..."
         (let ((buffer-name "*perldoc*"))
           (with-output-to-temp-buffer buffer-name
             (with-current-buffer (get-buffer buffer-name)
               (call-process "perldoc" nil t t "-t" file)))))))))

(defun anacode-perldoc-perl-mode-hook ()
  "The anacode\\=-perldoc hook function for Perl mode."
  (local-set-key [?\C-c ?d] 'anacode-perldoc-run))

(anacode-perl-mode-add-hook 'anacode-perldoc-perl-mode-hook)

(provide 'anacode-perldoc)

;;; anacode-perldoc.el ends here
