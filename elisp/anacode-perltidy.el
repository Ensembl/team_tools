;;; anacode-perltidy.el -- run perltidy on the region

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl perltidy

(require 'anacode-perl--core)

(defconst anacode-perltidy nil
  "The documentation for the anacode\\=-perltidy library.
Commands:
  \\<perl-mode-map>\\[anacode-perltidy-run] - run perltidy on the region.
    mnemonic: (t)idy
    details: `anacode-perltidy-run'")

(defun anacode-perltidy-run ()
  "Run perltidy on the region."
  (interactive)
  (anacode-perl-require-major-mode-is-perl
   (with-temp-message "Running perltidy..."
     (shell-command-on-region
      (region-beginning) (region-end)
      "perltidy" nil t "*PerlTidy*" t))))

(defun anacode-perltidy-perl-mode-hook ()
  "The anacode\\=-perltidy hook function for Perl mode."
  (local-set-key [?\C-c ?t] 'anacode-perltidy-run))

(anacode-perl-mode-add-hook 'anacode-perltidy-perl-mode-hook)

(provide 'anacode-perltidy)

;;; anacode-perltidy.el ends here
