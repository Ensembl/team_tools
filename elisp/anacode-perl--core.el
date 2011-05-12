;;; anacode-perl--core.el -- core Perl library

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl

(defconst anacode-perl-mode-list
  '(perl-mode cperl-mode)
  "The list of Perl major modes.")

(defun anacode-perl-major-mode-is-perl-p ()
  "Return whether the current major mode is a Perl mode."
  (memq major-mode anacode-perl-mode-list))

(defmacro anacode-perl-require-major-mode-is-perl (&rest body)
  "Require the current major mode to be a Perl mode.
Evaluate BODY if the current major mode is a Perl mode,
otherwise display a warning message."
  `(if (anacode-perl-major-mode-is-perl-p)
       ,@body
     (message
      "The buffer %s does not appear to contain Perl code!"
      (buffer-name))))

(provide 'anacode-perl--core)

;;; anacode-perl--core.el ends here
