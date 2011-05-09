;;; anacode.el -- run perlcritic on Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl

(defconst anacode nil
  "The documentation for the anacode libraries.
Features:
  `anacode-perlcritic' - run perlcritic on your Perl code.
  `anacode-perldoc'    - run perldoc on your Perl code.")

(require 'anacode-perlcritic)
(require 'anacode-perldoc)

(provide 'anacode)

;;; anacode.el ends here
