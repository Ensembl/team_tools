;;; anacode-perl-attribute.el -- insert an attribute into OO Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl

(require 'anacode--core)
(require 'anacode-perl--core)

(defconst anacode-perl-attribute nil
  "The documentation for the anacode\\=-perl-attribute library.
Commands:
  \\<perl-mode-map>\\[anacode-perl-attribute-run] - insert an attribute method.
    mnemonic: (a)ttribute
    details: `anacode-perl-attribute-run'")

(defconst anacode-perl-attribute-method-pattern
  "\\`[[:alnum:]_]+\\'"
  "The syntax of a method name.")

(defconst anacode-perl-attribute-read-write-method-format
  "
sub %s {
    my ($self, @args) = @_;
    ($self->{'%s'}) = @args if @args;
    my $%s = $self->{'%s'};
    return $%s;
}
"
  "The format of a read-write method")

(defun anacode-perl-attribute-read-write-method (method)
  (format anacode-perl-attribute-read-write-method-format
          method method method method method))

(defconst anacode-perl-attribute-read-only-method-format
  "
sub %s {
    my ($self) = @_;
    my $%s = $self->{'%s'};
    return $%s;
}
"
  "The format of a read-only method")

(defun anacode-perl-attribute-read-only-method (method)
  (format anacode-perl-attribute-read-only-method-format
          method method method method))

(defun anacode-perl-attribute-run (prefix method)
  "Insert an attribute method.

This inserts an attribute method at point.

The default is to insert a read-write attribute.
Supply a prefix argument to get a read-only attribute."
  (interactive "P\nBmethod name: ")
  (if (string-match anacode-perl-attribute-method-pattern method)
      (if (looking-at "^\$")
          (insert
           (cond
            (prefix (anacode-perl-attribute-read-only-method  method))
            (t      (anacode-perl-attribute-read-write-method method))))
        (message "Not at a blank line!"))
    (message "%s" "Invalid method name!")))

(defun anacode-perl-attribute-perl-mode-hook ()
  "The anacode\\=-perl-attribute hook function for Perl mode."
  (local-set-key [?\C-c ?a] 'anacode-perl-attribute-run))

(anacode-perl-mode-add-hook 'anacode-perl-attribute-perl-mode-hook)

(provide 'anacode-perl-attribute)

;;; anacode-perl-attribute.el ends here
