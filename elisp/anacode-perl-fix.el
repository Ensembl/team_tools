;;; anacode-perl-fix.el -- fix Perl source

;; Author: Ana Code <anacode@sanger.ac.uk>
;; Keywords: perl

(require 'anacode--core)
(require 'anacode-perl--core)

(defconst anacode-perl-fix nil
  "The documentation for the anacode\\=-perl-fix library.
Commands:
  \\<perl-mode-map>\\[anacode-perl-fix-run] - fix your Perl code.
    mnemonic: (f)ix
    details: `anacode-perl-fix-run'")

(defconst anacode-perl-fix-capture-whitespace-pattern
  "\\(\\s-*\\)"
  "A pattern that matches and captures whitespace."
  )

(defconst anacode-perl-fix-sub-args-pattern
  (concat
   "\\<my\\>"
   anacode-perl-fix-capture-whitespace-pattern
   "("
   anacode-perl-fix-capture-whitespace-pattern
   "\\([^()]*?\\)"
   anacode-perl-fix-capture-whitespace-pattern
   ")"
   anacode-perl-fix-capture-whitespace-pattern
   "="
   anacode-perl-fix-capture-whitespace-pattern
   "@_"
   anacode-perl-fix-capture-whitespace-pattern
   ";"
   )
  "A pattern that matches the declaration of a subroutine's arguments.")

(defconst anacode-perl-fix-comma-pattern
  (concat
   anacode-perl-fix-capture-whitespace-pattern
   ","
   anacode-perl-fix-capture-whitespace-pattern
   )
  "A pattern that matches a commma and its surrounding whitespace.")

(defvar anacode-perl-fix-count)

(defmacro anacode-perl-fix-count-report (&rest body)
  `(let ((anacode-perl-fix-count 0))
     ,@body
     (message (format "fixes: %d" anacode-perl-fix-count))))

(defun anacode-perl-fix-count-increment ()
  (setq anacode-perl-fix-count
        (+ anacode-perl-fix-count 1)))

(defvar anacode-perl-fix-match-index)

(defun anacode-perl-fix-match-string ()
  (match-string anacode-perl-fix-match-index))

(defmacro anacode-perl-fix-match-start (&rest body)
  `(let ((anacode-perl-fix-match-index 1))
     ,@body))

(defun anacode-perl-fix-match-next ()
  (setq anacode-perl-fix-match-index
        (+ anacode-perl-fix-match-index 1)))

(defmacro anacode-perl-fix-search (pattern start end &rest body)
  (let ((pattern-var (make-symbol "pattern"))
        (start-var   (make-symbol "start"  ))
        (end-var     (make-symbol "end"    )))
    `(let ((,pattern-var ,pattern)
           (,start-var   ,start  )
           (,end-var     ,end    ))
       (save-excursion
         (goto-char ,start)
         (save-match-data
           (while (re-search-forward ,pattern ,end t)
             (anacode-perl-fix-match-start
              ,@body)))))))

(defmacro anacode-perl-fix-search-match (pattern &rest body)
  (let ((pattern-var (make-symbol "pattern"))
        (index-var   (make-symbol "index"  ))
        (start-var   (make-symbol "start"  ))
        (end-var     (make-symbol "end"    )))
    `(let* ((,pattern-var ,pattern)
            (,index-var   anacode-perl-fix-match-index)
            (,start-var   (match-beginning ,index-var))
            (,end-var     (match-end ,index-var)))
       (anacode-perl-fix-search
        ,pattern-var ,start-var ,end-var
        ,@body)
       (anacode-perl-fix-match-next))))

(defmacro anacode-perl-fix-excursion-to-match (&rest body)
  `(save-excursion
     (push-mark (match-beginning anacode-perl-fix-match-index))
     (goto-char (match-end       anacode-perl-fix-match-index))
     ,@body))

(defun anacode-perl-fix-query-match (string query)
  (let ((match-string (anacode-perl-fix-match-string)))
    (if (not (string-equal match-string string))
        (anacode-perl-fix-excursion-to-match
         (when (y-or-n-p (format "%s? " query))
           (replace-match
            string t t nil
            anacode-perl-fix-match-index)
           (anacode-perl-fix-count-increment)))))
  (anacode-perl-fix-match-next))

(defun anacode-perl-fix-no-space ()
  (anacode-perl-fix-query-match
   "" "remove whitespace"))

(defun anacode-perl-fix-single-space ()
  (let* ((match-string (anacode-perl-fix-match-string))
         (query
          (if (string-equal match-string "")
              "insert whitespace"
            "reduce whitespace")))
    (anacode-perl-fix-query-match " " query)))

(defun anacode-perl-fix-run (start end)
  "Fix your Perl source.

This tidies up the Perl code in the region.

It tidies up the whitespace in the declarations of subroutine
arguments."
  (interactive "r")
  (anacode-perl-require-major-mode-is-perl
   (anacode-perl-fix-count-report
    (anacode-perl-fix-search
     anacode-perl-fix-sub-args-pattern
     start end
     (anacode-perl-fix-single-space)  ; "my ("
     (anacode-perl-fix-no-space)      ; "($foo"
     (anacode-perl-fix-search-match
      anacode-perl-fix-comma-pattern
      (anacode-perl-fix-no-space)     ; "$foo,"
      (anacode-perl-fix-single-space) ; ", $bar"
      )
     (anacode-perl-fix-no-space)      ; "$bar)"
     (anacode-perl-fix-single-space)  ; ") ="
     (anacode-perl-fix-single-space)  ; "= @_"
     (anacode-perl-fix-no-space)      ; "@_;"
     ))))

(defun anacode-perl-fix-perl-mode-hook ()
  "The anacode\\=-perl-fix hook function for Perl mode."
  (local-set-key [?\C-c ?f] 'anacode-perl-fix-run))

(anacode-perl-mode-add-hook 'anacode-perl-fix-perl-mode-hook)

(provide 'anacode-perl-fix)

;;; anacode-perl-fix.el ends here
