
(provide 'anacode-perldoc)

(defun anacode-perldoc ()
  (interactive)
  (if (eq major-mode 'perl-mode)
      (let ((file (buffer-file-name)))
        (when file
          (basic-save-buffer)
          (with-temp-message "Running perldoc..."
            (let ((buffer-name "*perldoc*"))
              (with-output-to-temp-buffer buffer-name
                (with-current-buffer (get-buffer buffer-name)
                  (call-process "perldoc" nil t t "-t" file)))))))
    (message "The buffer %s does not appear to contain Perl code!" (buffer-name))))

(defun anacode-perldoc-perl-mode-hook ()
  (local-set-key [?\C-c ?d] 'anacode-perldoc))

(add-hook 'perl-mode-hook 'anacode-perldoc-perl-mode-hook)
