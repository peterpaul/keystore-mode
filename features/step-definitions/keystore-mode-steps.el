;;; -*- lexical-binding: t; -*-
;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(defun keystore-steps-generate-keypairs-from-table (keystore-file keystore-password keys)
  "Execute 'keytool -genkeypair' on KEYSTORE-FILE with KEYSTORE-PASSWORD for KEYS.

KEYS is a table with two columns: 'alias' and 'subject'."
  (let* ((table keys)
         (header (car table))
         (rows (cdr table)))
    (dolist (row rows)
      (let ((alias (car row))
            (subject (cadr row)))
        (keystore-genkeypair keystore-file keystore-password "RSA" "1024" 365 alias subject)))))

(Given "^file \"\\(.+\\)\" does not exist$"
       (lambda (file)
         (when (file-exists-p file)
           (delete-file file))))

(Given "^buffer \"\\(.+\\)\" does not exist$"
       (lambda (buffer)
         (let ((buf (get-buffer buffer)))
           (when buf (kill-buffer buf)))))

(Given "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\" and these keys:$"
       (lambda (keystore-file keystore-password keys)
         (Given (format "file \"%s\" does not exist" keystore-file))
         (keystore-steps-generate-keypairs-from-table keystore-file keystore-password keys)
         (keystore-visit keystore-file keystore-password)))

(Given "^keystore \"\\(.+\\)\" with password \"\\(.+\\)\" and these keys:$"
       (lambda (keystore-file keystore-password keys)
         (Given (format "file \"%s\" does not exist" keystore-file))
         (keystore-steps-generate-keypairs-from-table keystore-file keystore-password keys)))

(Given "^buffer \"\\(.+\\)\" with contents\\(?: \"\\(.+\\)\"\\|:\\)$"
       (lambda (buffer contents)
         (let ((buf (get-buffer-create buffer)))
           (with-current-buffer buf
             (erase-buffer)
             (insert contents)))))

(Given "^file \"\\([^\"]+\\)\" with contents\\(?: \"\\(.+\\)\"\\|:\\)$"
       (lambda (file contents)
         (let ((buf (get-buffer-create "*temp*")))
           (with-current-buffer buf
             (erase-buffer)
             (insert contents)
             (write-file file)
             (kill-buffer buf)))))

(Given "^I am in buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (let ((buf (get-buffer buffer)))
           (switch-to-buffer buf))))

(When "^I answer yes$"
      (lambda ()
        (setq keystore-mode-yes-or-no t)))

(When "^I answer no$"
      (lambda ()
        (setq keystore-mode-yes-or-no nil)))

(When "^I create a keypair with alias \"\\(.+\\)\" and subject \"\\(.+\\)\"$"
      (lambda (alias subject)
        (keystore-genkeypair-list "RSA" "1024" 365 alias subject)))

(When "^I create a keypair with alias \"\\(.+\\)\" and subject \"\\(.+\\)\" in keystore \"\\(.+\\)\" with password \"\\(.+\\)\"$"
      (lambda (alias subject keystore-file keystore-password)
        (keystore-genkeypair keystore-file keystore-password "RSA" "1024" 365 alias subject)))

(When "^I open keystore \"\\(.+\\)\" with password \"\\(.+\\)\"$"
      (lambda (keystore-file keystore-password)
        (keystore-visit keystore-file keystore-password)))

(When "^I switch to buffer \"\\(.+\\)\"$"
      (lambda (buffer)
        (Given (format "I am in buffer \"%s\"" buffer))))

(When "^I import certificate \"\\(.+\\)\" from buffer \"\\(.+\\)\"$"
      (lambda (alias buffer)
        (keystore-importcert-buffer buffer alias)))

(When "^I import certificate \"\\(.+\\)\" from file \"\\(.+\\)\"$"
      (lambda (alias file)
        (keystore-importcert-file file alias)))

(When "^I import keystore \"\\([^\"]+\\)\" with password \"\\([^\"]+\\)\"$"
      (lambda (srckeystore srcstorepass)
        (keystore-importkeystore srckeystore srcstorepass)))

(When "^I execute the action chain, and capture errors$"
      "Executes the action chain."
      (lambda ()
        (setq error-message-from-action-chain nil)
        (condition-case err
            (When "I execute the action chain")
          (error
           (setq error-message-from-action-chain (error-message-string err))
           ;; disable action chain, since errors bypass the normal cleanup
           (setq espuds-chain-active nil)
           nil))))

(Then "^I should see error message \"\\(.+\\)\"$"
      (lambda (expected-error-message)
        (let ((message-no-error "Expected error message '%s', but no error occurred.")
              (message-mismatch "Expected error message '%s', but got '%s'."))
          (cl-assert error-message-from-action-chain nil
                     message-no-error expected-error-message)
          (cl-assert (s-equals? error-message-from-action-chain expected-error-message) nil
                     message-mismatch expected-error-message error-message-from-action-chain))))

(Then "^buffer \"\\(.+\\)\" should exist$"
      (lambda (buffer)
        (let ((message "Expected buffer '%s' to exist, but all I got was %s"))
          (cl-assert (get-buffer buffer) nil message buffer
                     (mapconcat (lambda (x) (format "'%s'" x))
                                (buffer-list)
                                ", ")))))

(Then "^file \"\\(.+\\)\" should exist$"
      (lambda (file)
        (let ((message "Expected file '%s' to exist, but it did not"))
          (cl-assert (file-exists-p file) nil message buffer))))

(Then "^buffer \"\\(.+\\)\" should contain\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (buffer expected)
        (Then (format "buffer \"%s\" should exist" buffer))
        (let ((actual (save-excursion
                        (switch-to-buffer (get-buffer buffer))
                        (buffer-string)))
              (message "Expected\n%s\nto be part of:\n%s"))
          (cl-assert (s-contains? expected actual) nil message expected actual))))

(Then "^buffer \"\\(.+\\)\" should contain pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (buffer expected)
        (Then (format "buffer \"%s\" should exist" buffer))
        (let ((actual (save-excursion
                        (switch-to-buffer (get-buffer buffer))
                        (buffer-string)))
              (message "Expected to see pattern '%s' in '%s', but did not."))
          (cl-assert
           (s-matches? expected actual) nil message expected actual))))
